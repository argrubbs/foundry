;;; voice-test.el --- Tests for voice notes and dictation -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/voice-test.el
;; No audio, no network and no whisper.cpp: recording and gptel-request are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'gptel)
(require 'gptel-ollama)
(require 'gptel-org)
(require 'org-voice)

;; Special here too: whisper.el is not loaded, and these tests bind them.
(defvar whisper-before-transcription-hook)
(defvar whisper-install-directory)
(defvar whisper-install-whispercpp)
(defvar whisper-model)
(defvar whisper-recording-timeout)
(defvar whisper-return-cursor)
(defvar whisper--marker)

(defmacro voice-test--with-notes (&rest body)
  "Run BODY with an empty voice notes file and no recording in progress."
  `(let* ((ide-voice-file (make-temp-file "voice-notes" nil ".org"))
          (ide-voice-model nil)
          (ide-voice--pending nil)
          (org-startup-with-latex-preview nil))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-file-buffer ide-voice-file)))
         (with-current-buffer b (set-buffer-modified-p nil))
         (kill-buffer b))
       (delete-file ide-voice-file))))

(defmacro voice-test--capturing-request (&rest body)
  "Run BODY with `gptel-request' stubbed; bind `requests' to the calls seen, oldest first.
Each call is a plist of its keywords plus :prompt, :model, :on-disk, :context and :tools."
  `(let ((requests nil))
     (cl-letf (((symbol-function 'gptel-request)
                (lambda (prompt &rest keys)
                  (setq requests
                        (append requests
                                (list (append (list :prompt prompt :model gptel-model
                                                    :context gptel-use-context :tools gptel-use-tools
                                                    :on-disk (with-temp-buffer
                                                               (insert-file-contents ide-voice-file)
                                                               (buffer-string)))
                                              keys)))))))
       ,@body)))

(defun voice-test--file ()
  (with-temp-buffer (insert-file-contents ide-voice-file) (buffer-string)))

(defun voice-test--route (text pending)
  "Run the routing hook on transcription TEXT; return what whisper.el would insert."
  (setq ide-voice--pending pending)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (ide-voice--route)
    (buffer-string)))

(defun voice-test--reply (request response &optional info)
  "Answer REQUEST, as captured by `voice-test--capturing-request', with RESPONSE."
  (funcall (plist-get request :callback) response info))

;;;; Routing

(ert-deftest voice-note-does-not-insert-at-point ()
  "A voice note empties whisper.el's output, and that is what stops insertion."
  (voice-test--with-notes
   (voice-test--capturing-request
    (should (equal (voice-test--route "hello there" '(:style summary)) ""))
    (should (string-match-p "hello there" (voice-test--file)))
    (should (null ide-voice--pending)))))

(ert-deftest voice-dictation-is-left-for-whisper-to-insert ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (let ((whisper--marker nil))
      (should (equal (voice-test--route "hello there" nil) "hello there")))
    (should (null requests))
    (should (equal (voice-test--file) "")))))

(ert-deftest voice-dictation-follows-text-with-a-space ()
  (voice-test--with-notes
   (let ((whisper--marker nil))
     (with-temp-buffer
       (insert "first sentence.")
       (setq whisper--marker (point-marker))
       (should (equal (voice-test--route "second" nil) " second"))))
   (let ((whisper--marker nil))
     (with-temp-buffer
       (insert "first line\n")
       (setq whisper--marker (point-marker))
       (should (equal (voice-test--route "second" nil) "second"))))))

(ert-deftest voice-silence-makes-nothing ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (let ((whisper--marker nil))
      (dolist (blank '("[BLANK_AUDIO]" " (silence) " "[ Silence ] [BLANK_AUDIO]"))
        (should (equal (voice-test--route blank '(:style summary)) ""))
        (should (equal (voice-test--route blank nil) ""))))
    (should (null requests))
    (should (equal (voice-test--file) "")))))

;;;; Entries

(ert-deftest voice-transcript-is-saved-before-the-model-is-asked ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my rambling thought" '(:style summary))
    (should (= (length requests) 1))
    (should (string-match-p "\\*\\* Transcript" (plist-get (car requests) :on-disk)))
    (should (string-match-p "my rambling thought" (plist-get (car requests) :on-disk)))
    (should (equal (plist-get (car requests) :prompt) "my rambling thought")))))

(ert-deftest voice-summary-fills-in-title-and-section ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my rambling thought" '(:style summary))
    (voice-test--reply (car requests) "TITLE: A clear thought\n\n- The main point")
    (let ((file (voice-test--file)))
      (should (string-match-p "^\\* A clear thought +:voice:$" file))
      (should (string-match-p "^\\*\\* Summary\n- The main point" file))
      (should-not (string-match-p "Working on it" file))
      (should (string-match-p "my rambling thought" file))
      ;; The summary comes first, so the folded transcript stays out of the way.
      (should (< (string-match "\\*\\* Summary" file) (string-match "\\*\\* Transcript" file)))))))

(ert-deftest voice-failed-request-keeps-transcript-and-says-so ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my rambling thought" '(:style summary))
    (voice-test--reply (car requests) nil '(:status "Could not connect"))
    (let ((file (voice-test--file)))
      (should (string-match-p "Failed: Could not connect" file))
      (should (string-match-p "ide-voice-reprocess" file))
      (should (string-match-p "my rambling thought" file))))))

(ert-deftest voice-refused-request-keeps-transcript ()
  "An error while sending, like the private-data guard's, must not lose the note."
  (voice-test--with-notes
   (cl-letf (((symbol-function 'gptel-request)
              (lambda (&rest _) (user-error "Not sending it to a hosted model"))))
     (voice-test--route "my rambling thought" '(:style summary)))
   (let ((file (voice-test--file)))
     (should (string-match-p "Failed: Not sending" file))
     (should (string-match-p "my rambling thought" file)))))

(ert-deftest voice-entries-are-newest-first-and-replies-find-their-own ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (cl-letf (((symbol-function 'format-time-string)
               (let ((n 0) (real (symbol-function 'format-time-string)))
                 (lambda (fmt &rest args)
                   (if (equal fmt "%Y%m%dT%H%M%S")
                       (format "ID%d" (cl-incf n))
                     (apply real fmt args))))))
      (voice-test--route "older words" '(:style summary))
      (voice-test--route "newer words" '(:style summary)))
    ;; Answered out of order, and after the file has gained a newer entry.
    (voice-test--reply (car requests) "TITLE: Older\n\n- older summary")
    (voice-test--reply (cadr requests) "TITLE: Newer\n\n- newer summary")
    (let ((file (voice-test--file)))
      (should (< (string-match "^\\* Newer" file) (string-match "^\\* Older" file)))
      (should (string-match-p "^\\* Older[^\0]*older summary[^\0]*older words" file))
      (should (string-match-p "^\\* Newer[^\0]*newer summary[^\0]*newer words" file))))))

(ert-deftest voice-transcript-cannot-break-the-outline ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "one\n* not a heading\ntwo" '(:style summary))
    (should (string-match-p "^,\\* not a heading$" (voice-test--file)))
    (with-current-buffer (ide-voice--buffer)
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (should (equal (ide-voice--entry-transcript) "one\n* not a heading\ntwo"))))))

(ert-deftest voice-response-parsing ()
  (should (equal (ide-voice--parse-response "<think>hmm\nhmm</think>\nTITLE: Thing\n\n- point")
                 '("Thing" . "- point")))
  (should (equal (ide-voice--parse-response "**TITLE: Bold thing**\n- point")
                 '("Bold thing" . "- point")))
  (should (equal (ide-voice--parse-response "- no title here") '(nil . "- no title here")))
  ;; A heading from the model would otherwise become a sibling of the entry.
  (should-not (string-match-p "^\\*" (cdr (ide-voice--parse-response "TITLE: T\n# Heading\ntext")))))

;;;; The request

(ert-deftest voice-request-uses-its-own-model-without-chat-state ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (let ((ide-voice-model '(ollama . voice-test-model))
          (ide-ai--backends nil)
          (gptel-model 'chat-default-model)
          (gptel-use-context 'system)
          (gptel-use-tools t))
      (voice-test--route "words" '(:style summary)))
    (let ((request (car requests)))
      (should (eq (plist-get request :model) 'voice-test-model))
      (should-not (plist-get request :context))
      (should-not (plist-get request :tools))
      (should (string-match-p "TITLE:" (plist-get request :system))))
    (should (string-match-p ":MODEL: +voice-test-model" (voice-test--file))))))

(ert-deftest voice-unknown-provider-is-reported-in-the-entry ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (let ((ide-voice-model '(no-such-provider . m)))
      (voice-test--route "words" '(:style summary)))
    (should (null requests))
    (should (string-match-p "Failed: Unknown AI provider" (voice-test--file)))
    (should (string-match-p "words" (voice-test--file))))))

(ert-deftest voice-explain-names-its-audience ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "the pipeline" '(:style explain :audience "a manager"))
    (should (string-match-p "so that a manager can" (plist-get (car requests) :system)))
    (voice-test--reply (car requests) "TITLE: Pipeline\n\n- plain words")
    (should (string-match-p "^\\*\\* Explain for a manager\n- plain words" (voice-test--file)))
    (should (string-match-p ":AUDIENCE: +a manager" (voice-test--file))))))

;; Seen live: Ollama with reasoning on ended 6 runs of 8 with reason "length" and no answer.

(ert-deftest voice-local-model-does-not-reason-and-chat-is-untouched ()
  (voice-test--with-notes
   (let* ((ide-voice-model '(ollama . voice-test-model))
          (ide-ai--backends nil)
          (ide-voice-ollama-context 1234)
          (choice (ide-voice--backend))
          (shared (ide-ai--backend (ide-ai--provider 'ollama) '(voice-test-model)))
          (params (gptel-backend-request-params (car choice))))
     (should (eq (cdr choice) 'voice-test-model))
     (should (eq (plist-get params :think) :json-false))
     (should (= (plist-get (plist-get params :options) :num_ctx) 1234))
     ;; A copy that the private-data guard still recognizes as the same provider.
     (should (eq (car (ide-ai--backend-provider (car choice))) 'ollama))
     (should-not (eq (car choice) shared))
     (should-not (gptel-backend-request-params shared)))))

(ert-deftest voice-answer-that-was-cut-off-says-so ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my words" '(:style summary))
    (voice-test--reply (car requests) "TITLE: Part\n\n- We" '(:stop-reason "length"))
    (let ((file (voice-test--file)))
      (should (string-match-p "^- We$" file))
      (should (string-match-p "Cut off" file))))))

(ert-deftest voice-empty-answer-is-a-failure ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my words" '(:style summary))
    (voice-test--reply (car requests) "TITLE: Only a title\n")
    (let ((file (voice-test--file)))
      (should (string-match-p "Failed: the model returned no answer" file))
      ;; Still findable as untitled, so a retry can title it.
      (should (string-match-p "^\\* Voice note" file))))))

(ert-deftest voice-reasoning-with-no-answer-does-not-stay-working ()
  "gptel makes no further call after reasoning when the answer is empty."
  (voice-test--with-notes
   (voice-test--capturing-request
    (let (later)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn) (setq later fn))))
        (voice-test--route "my words" '(:style summary))
        (voice-test--reply (car requests) '(reasoning . "hmm")))
      (should (string-match-p "Working on it" (voice-test--file)))
      (funcall later)
      (should-not (string-match-p "Working on it" (voice-test--file)))
      (should (string-match-p "Failed: the model ran out of room" (voice-test--file)))))))

(ert-deftest voice-silent-model-times-out-and-a-late-answer-still-lands ()
  "gptel makes no call at all when a model answers with nothing."
  (voice-test--with-notes
   (voice-test--capturing-request
    (let (later)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn) (setq later fn))))
        (voice-test--route "my words" '(:style summary)))
      (should (string-match-p "Working on it" (voice-test--file)))
      (funcall later)
      (should (string-match-p "Failed: the model did not answer in time" (voice-test--file)))
      (voice-test--reply (car requests) "TITLE: Late\n\n- the answer")
      (should (string-match-p "- the answer" (voice-test--file)))
      (should-not (string-match-p "Failed" (voice-test--file)))))))

(ert-deftest voice-reasoning-then-answer-keeps-the-answer ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (let (later)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_time _repeat fn) (setq later fn))))
        (voice-test--route "my words" '(:style summary))
        (voice-test--reply (car requests) '(reasoning . "hmm"))
        (voice-test--reply (car requests) "TITLE: T\n\n- the answer"))
      (funcall later)
      (should (string-match-p "- the answer" (voice-test--file)))
      (should-not (string-match-p "Failed" (voice-test--file)))))))

;;;; Reprocess

(ert-deftest voice-reprocess-retries-a-failure-in-place ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my words" '(:style summary))
    (voice-test--reply (car requests) nil '(:status "down"))
    (with-current-buffer (ide-voice--buffer)
      (goto-char (point-min))
      (re-search-forward "my words")     ; point inside the Transcript subheading
      (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "summary")))
        (ide-voice-reprocess)))
    (should (= (length requests) 2))
    (should (equal (plist-get (cadr requests) :prompt) "my words"))
    (voice-test--reply (cadr requests) "TITLE: Retried\n\n- now it worked")
    (let ((file (voice-test--file)))
      (should-not (string-match-p "Failed" file))
      (should (string-match-p "^\\* Retried" file))
      (should (= 1 (voice-test--count "^\\*\\* Summary$" file)))))))

(defun voice-test--count (regexp string)
  "Count the matches of REGEXP in STRING."
  (with-temp-buffer (insert string) (goto-char (point-min)) (how-many regexp)))

(ert-deftest voice-reprocess-adds-a-section-per-audience ()
  (voice-test--with-notes
   (voice-test--capturing-request
    (voice-test--route "my words" '(:style summary))
    (voice-test--reply (car requests) "TITLE: Kept title\n\n- summary")
    (with-current-buffer (ide-voice--buffer)
      (goto-char (point-min))
      (re-search-forward "^\\* ")
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (prompt &rest _) (if (string-prefix-p "Style" prompt) "explain" "a manager"))))
        (ide-voice-reprocess)))
    (voice-test--reply (cadr requests) "TITLE: Ignored\n\n- for managers")
    (let ((file (voice-test--file)))
      (should (string-match-p "^\\* Kept title" file))
      (should (string-match-p "^\\*\\* Summary\n- summary" file))
      (should (string-match-p "^\\*\\* Explain for a manager\n- for managers" file))))))

(ert-deftest voice-reprocess-refuses-other-headings ()
  (with-temp-buffer
    (org-mode)
    (insert "* An ordinary heading\ntext\n")
    (goto-char (point-min))
    (should-error (ide-voice-reprocess) :type 'user-error)))

;;;; Starting and stopping

(defmacro voice-test--with-whisper (recording transcribing &rest body)
  "Run BODY with whisper.el stubbed; bind `runs' to the number of `whisper-run' calls."
  (declare (indent 2))
  `(let ((runs 0) (hook-at-run 'unset))
     (cl-letf (((symbol-function 'ide-voice--ensure-installed) #'ignore)
               ((symbol-function 'ide-voice--keep-last-recording) #'ignore)
               ((symbol-function 'whisper-recording-p) (lambda () ,recording))
               ((symbol-function 'whisper-transcribing-p) (lambda () ,transcribing))
               ((symbol-function 'whisper-run)
                (lambda (&rest _)
                  (cl-incf runs)
                  (setq hook-at-run whisper-before-transcription-hook))))
       ,@body)))

(ert-deftest voice-commands-set-what-happens-to-the-text ()
  (voice-test--with-notes
   (let ((whisper-before-transcription-hook '(a-check)))
     (voice-test--with-whisper nil nil
       (ide-voice-note)
       (should (equal ide-voice--pending '(:style summary)))
       ;; Notes do not need a writable buffer.
       (should (null hook-at-run))
       (ide-voice-dictate)
       (should (null ide-voice--pending))
       ;; Dictation keeps whisper.el's read-only check.
       (should (equal hook-at-run '(a-check)))
       (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "a manager")))
         (ide-voice-explain))
       (should (equal ide-voice--pending '(:style explain :audience "a manager")))
       (should (= runs 3))))))

(ert-deftest voice-any-key-stops-a-recording-without-asking-anything ()
  (voice-test--with-notes
   (let ((whisper-before-transcription-hook nil)
         (ide-voice--pending '(:style summary)))
     (voice-test--with-whisper t nil
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) (error "Stopping must not prompt"))))
         (ide-voice-explain))
       (should (= runs 1))
       ;; The recording keeps the purpose it was started with.
       (should (equal ide-voice--pending '(:style summary)))))))

(ert-deftest voice-keys-leave-a-running-transcription-alone ()
  "whisper-run would ask for a typed \"yes\" to kill it."
  (voice-test--with-notes
   (let ((whisper-before-transcription-hook nil)
         (ide-voice--pending '(:style summary)))
     (voice-test--with-whisper nil t
       (ide-voice-note)
       (ide-voice-dictate)
       (should (= runs 0))
       (should (equal ide-voice--pending '(:style summary)))))))

(ert-deftest voice-failed-start-forgets-the-purpose ()
  (voice-test--with-notes
   (let ((whisper-before-transcription-hook nil))
     (voice-test--with-whisper nil nil
       (cl-letf (((symbol-function 'whisper-run) (lambda (&rest _) (error "No microphone"))))
         (should-error (ide-voice-note)))
       (should (null ide-voice--pending))))))

(ert-deftest voice-missing-install-names-the-fix ()
  (let ((whisper-install-directory "/nonexistent/"))
    (cl-letf (((symbol-function 'require)
               (let ((real (symbol-function 'require)))
                 (lambda (feature &rest args)
                   (or (eq feature 'whisper) (apply real feature args))))))
      (should (string-match-p "make voice"
                              (cadr (should-error (ide-voice--ensure-installed) :type 'user-error)))))))

;;;; Vocabulary

(ert-deftest voice-vocabulary-reaches-the-recognizer ()
  (let ((ide-voice-vocabulary "Foundry, gptel"))
    (should (equal (ide-voice--add-vocabulary '("whisper-cli" "--file" "x.wav"))
                   '("whisper-cli" "--file" "x.wav" "--prompt" "Foundry, gptel"))))
  (let ((ide-voice-vocabulary nil))
    (should (equal (ide-voice--add-vocabulary '("whisper-cli")) '("whisper-cli")))))

(ert-deftest voice-recording-limit-allows-a-long-ramble ()
  (let ((whisper-recording-timeout 300) (whisper-install-directory nil)
        (whisper-install-whispercpp t) (whisper-model nil) (whisper-return-cursor t)
        (ide-voice-whisper-model "some-model"))
    (ide-voice--whisper-settings)
    (should (> whisper-recording-timeout 300))
    (should (eq whisper-install-whispercpp 'manual))
    (should (equal whisper-model "some-model"))
    (should (eq whisper-return-cursor 'end))))

;;; voice-test.el ends here
