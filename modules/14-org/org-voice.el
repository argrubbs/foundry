;;; org-voice.el --- Voice notes and dictation -*- lexical-binding: t; -*-

;;; Commentary:
;; Talk instead of type.  whisper.el records and transcribes; this module
;; decides where the words go: to point (dictation), or into a voice notes file
;; with a summary or an explanation for a chosen audience written by gptel.
;; The transcript is saved before any model is asked, so a failed or poor
;; summary never loses what was said.  `make voice' builds whisper.cpp.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar ide-notes-dir)
(defvar ide-cache-dir)
(defvar ide-var-dir)
(defvar whisper-install-directory)
(defvar whisper-install-whispercpp)
(defvar whisper-model)
(defvar whisper-recording-timeout)
(defvar whisper-return-cursor)
(defvar whisper-before-transcription-hook)
(defvar whisper-after-transcription-hook)
(defvar whisper--temp-file)
(defvar whisper--marker)
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-use-context)
(defvar gptel-use-tools)
(defvar gptel-temperature)

(declare-function whisper-run "whisper")
(declare-function whisper-recording-p "whisper")
(declare-function whisper-transcribing-p "whisper")
(declare-function gptel-request "gptel-request")
(declare-function gptel--convert-markdown->org "gptel-org")
(declare-function ide-ai--provider "13-ai")
(declare-function ide-ai--backend "13-ai")
(declare-function ide-ai--backend-provider "13-ai")
(declare-function org-back-to-heading "org")
(declare-function org-edit-headline "org")
(declare-function org-end-of-subtree "org")
(declare-function org-entry-get "org")
(declare-function org-entry-put "org")
(declare-function org-find-property "org")
(declare-function org-get-heading "org")
(declare-function org-up-heading-safe "org")

(defgroup ide-voice nil "Voice notes and dictation." :group 'tools)

(defcustom ide-voice-file nil
  "Org file voice notes go to; nil means voice-notes.org in `ide-notes-dir'."
  :type '(choice (const nil) file))

(defcustom ide-voice-model nil
  "Provider and model that write voice notes: (PROVIDER-ID . MODEL), or nil.
nil uses the current gptel default.  Example: \\='(ollama . qwen3.5:9b).
A hosted provider sends your transcripts off this machine."
  :type '(choice (const nil) (cons symbol symbol)))

(defcustom ide-voice-ollama-context 16384
  "Context window, in tokens, for voice requests to Ollama.
It must hold the whole transcript: half an hour of speech is about 8000 tokens,
and Ollama silently drops what does not fit."
  :type 'integer)

(defcustom ide-voice-timeout 300
  "Seconds to wait for a model before marking a voice note as failed.
gptel makes no call at all when a model returns an empty answer, so without
this the note would say it is working forever.  A late answer still lands."
  :type 'integer)

(defcustom ide-voice-whisper-model "large-v3-turbo"
  "Speech recognition model; whisper.el offers to download it on first use."
  :type 'string)

(defcustom ide-voice-vocabulary
  "Emacs, Org mode, Kubernetes, Terraform, Ansible, Azure DevOps, gptel, Ollama."
  "Words the speech recognizer should expect, or nil for none.
Add the names and terms you use that it gets wrong."
  :type '(choice (const nil) string))

(defcustom ide-voice-audiences
  '("a non-technical colleague" "a manager" "an engineer new to this area")
  "Audiences offered by `ide-voice-explain' and `ide-voice-reprocess'.
Anything else can be typed at the prompt."
  :type '(repeat string))

(defcustom ide-voice-styles
  '((summary . "You turn a rambling spoken transcript into clear notes for the person who said it.
Lead with the main point, then keep only what supports it, and list any action items they spoke.
Keep their meaning and do not add facts.  The transcript comes from speech recognition, so it may
contain misheard words: list anything ambiguous under \"Unclear:\" instead of guessing.")
    (explain . "You rewrite a spoken technical explanation so that %s can understand and act on it.
Replace or define jargon, keep the facts accurate, and say why it matters to them.  Do not add
facts.  Mark any analogy that simplifies something.  The transcript comes from speech recognition,
so it may contain misheard words: list anything ambiguous under \"Unclear:\" instead of guessing."))
  "How each style treats a transcript: (STYLE . SYSTEM-PROMPT).
A %s in the prompt is replaced by an audience, which is then asked for."
  :type '(alist :key-type symbol :value-type string))

(defconst ide-voice--format-rules
  "\n\nStart with one line: TITLE: and a title of at most eight words.  Then give only the result,
as Org mode text using short paragraphs and \"-\" bullets, with no headings and no preamble."
  "Output format asked of every style; `ide-voice--parse-response' relies on it.")

(defconst ide-voice--untitled "Voice note" "Headline of an entry until a model titles it.")

(defvar ide-voice--pending nil
  "Plist (:style :audience) for the recording in progress; nil means dictation.")

(defun ide-voice--whisper-settings ()
  "Settings for whisper.el, applied before it loads."
  (setq whisper-install-directory ide-cache-dir
        ;; `make voice' builds with Vulkan; whisper.el's own build is CPU only.
        whisper-install-whispercpp 'manual
        whisper-model ide-voice-whisper-model
        ;; The default of five minutes would cut a long explanation short.
        whisper-recording-timeout 1800
        ;; Dictating again carries on after the text just inserted.
        whisper-return-cursor 'end))

(use-package whisper
  :ensure (:host github :repo "natrys/whisper.el")  ; MELPA's `whisper' is another package
  :commands (whisper-run whisper-file)
  :init (ide-voice--whisper-settings))

(defun ide-voice--add-vocabulary (command)
  "Filter `whisper-command': add `ide-voice-vocabulary' to whisper.cpp's COMMAND."
  (if ide-voice-vocabulary (append command (list "--prompt" ide-voice-vocabulary)) command))

(with-eval-after-load 'whisper
  (advice-add 'whisper-command :filter-return #'ide-voice--add-vocabulary))

;;;; The notes file -----------------------------------------------------------

(defun ide-voice--file ()
  (or ide-voice-file (expand-file-name "voice-notes.org" ide-notes-dir)))

(defun ide-voice--buffer ()
  "The buffer visiting the voice notes file."
  (let ((file (ide-voice--file)))
    (make-directory (file-name-directory file) t)
    (find-file-noselect file)))

(defun ide-voice--save ()
  (let ((save-silently t)) (save-buffer)))

(defmacro ide-voice--with-entry (id &rest body)
  "Run BODY at the heading of entry ID, then save; return nil if the entry is gone."
  (declare (indent 1))
  `(with-current-buffer (ide-voice--buffer)
     (when-let* ((pos (save-restriction (widen) (org-find-property "VOICE_ID" ,id))))
       (save-excursion
         (save-restriction
           (widen)
           (goto-char pos)
           ,@body))
       (ide-voice--save)
       t)))

(defun ide-voice--escape (text)
  "Keep lines of TEXT from reading as Org headings."
  (replace-regexp-in-string "^\\*" ",*" text))

(defun ide-voice--unescape (text)
  (replace-regexp-in-string "^,\\*" "*" text))

(defun ide-voice--write-entry (transcript style audience)
  "Add an entry holding TRANSCRIPT, newest first, save the file and return its id."
  (let ((id (format-time-string "%Y%m%dT%H%M%S")))
    (with-current-buffer (ide-voice--buffer)
      (save-excursion
        (save-restriction
          (widen)
          (when (= (buffer-size) 0) (insert "#+title: Voice notes\n\n"))
          (goto-char (point-min))
          (if (re-search-forward "^\\* " nil t)
              (goto-char (match-beginning 0))
            (goto-char (point-max))
            (unless (bolp) (insert "\n")))
          (insert (format "* %s :voice:\n:PROPERTIES:\n:VOICE_ID: %s\n:STYLE: %s\n%s:END:\n%s\n"
                          ide-voice--untitled id style
                          (if audience (format ":AUDIENCE: %s\n" audience) "")
                          (format-time-string "[%Y-%m-%d %a %H:%M]"))
                  "** Transcript\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n"
                  (ide-voice--escape transcript) "\n\n")))
      (ide-voice--save))
    id))

(defun ide-voice--set-section (heading body)
  "In the entry at point, make BODY the text of subheading HEADING.
A new subheading goes before the transcript."
  (let ((end (save-excursion (org-end-of-subtree t t) (point))))
    (if (re-search-forward (format "^\\*\\* %s[ \t]*$" (regexp-quote heading)) end t)
        (let* ((start (line-beginning-position 2))
               (stop (progn (goto-char start)
                            (if (re-search-forward "^\\*+ " end t) (match-beginning 0) end))))
          (delete-region start stop)
          (goto-char start)
          (insert body "\n"))
      (if (re-search-forward "^\\*\\* Transcript" end t)
          (goto-char (match-beginning 0))
        (goto-char end))
      (insert "** " heading "\n" body "\n"))))

(defun ide-voice--entry-transcript ()
  "The transcript of the entry at point, or nil."
  (let ((end (save-excursion (org-end-of-subtree t t) (point))))
    (save-excursion
      (when (re-search-forward "^\\*\\* Transcript[ \t]*$" end t)
        (forward-line 1)
        (when (looking-at-p "[ \t]*:PROPERTIES:")
          (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
          (forward-line 1))
        (ide-voice--unescape
         (string-trim (buffer-substring-no-properties
                       (point) (if (re-search-forward "^\\*+ " end t) (match-beginning 0) end))))))))

;;;; Asking the model ---------------------------------------------------------

(defun ide-voice--heading (style audience)
  "The subheading a result of STYLE for AUDIENCE goes under."
  (if audience
      (format "%s for %s" (capitalize (symbol-name style)) audience)
    (capitalize (symbol-name style))))

(defun ide-voice--system-prompt (style audience)
  (let ((prompt (or (alist-get style ide-voice-styles)
                    (error "No voice style named %s" style))))
    (concat (if audience (format prompt audience) prompt) ide-voice--format-rules)))

(defun ide-voice--parse-response (text)
  "Split model output TEXT into (TITLE . ORG-BODY).
TITLE is nil when the model gave none.  Reasoning is dropped, Markdown becomes
Org, and headings become bullets so they cannot break the entry's structure."
  (require 'gptel-org)
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (search-forward "<think>" nil t)
      (let ((start (match-beginning 0)))
        (when (search-forward "</think>" nil t) (delete-region start (point)))))
    (goto-char (point-min))
    (let ((title (when (re-search-forward "^[*#_ \t]*TITLE:[*_ \t]*\\(.*?\\)[*_ \t]*$" nil t)
                   (prog1 (match-string 1)
                     (delete-region (line-beginning-position) (line-beginning-position 2))))))
      (cons (and title (not (string-empty-p title)) title)
            (replace-regexp-in-string
             "^\\*+ " "- " (gptel--convert-markdown->org (string-trim (buffer-string))))))))

(defun ide-voice--cut-off-p (info)
  "Non-nil when request INFO says the model ran out of room before finishing."
  (member (format "%s" (plist-get info :stop-reason)) '("length" "max_tokens" "MAX_TOKENS")))

(defun ide-voice--finish (id heading response error-text &optional cut-off)
  "Put RESPONSE, or ERROR-TEXT when it is nil, under HEADING of entry ID.
CUT-OFF non-nil adds a warning that RESPONSE is incomplete."
  (let* ((parsed (and response (ide-voice--parse-response response)))
         (title (car parsed))
         (body (and parsed (not (string-empty-p (string-trim (cdr parsed)))) (cdr parsed)))
         (problem (or error-text (and (not body) "the model returned no answer")))
         (saved (ide-voice--with-entry id
                  (when (and title body (equal (org-get-heading t t t t) ide-voice--untitled))
                    (org-edit-headline title))
                  (ide-voice--set-section
                   heading
                   (if body
                       (concat body
                               (and cut-off
                                    "\n\n/Cut off: the model ran out of room.  M-x ide-voice-reprocess tries again./"))
                     (format "/Failed: %s.  Fix that, then run M-x ide-voice-reprocess here./" problem))))))
    (cond ((not saved)
           (when body (kill-new body))
           (message "Voice note %s is no longer in %s%s" id (ide-voice--file)
                    (if body "; the result is on the kill ring" "")))
          ((and body cut-off) (message "Voice note cut off, the model ran out of room: %s" (or title heading)))
          (body (message "Voice note ready: %s" (or title heading)))
          (t (message "Voice note saved without a summary: %s" problem)))))

(defun ide-voice--backend ()
  "The gptel backend and model for voice requests: (BACKEND . MODEL).
A local model is asked not to reason first: reasoning fills its context window,
often leaving no room for any answer, and takes several times as long.  The
backend is a private copy, so chat sessions on the same provider are unaffected."
  (let* ((provider (if ide-voice-model
                       (or (ide-ai--provider (car ide-voice-model))
                           (error "Unknown AI provider %s" (car ide-voice-model)))
                     (ide-ai--backend-provider gptel-backend)))
         (model (if ide-voice-model (cdr ide-voice-model) gptel-model))
         (backend (if ide-voice-model (ide-ai--backend provider (list model)) gptel-backend)))
    (if (eq (car provider) 'ollama)
        (let ((copy (copy-sequence backend)))
          (setf (cl-struct-slot-value (type-of copy) 'request-params copy)
                `(:think :json-false
                  :options (:num_ctx ,ide-voice-ollama-context :num_predict 2048)))
          (cons copy model))
      (cons backend model))))

(defun ide-voice--request (id text style audience)
  "Ask the model for STYLE of TEXT for AUDIENCE, into entry ID.
Whatever goes wrong is recorded in the entry; TEXT is already safe there."
  (let* ((heading (ide-voice--heading style audience))
         (answered nil)
         (give-up (lambda (seconds problem)
                    (run-at-time seconds nil
                                 (lambda ()
                                   (unless answered
                                     (setq answered t)
                                     (ide-voice--finish id heading nil problem)))))))
    (condition-case err
        (progn
          (require 'gptel)
          (let* ((choice (ide-voice--backend))
                 (gptel-backend (car choice))
                 (gptel-model (cdr choice))
                 ;; Chat context and tools belong to chat; low temperature keeps it faithful.
                 (gptel-use-context nil)
                 (gptel-use-tools nil)
                 (gptel-temperature 0.3))
            (ide-voice--with-entry id
              (org-entry-put nil "MODEL" (format "%s" gptel-model))
              (ide-voice--set-section heading "/Working on it…/"))
            (gptel-request text
              :buffer (ide-voice--buffer)
              :system (ide-voice--system-prompt style audience)
              :callback
              (lambda (response info)
                (cond
                 ((stringp response)
                  (setq answered t)
                  (ide-voice--finish id heading response nil (ide-voice--cut-off-p info)))
                 ((or (null response) (eq response 'abort))
                  (setq answered t)
                  (ide-voice--finish id heading nil
                                     (or (plist-get info :status) "the request was aborted")))
                 ;; Reasoning with no answer after it ends with no further
                 ;; call, so there is no need to wait out the whole timeout.
                 ((eq (car-safe response) 'reasoning)
                  (funcall give-up 1 "the model ran out of room while reasoning")))))
            (funcall give-up ide-voice-timeout "the model did not answer in time")))
      (error (ide-voice--finish id heading nil (error-message-string err))))))

(defun ide-voice--capture (text style audience)
  "Save transcript TEXT as a new entry, then ask for STYLE for AUDIENCE."
  (condition-case err
      (ide-voice--request (ide-voice--write-entry text style audience) text style audience)
    (error
     (kill-new text)
     (message "Voice note not saved (%s); the transcript is on the kill ring"
              (error-message-string err)))))

;;;; Routing a finished transcription ------------------------------------------

(defun ide-voice--blank-p (text)
  "Non-nil when TEXT holds only markers such as [BLANK_AUDIO] or (silence)."
  (string-match-p "\\`\\(?:[ \t\n]*\\(?:\\[[^]]*\\]\\|([^)]*)\\)\\)*[ \t\n]*\\'" text))

(defun ide-voice--space-before ()
  "Put a space before the dictated text in this buffer when it follows other text."
  (when-let* ((buffer (and (markerp whisper--marker) (marker-buffer whisper--marker)))
              (before (with-current-buffer buffer (char-before whisper--marker))))
    (unless (memq (char-syntax before) '(?\s ?\())
      (goto-char (point-min))
      (insert " "))))

(defun ide-voice--route ()
  "Send the transcription in this buffer where it belongs.
Runs on `whisper-after-transcription-hook'.  whisper.el inserts whatever is
left here at point, so emptying the buffer sends the text to the notes file."
  (let ((pending ide-voice--pending)
        (text (string-trim (buffer-string))))
    (setq ide-voice--pending nil)
    (cond ((ide-voice--blank-p text)
           (erase-buffer)
           (message "No speech heard"))
          (pending
           (erase-buffer)
           (ide-voice--capture text (plist-get pending :style) (plist-get pending :audience)))
          (t (ide-voice--space-before)))))

(add-hook 'whisper-after-transcription-hook #'ide-voice--route)

;;;; Commands -------------------------------------------------------------------

(defun ide-voice--ensure-installed ()
  (unless (require 'whisper nil t)
    (user-error "The whisper package is not installed; run make packages"))
  (let ((cli (expand-file-name "whisper.cpp/build/bin/whisper-cli" whisper-install-directory)))
    (unless (file-executable-p cli)
      (user-error "No speech recognizer at %s; run make voice" cli))))

(defun ide-voice--keep-last-recording ()
  "Move the previous recording aside, so a failed transcription can be retried."
  (ignore-errors
    (when (file-exists-p whisper--temp-file)
      (let ((dir (expand-file-name "voice/" ide-var-dir)))
        (make-directory dir t)
        (rename-file whisper--temp-file (expand-file-name "last.wav" dir) t)))))

(defun ide-voice--record (pending)
  "Start recording; PENDING is what happens to the text (see `ide-voice--pending')."
  (ide-voice--keep-last-recording)
  (setq ide-voice--pending pending)
  (condition-case err
      (if pending
          ;; Notes do not touch the current buffer, so a read-only one is fine.
          (let ((whisper-before-transcription-hook nil)) (whisper-run))
        (whisper-run))
    (error (setq ide-voice--pending nil)
           (signal (car err) (cdr err)))))

(defun ide-voice--toggle (start)
  "Stop the recording in progress, or call START to begin one.
Never offers whisper.el's typed prompt to kill a running transcription."
  (ide-voice--ensure-installed)
  (cond ((whisper-recording-p) (whisper-run))
        ((whisper-transcribing-p) (message "Still transcribing the last recording"))
        (t (funcall start))))

;;;###autoload
(defun ide-voice-note ()
  "Record a voice note, or stop the recording in progress.
The transcript goes to `ide-voice-file' with a summary by `ide-voice-model'."
  (interactive)
  (ide-voice--toggle (lambda () (ide-voice--record '(:style summary)))))

;;;###autoload
(defun ide-voice-dictate ()
  "Record speech to insert at point, or stop the recording in progress.
The text appears when transcription finishes, not while speaking."
  (interactive)
  (ide-voice--toggle (lambda () (ide-voice--record nil))))

;;;###autoload
(defun ide-voice-explain ()
  "Record an explanation to rewrite for an audience, or stop the recording."
  (interactive)
  (ide-voice--toggle
   (lambda () (ide-voice--record (list :style 'explain :audience (ide-voice--read-audience))))))

(defun ide-voice--read-audience ()
  (completing-read (format-prompt "Audience" (car ide-voice-audiences))
                   ide-voice-audiences nil nil nil nil (car ide-voice-audiences)))

;;;###autoload
(defun ide-voice-reprocess ()
  "Run a style again on the transcript of the voice note at point.
Use it to retry a failed summary, or to explain a note to another audience;
each style and audience keeps its own section."
  (interactive)
  (require 'org)
  (unless (derived-mode-p 'org-mode) (user-error "Not in an Org buffer"))
  (pcase-let*
      ((`(,id ,text ,current)
        (save-excursion
          (org-back-to-heading t)
          (while (org-up-heading-safe))
          (list (org-entry-get nil "VOICE_ID")
                (ide-voice--entry-transcript)
                (org-entry-get nil "STYLE"))))
       (style (progn
                (unless (and id text) (user-error "Not on a voice note with a transcript"))
                (intern (completing-read (format-prompt "Style" current)
                                         (mapcar #'car ide-voice-styles) nil t nil nil current))))
       (audience (and (string-match-p "%s" (or (alist-get style ide-voice-styles) ""))
                      (ide-voice--read-audience))))
    (ide-voice--request id text style audience)))

;;;###autoload
(defun ide-voice-open-notes ()
  "Open the voice notes file."
  (interactive)
  (pop-to-buffer (ide-voice--buffer)))

(provide 'org-voice)
;;; org-voice.el ends here
