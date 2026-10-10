;;; docs-update-test.el --- Tests for the documentation viewer and updater -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/docs-update-test.el
;; No network and no real AI agent: the agent is a fake function.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require '15-docs)

(defvar docs-upd--root (file-name-as-directory (expand-file-name user-emacs-directory)))
(defvar docs-upd--page (expand-file-name "docs/index.html" docs-upd--root))
(defvar docs-upd--facts (ide-docs--facts) "Real facts, computed once.")

(defun docs-upd--slurp (f) (with-temp-buffer (insert-file-contents f) (buffer-string)))

(defmacro docs-upd--with-page (&rest body)
  "Run BODY against a temporary copy of the page with safe paths and a passing validator."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "docs-upd" t)))
          (ide-docs-file (expand-file-name "index.html" dir))
          (ide-docs-backup-dir (expand-file-name "backups/" dir))
          (ide-docs-candidate-file (expand-file-name "cand/index.html" dir))
          (ide-docs-update-confirm nil)
          (ide-docs-agent-timeout 5)
          (ide-docs-max-retries 2)
          (ide-docs-validate-function (lambda (_f cb) (funcall cb (cons t "ok")))))
     (copy-file docs-upd--page ide-docs-file)
     (unwind-protect (progn ,@body) (delete-directory dir t))))

(defun docs-upd--wait (fn)
  "Call FN with an on-done callback and wait (max 20 s) for (STATUS . MESSAGE)."
  (let (res)
    (funcall fn (lambda (s m) (setq res (cons s m))))
    (let ((deadline (+ (float-time) 20)))
      (while (and (null res) (< (float-time) deadline)) (accept-process-output nil 0.05)))
    res))

(defun docs-upd--facts-with-key ()
  "Real facts plus one invented binding of a real, documented-looking command."
  (let ((f (copy-sequence docs-upd--facts)))
    (plist-put f :keybinds (append (plist-get f :keybinds) (list (list "C-c z z" 'zz-docs-test-cmd "global"))))))

(defmacro docs-upd--with-test-command (&rest body)
  `(progn (defalias 'zz-docs-test-cmd (lambda () "Do a test thing." (interactive)))
          (unwind-protect (progn ,@body) (fmakunbound 'zz-docs-test-cmd))))

(defun docs-upd--agent (&rest replies)
  "Return an agent function answering with REPLIES in turn; records prompts in `docs-upd--prompts'."
  (setq docs-upd--prompts nil)
  (lambda (system prompt callback)
    (push (cons system prompt) docs-upd--prompts)
    (funcall callback (pop replies) nil)))
(defvar docs-upd--prompts nil)

(defun docs-upd--good-reply ()
  (json-serialize '((keybinds . [((keys . "C-c z z") (ctx . "global") (title . "Zed command")
                                  (desc . "Runs the zed test command.") (cat . "Help & docs"))]))))

;;;; Data IO ---------------------------------------------------------------------------------

(ert-deftest docs-upd-json-roundtrip ()
  (let* ((html (docs-upd--slurp docs-upd--page)) (data (ide-docs--parse-data html))
         (text (ide-docs--serialize data)))
    (should-not (string-match-p "</" text))
    (should (string-match-p "≈" text))
    (should (equal (json-parse-string text :object-type 'alist :array-type 'array :null-object :null :false-object :false) data))
    (should (equal (ide-docs--parse-data (ide-docs--render html data)) data))))

(ert-deftest docs-upd-non-ascii-survives-prompt-and-page ()
  "Non-ASCII text (arrows, emoji) stays real text, not raw bytes."
  (let* ((html (docs-upd--slurp docs-upd--page)) (data (ide-docs--parse-data html))
         (out (ide-docs--render html (ide-docs--put data 'verified (vector "Café → 🐍")))))
    (should (multibyte-string-p out))
    (should (string-match-p "Café → 🐍" out))
    (should (equal (alist-get 'verified (ide-docs--parse-data out)) ["Café → 🐍"])))
  (docs-upd--with-test-command
    (defalias 'zz-docs-test-cmd (lambda () "Zeigt Größe → an." (interactive)))
    (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
           (r (ide-docs--merge data (docs-upd--facts-with-key))))
      (should (string-match-p "Größe → an" (ide-docs--build-prompt (car r) (ide-docs--needs (cdr r))))))))

(ert-deftest docs-upd-examples-are-read-from-files ()
  (let ((ex (ide-docs--facts-examples)))
    (should (equal (mapcar (lambda (e) (alist-get 'id e)) ex)
                   (append (cl-remove-if-not (lambda (i) (file-exists-p (expand-file-name (concat "examples/" i ".el.example") docs-upd--root)))
                                             ide-docs-example-order)
                           (cl-remove-if-not (lambda (i) (and (member i ide-docs-example-order)
                                                              (not (file-exists-p (expand-file-name (concat "examples/" i ".el.example") docs-upd--root)))))
                                             ide-docs-example-order))))
    (dolist (e ex)
      (should (> (length (alist-get 'title e)) 3))
      (should (> (length (alist-get 'blurb e)) 20))
      (should (> (length (alist-get 'files e)) 0)))
    (let ((rust (cl-find "rust" ex :key (lambda (e) (alist-get 'id e)) :test #'equal)))
      (should (equal (mapcar (lambda (f) (alist-get 'path f)) (alist-get 'files rust)) '("user/rust.el" "user/mise.toml")))
      (should (string-match-p "rust-analyzer" (alist-get 'code (aref (alist-get 'files rust) 0)))))))

(ert-deftest docs-upd-merge-refreshes-stale-examples ()
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (stale (ide-docs--put data 'examples (vector `((id . "gone") (title . "Gone") (blurb . "x") (files . [])))))
         (r (ide-docs--merge stale docs-upd--facts)))
    (should (plist-get (cdr r) :examples-changed))
    (should (equal (alist-get 'examples (car r)) (vconcat (plist-get docs-upd--facts :examples))))))

(ert-deftest docs-upd-script-injection-is-inert ()
  "A description containing </script> cannot terminate the data block."
  (let* ((html (docs-upd--slurp docs-upd--page)) (data (ide-docs--parse-data html))
         (evil (ide-docs--put data 'verified (vector "</script><img src=x onerror=alert(1)>")))
         (out (ide-docs--render html evil)))
    (should (equal (ide-docs--parse-data out) evil))
    (should-not (string-match-p "</script><img" out))))

;;;; Facts and merge ----------------------------------------------------------------------------

(ert-deftest docs-upd-page-matches-live-config ()
  "After an update there is nothing left to fix: the page is in sync."
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (report (cdr (ide-docs--merge data docs-upd--facts))))
    (dolist (k '(:kb-added :kb-removed :kb-changed :menu-added :menu-changed :tools-added :tools-removed
                 :modules-added :modules-removed :make-added :make-removed :cmds-dropped :uncovered :stale-steps))
      (should-not (plist-get report k)))))

(ert-deftest docs-upd-merge-detects-new-removed-changed-keys ()
  (docs-upd--with-test-command
    (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
           (facts (docs-upd--facts-with-key))
           (r (ide-docs--merge data facts)) (rep (cdr r)))
      (should (equal (plist-get rep :kb-added) '(("C-c z z" . "global"))))
      (let ((e (cl-find "C-c z z" (alist-get 'keybinds (car r)) :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
        (should (equal (alist-get 'desc e) "Do a test thing"))
        (should (equal (alist-get 'cat e) "Other")))
      (should (ide-docs--needs-any-p (ide-docs--needs rep)))
      ;; removal
      (let* ((facts2 (plist-put (copy-sequence docs-upd--facts) :keybinds (cdr (plist-get docs-upd--facts :keybinds))))
             (rep2 (cdr (ide-docs--merge data facts2))))
        (should (= 1 (length (plist-get rep2 :kb-removed)))))
      ;; changed command keeps prose, is flagged
      (let* ((kbs (plist-get docs-upd--facts :keybinds))
             (changed (cons (list (nth 0 (car kbs)) 'ignore (nth 2 (car kbs))) (cdr kbs)))
             (r3 (ide-docs--merge data (plist-put (copy-sequence docs-upd--facts) :keybinds changed)))
             (e (cl-find (nth 0 (car kbs)) (alist-get 'keybinds (car r3)) :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
        (should (= 1 (length (plist-get (cdr r3) :kb-changed))))
        (should (equal (alist-get 'cmd e) "ignore"))
        (should (alist-get 'title e))))))

(ert-deftest docs-upd-merge-repairs-menu-labels-without-flagging ()
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (menus (mapcar (lambda (m) (if (equal (alist-get 'id m) "services")
                                        (ide-docs--put m 'groups
                                                       (vconcat (mapcar (lambda (g) (ide-docs--put g 'items (vconcat (mapcar (lambda (i) (if (equal (alist-get 'key i) "d") (ide-docs--put i 'label "Docker!") i)) (alist-get 'items g))))) (alist-get 'groups m))))
                                      m))
                        (alist-get 'menus data)))
         (broken (ide-docs--put data 'menus (vconcat menus)))
         (r (ide-docs--merge broken docs-upd--facts)))
    (should-not (plist-get (cdr r) :menu-changed))
    (should (equal (alist-get 'menus (car r)) (alist-get 'menus data)))))

(ert-deftest docs-upd-merge-drops-vanished-commands-and-flags-uncovered ()
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (cards (mapcar (lambda (f) (if (equal (alist-get 'title f) "Docker")
                                        (ide-docs--put f 'cmds (vconcat (alist-get 'cmds f) ["ide-no-such-command"])) f))
                        (alist-get 'features data)))
         (r (ide-docs--merge (ide-docs--put data 'features (vconcat cards)) docs-upd--facts)))
    (should (equal (plist-get (cdr r) :cmds-dropped) '("ide-no-such-command")))))

(ert-deftest docs-upd-stats-are-recomputed ()
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (stale (ide-docs--put data 'stats (vconcat (mapcar (lambda (s) (ide-docs--put s 'v "1")) (alist-get 'stats data)))))
         (r (car (ide-docs--merge stale docs-upd--facts)))
         (v (lambda (re) (alist-get 'v (cl-find-if (lambda (s) (string-match-p re (alist-get 'l s))) (alist-get 'stats r))))))
    (should (equal (funcall v "automated tests") (number-to-string (plist-get docs-upd--facts :tests))))
    (should (equal (funcall v "keybindings") (number-to-string (length (alist-get 'keybinds r)))))
    (should (equal (funcall v "first frame") "1"))))

(ert-deftest docs-upd-stale-workflow-steps ()
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (wf (vconcat (alist-get 'workflows data)
                      (vector `((id . "x") (title . "X") (blurb . "")
                                (steps . [((t . "a") (d . "Press {{C-c z q}} now")) ((t . "b") (d . "Run [[ide-gone-command]]"))
                                          ((t . "c") (d . "Set [[confluence-site]] and press {{C-b}}"))])))))
         (bad (ide-docs--workflow-problems (ide-docs--put data 'workflows wf)
                                           (mapcar (lambda (f) (ide-docs--norm-keys (nth 0 f))) (plist-get docs-upd--facts :keybinds)))))
    (should (equal bad '(("x" . 0) ("x" . 1))))))

;;;; Agent reply handling --------------------------------------------------------------------------

(ert-deftest docs-upd-extract-json ()
  (should (equal (car (ide-docs--extract-json "```json\n{\"a\": 1}\n```")) '((a . 1))))
  (should (equal (car (ide-docs--extract-json "Sure! Here you go: {\"a\": [1,2]} hope it helps")) '((a . (1 2)))))
  (should (string-match-p "no JSON" (cdr (ide-docs--extract-json "nothing here"))))
  (should (string-match-p "invalid JSON" (cdr (ide-docs--extract-json "{\"a\": }")))))

(ert-deftest docs-upd-apply-accepts-only-requested-items ()
  (docs-upd--with-test-command
    (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
           (r (ide-docs--merge data (docs-upd--facts-with-key)))
           (needs (ide-docs--needs (cdr r)))
           (resp `((keybinds . (((keys . "C-c z z") (ctx . "global") (title . "Zed\ncommand") (desc . ,(make-string 400 ?x)) (cat . "Help & docs"))
                                ((keys . "M-1") (ctx . "global") (title . "HIJACKED") (desc . "nope") (cat . "AI"))))
                   (menu_items . (((menu . "services") (key . "d") (does . "nope"))))))
           (out (ide-docs--apply-response (car r) needs resp))
           (e (cl-find "C-c z z" (alist-get 'keybinds (car out)) :key (lambda (k) (alist-get 'keys k)) :test #'equal))
           (m1 (cl-find "M-1" (alist-get 'keybinds (car out)) :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
      (should (equal (alist-get 'title e) "Zed command"))
      (should (<= (length (alist-get 'desc e)) 160))
      (should (equal (alist-get 'cat e) "Help & docs"))
      (should-not (equal (alist-get 'title m1) "HIJACKED"))
      (should (= 2 (length (cdr out)))))))

(ert-deftest docs-upd-apply-rejects-malformed-and-unknown ()
  (docs-upd--with-test-command
    (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
           (r (ide-docs--merge data (docs-upd--facts-with-key)))
           (needs (plist-put (ide-docs--needs (cdr r)) :commands '(ide-docs-update-facts-only)))
           (resp '((keybinds . (((keys . "C-c z z") (ctx . "global") (title . "only title"))))
                   (commands . (((cmd . "ide-docs-update-facts-only") (card . "No such card"))
                                ((cmd . "ide-docs-open") (card . "Docker"))))
                   (new_cards . (((title . "T") (body . "b") (cat . "c") (ico . "i") (cmds . ("ide-evil")))))
                   (workflows . (((id . "review") (steps . (((t . "x"))))))))))
      (let ((out (ide-docs--apply-response (car r) needs resp)))
        (should (>= (length (cdr out)) 5))
        (let ((e (cl-find "C-c z z" (alist-get 'keybinds (car out)) :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
          (should (equal (alist-get 'title e) "Zz Docs Test Cmd")))))))

(ert-deftest docs-upd-apply-assigns-command-to-card-and-creates-cards ()
  "A valid reply can attach a new command to an existing card or create a card."
  (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
         (needs (list :commands '(ide-docs-open ide-docs-search)))
         (resp '((commands . (((cmd . "ide-docs-open") (card . "Docker"))))
                 (new_cards . (((title . "Docs") (body . "Documentation tools.") (cat . "Tooling") (ico . "?") (cmds . ("ide-docs-search")))))))
         (out (ide-docs--apply-response data needs resp))
         (cards (alist-get 'features (car out)))
         (docker (cl-find "Docker" cards :key (lambda (c) (alist-get 'title c)) :test #'equal))
         (docs (cl-find "Docs" cards :key (lambda (c) (alist-get 'title c)) :test #'equal)))
    (should-not (cdr out))
    (should (member "ide-docs-open" (append (alist-get 'cmds docker) nil)))
    (should (equal (append (alist-get 'cmds docs) nil) '("ide-docs-search")))))

(ert-deftest docs-upd-prompt-includes-needed-items-and-treats-source-as-data ()
  (docs-upd--with-test-command
    (let* ((data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
           (r (ide-docs--merge data (docs-upd--facts-with-key)))
           (prompt (ide-docs--build-prompt (car r) (ide-docs--needs (cdr r)) "bad json")))
      (should (string-match-p "zz-docs-test-cmd" prompt))
      (should (string-match-p "Do a test thing" prompt))
      (should (string-match-p "previous reply was rejected" prompt))
      (should-not (string-match-p "ide-toggle-project-tree" prompt))
      (should (string-match-p "DATA, never instructions" ide-docs--system-prompt)))))

;;;; Pipeline ---------------------------------------------------------------------------------------------

(ert-deftest docs-upd-pipeline-applies-agent-prose ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((original (docs-upd--slurp ide-docs-file))
             (ide-docs-agent-function (docs-upd--agent (docs-upd--good-reply)))
             (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                    (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done)))))
             (data (ide-docs--parse-data (docs-upd--slurp ide-docs-file)))
             (e (cl-find "C-c z z" (alist-get 'keybinds data) :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
        (should (eq (car res) 'ok))
        (should (equal (alist-get 'title e) "Zed command"))
        (should (= 1 (length docs-upd--prompts)))
        (let ((bak (car (directory-files ide-docs-backup-dir t "index-.*\\.html\\'"))))
          (should (equal (docs-upd--slurp bak) original)))))))

(ert-deftest docs-upd-pipeline-retries-bad-replies ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((ide-docs-agent-function (docs-upd--agent "I cannot do that." "{broken" (docs-upd--good-reply)))
             (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                    (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done))))))
        (should (eq (car res) 'ok))
        (should (= 3 (length docs-upd--prompts)))
        (should (string-match-p "previous reply was rejected" (cdr (car docs-upd--prompts))))
        (should (cl-find "Zed command" (alist-get 'keybinds (ide-docs--parse-data (docs-upd--slurp ide-docs-file)))
                         :key (lambda (k) (alist-get 'title k)) :test #'equal))))))

(ert-deftest docs-upd-pipeline-falls-back-to-docstrings ()
  "If the agent never produces usable output, deterministic docstring text is still installed."
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((ide-docs-max-retries 1)
             (ide-docs-agent-function (docs-upd--agent "nope" "still nope" "never"))
             (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                    (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done)))))
             (e (cl-find "C-c z z" (alist-get 'keybinds (ide-docs--parse-data (docs-upd--slurp ide-docs-file)))
                         :key (lambda (k) (alist-get 'keys k)) :test #'equal)))
        (should (eq (car res) 'ok))
        (should (= 2 (length docs-upd--prompts)))
        (should (equal (alist-get 'desc e) "Do a test thing"))))))

(ert-deftest docs-upd-pipeline-survives-agent-error-and-timeout ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let ((ide-docs-max-retries 0))
        ;; the agent function signals an error
        (let* ((ide-docs-agent-function (lambda (&rest _) (error "backend exploded")))
               (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                      (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done))))))
          (should (eq (car res) 'ok)))
        ;; the agent never answers
        (copy-file docs-upd--page ide-docs-file t)
        (let* ((ide-docs-agent-timeout 0.3)
               (ide-docs-agent-function (lambda (&rest _) nil))
               (start (float-time))
               (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                      (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done))))))
          (should (eq (car res) 'ok))
          (should (< (- (float-time) start) 10)))))))

(ert-deftest docs-upd-failed-validation-leaves-page-untouched ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((original (docs-upd--slurp ide-docs-file))
             (ide-docs-validate-function (lambda (_f cb) (funcall cb (cons nil "docs-check: FAILED something"))))
             (ide-docs-agent-function (docs-upd--agent (docs-upd--good-reply)))
             (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                    (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done))))))
        (should (eq (car res) 'failed))
        (should (equal (docs-upd--slurp ide-docs-file) original))
        (should-not (file-exists-p ide-docs-backup-dir))))))

(ert-deftest docs-upd-dry-run-and-unchanged-write-nothing ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let ((original (docs-upd--slurp ide-docs-file)) (validated nil)
            (ide-docs-validate-function (lambda (_f cb) (setq validated t) (funcall cb (cons t "ok")))))
        (should (eq 'dry-run (car (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                                    (docs-upd--wait (lambda (done) (ide-docs--run :agent t :dry-run t :on-done done)))))))
        (should (equal (docs-upd--slurp ide-docs-file) original))
        (should-not validated)))
    ;; with the real facts the page is in sync, so nothing is written or validated
    (let ((original (docs-upd--slurp ide-docs-file)) (validated nil)
          (ide-docs-validate-function (lambda (_f cb) (setq validated t) (funcall cb (cons t "ok")))))
      (should (eq 'unchanged (car (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done))))))
      (should (equal (docs-upd--slurp ide-docs-file) original))
      (should-not validated))))

(ert-deftest docs-upd-no-agent-means-no-agent-calls ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((ide-docs-agent-function (docs-upd--agent (docs-upd--good-reply)))
             (res (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key))
                    (docs-upd--wait (lambda (done) (ide-docs--run :agent nil :on-done done))))))
        (should (eq (car res) 'ok))
        (should (null docs-upd--prompts))))))

(ert-deftest docs-upd-agent-needs-consent ()
  (docs-upd--with-page
    (docs-upd--with-test-command
      (let* ((ide-docs-update-confirm t)
             (ide-docs-agent-function (docs-upd--agent (docs-upd--good-reply))))
        (cl-letf (((symbol-function 'ide-docs--facts) #'docs-upd--facts-with-key)
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  (noninteractive nil))
          (let ((res (docs-upd--wait (lambda (done) (ide-docs--run :agent t :on-done done)))))
            (should (eq (car res) 'ok))
            (should (null docs-upd--prompts))))))))

(ert-deftest docs-upd-install-backs-up-and-prunes ()
  (docs-upd--with-page
    (make-directory ide-docs-backup-dir t)
    (dolist (n '("index-20200101-000000.html" "index-20200102-000000.html" "index-20200103-000000.html"))
      (with-temp-file (expand-file-name n ide-docs-backup-dir) (insert n)))
    (let ((ide-docs-keep-backups 2) (old (docs-upd--slurp ide-docs-file)))
      (make-directory (file-name-directory ide-docs-candidate-file) t)
      (with-temp-file ide-docs-candidate-file (insert "NEW PAGE"))
      (let ((bak (ide-docs--install ide-docs-candidate-file)))
        (should (equal (docs-upd--slurp ide-docs-file) "NEW PAGE"))
        (should (equal (docs-upd--slurp bak) old))
        (should (= 2 (length (directory-files ide-docs-backup-dir nil "\\`index-.*\\.html\\'"))))
        (should-not (directory-files (file-name-directory ide-docs-file) nil "\\.tmp\\'"))))))

(ert-deftest docs-upd-static-count-sentence ()
  (should (equal (ide-docs--update-static "One mise.toml declares 3 CLIs and x" '(:tools (a b c d e)))
                 "One mise.toml declares 5 CLIs and x")))

;;;; Viewer ----------------------------------------------------------------------------------------------------

(ert-deftest docs-upd-urls ()
  (let ((ide-docs-file "/tmp/my docs/index.html"))
    (should (equal (ide-docs--url) "file:///tmp/my%20docs/index.html"))
    (should (equal (ide-docs--url "keys" "azdo pr/x") "file:///tmp/my%20docs/index.html?q=azdo%20pr%2Fx#keys"))))

(ert-deftest docs-upd-open-uses-browser-or-xwidget ()
  (docs-upd--with-page
    (let (shown)
      (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq shown (list 'browser u))))
                ((symbol-function 'xwidget-webkit-browse-url) (lambda (u &rest _) (setq shown (list 'xwidget u)))))
        (let ((ide-docs-viewer 'browser)) (ide-docs-open) (should (eq (car shown) 'browser)))
        (let ((ide-docs-viewer 'xwidget)) (ide-docs-open "menus") (should (eq (car shown) 'xwidget))
             (should (string-suffix-p "#menus" (cadr shown))))
        (let ((ide-docs-viewer 'browser)) (ide-docs-search "azdo") (should (string-match-p "\\?q=azdo#keys\\'" (cadr shown))))
        (let ((ide-docs-viewer 'auto)) (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil)))
                                         (ide-docs-open) (should (eq (car shown) 'browser)))))
      (let ((ide-docs-file "/nonexistent/index.html")) (should-error (ide-docs-open) :type 'user-error)))))

(ert-deftest docs-upd-user-config-never-leaks-into-facts ()
  "Keys and commands a user adds under user/ must not reach the docs or break their checks."
  (let* ((dir (file-name-as-directory (make-temp-file "udir" t)))
         (ide-user-dir dir)
         (core ide-user--core-keymap)
         (file (expand-file-name "mine.el" dir)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "(defun ide-zz-mine () \"Mine.\" (interactive))\n"))
          (load file nil t)
          ;; the real snapshot (taken at startup, before any user file) is already in place
          (unless ide-user--core-keymap (setq ide-user--core-keymap (copy-keymap ide-keys-mode-map)))
          (define-key ide-keys-mode-map (kbd "C-c x z") #'ide-zz-mine)
          (define-key ide-keys-mode-map (kbd "C-c x b") #'ignore)
          (let* ((facts (ide-docs--facts)) (keys (mapcar #'car (plist-get facts :keybinds)))
                 (data (ide-docs--parse-data (docs-upd--slurp docs-upd--page)))
                 (report (cdr (ide-docs--merge data facts))))
            (should-not (member "C-c x z" keys))
            (should-not (member "C-c x b" keys))
            (should-not (memq 'ide-zz-mine (plist-get facts :commands)))
            ;; assert about this test's own items only, so a developer's real user/ files cannot interfere
            (should-not (member (cons "C-c x z" "global") (plist-get report :kb-added)))
            (should-not (member (cons "C-c x b" "global") (plist-get report :kb-added)))
            (should-not (memq 'ide-zz-mine (plist-get report :uncovered)))
            (should (ide-docs--user-defined-p 'ide-zz-mine))
            (should-not (ide-docs--user-defined-p 'ide-docs-open))))
      (define-key ide-keys-mode-map (kbd "C-c x z") nil)
      (define-key ide-keys-mode-map (kbd "C-c x b") nil)
      (fmakunbound 'ide-zz-mine)
      (setq ide-user--core-keymap core)
      (delete-directory dir t))))

(ert-deftest docs-upd-validation-runs-without-user-files ()
  "The validator's Emacs must not load user/, whatever the user's own environment says."
  (let (env)
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args) (setq env process-environment) (make-process-stub)))
              ((symbol-function 'make-process-stub) #'ignore))
      (ignore-errors (ide-docs--validate-subprocess "/tmp/none.html" #'ignore)))
    (should (member "IDE_NO_USER=1" env))
    (should (cl-some (lambda (e) (string-prefix-p "DOCS_FILE=" e)) env))))

(ert-deftest docs-upd-agent-task-prompt ()
  (let ((p (ide-docs-agent-task-prompt)))
    (should (string-match-p (regexp-quote docs-upd--root) p))
    (should (string-match-p "make docs-update" p))
    (should (string-match-p "make docs-check" p))))

(ert-deftest docs-upd-update-with-agent-inserts-task-without-sending ()
  "Starts gptel-agent in the config root and inserts (not sends) the task."
  (let (dir sent)
    (cl-letf (((symbol-function 'gptel-agent)
               (lambda (d &rest _) (setq dir d) (switch-to-buffer (generate-new-buffer "*gptel-agent:test*"))))
              ((symbol-function 'gptel-send) (lambda (&rest _) (setq sent t))))
      (ide-docs-update-with-agent)
      (unwind-protect
          (with-current-buffer "*gptel-agent:test*"
            (should (equal dir docs-upd--root))
            (should (string-match-p "make docs-check" (buffer-string)))
            (should-not sent))
        (kill-buffer "*gptel-agent:test*")))))

(ert-deftest docs-upd-keys-are-bound ()
  (should (eq (key-binding (kbd "C-c h d")) 'ide-docs-open))
  (should (eq (key-binding (kbd "C-c h u")) 'ide-docs-update))
  (should (eq (key-binding (kbd "C-c h s")) 'ide-docs-search))
  (should (eq (key-binding (kbd "C-c h c")) 'ide-docs-for-command)))

;;;; Real validator and gptel adapter ----------------------------------------------------------

(defun docs-upd--validate-real (file)
  "Run the real subprocess validator on FILE and wait for (OK . OUTPUT)."
  (let (res)
    (ide-docs--validate-subprocess file (lambda (r) (setq res r)))
    (let ((deadline (+ (float-time) 120)))
      (while (and (null res) (< (float-time) deadline)) (accept-process-output nil 0.1)))
    res))

(ert-deftest docs-upd-real-validator-accepts-good-and-rejects-wrong-pages ()
  "The safety net itself: a fresh Emacs must pass the real page and fail a corrupted copy."
  (docs-upd--with-page
    (let ((good (docs-upd--validate-real ide-docs-file)))
      (should (car good)))
    ;; claim a binding that does not exist: M-1 -> some other command
    (let* ((html (docs-upd--slurp ide-docs-file))
           (bad (replace-regexp-in-string "\"cmd\": \"ide-toggle-project-tree\"" "\"cmd\": \"ignore\"" html t t))
           (file (expand-file-name "bad.html" (file-name-directory ide-docs-file))))
      (should-not (equal bad html))
      (let ((coding-system-for-write 'utf-8)) (write-region bad nil file nil 'silent))
      (let ((res (docs-upd--validate-real file)))
        (should-not (car res))
        (should (string-match-p "docs-keybindings-match-config" (cdr res)))))))

(ert-deftest docs-upd-gptel-adapter ()
  "The default agent function forwards system+prompt to gptel and reports replies and errors."
  (require 'gptel)
  (let (seen)
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (prompt &rest args) (setq seen (list prompt (plist-get args :system) (plist-get args :stream)))
                 (funcall (plist-get args :callback) "{\"ok\": true}" '(:status "ok")))))
      (let (out) (ide-docs--gptel-agent "SYS" "PROMPT" (lambda (r e) (setq out (list r e))))
           (should (equal out '("{\"ok\": true}" nil)))
           (should (equal seen '("PROMPT" "SYS" nil)))))
    (cl-letf (((symbol-function 'gptel-request)
               (lambda (_p &rest args) (funcall (plist-get args :callback) nil '(:status "HTTP/2 401")))))
      (let (out) (ide-docs--gptel-agent "S" "P" (lambda (r e) (setq out (list r e))))
           (should (null (car out)))
           (should (string-match-p "401" (cadr out)))))))

(ert-run-tests-batch-and-exit)

;;; docs-update-test.el ends here
