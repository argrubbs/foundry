;;; lang-lib-test.el --- Tests for the shared language helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-lib-test.el -f ert-run-tests-batch-and-exit
;; Covers modules/11-languages/00-lang-lib.el and the fenced-code languages of 01-lang-modes.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar eglot--managed-mode)

;;;; Code actions

(ert-deftest lang-lib-code-action-asks-for-the-whole-buffer ()
  ;; Interactive calls with nil bounds failed inside Eglot whenever its
  ;; suggestion overlay was live, so organize-imports-on-save did nothing.
  (with-temp-buffer
    (insert "package main\n\nimport \"os\"\n")
    (let ((eglot--managed-mode t) called executed)
      (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                ((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'eglot-code-actions)
                 (lambda (&rest args)
                   (setq called args)
                   (list '(:title "a" :kind "source.organizeImports")
                         '(:title "b" :kind "source.organizeImports" :isPreferred t))))
                ((symbol-function 'eglot-execute) (lambda (s a) (setq executed (list s a)))))
        (ide-lang-code-action "source.organizeImports")
        (should (equal called (list (point-min) (point-max) "source.organizeImports")))
        (should (equal (plist-get (cadr executed) :title) "b"))
        (should (eq (car executed) 'server))))))

(ert-deftest lang-lib-code-action-without-actions-or-on-error-is-quiet ()
  (with-temp-buffer
    (let ((eglot--managed-mode t) executed)
      (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                ((symbol-function 'eglot-current-server) (lambda () 'server))
                ((symbol-function 'eglot-execute) (lambda (&rest _) (setq executed t)))
                ((symbol-function 'eglot-code-actions) (lambda (&rest _) nil)))
        (should-not (ide-lang-code-action "source.organizeImports"))
        (should-not executed))
      (let (logged)
        (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                  ((symbol-function 'eglot-code-actions) (lambda (&rest _) (error "Boom")))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq logged (apply #'format fmt args)))))
          (should (eq (ide-lang-code-action "source.organizeImports") 'failed))
          (should (string-match-p "source.organizeImports failed: Boom" logged)))))))

;;;; Code actions from a menu

(ert-deftest lang-lib-code-action-menu-passes-real-bounds ()
  ;; The TOC command keeps Eglot's menu: interactive, with bounds Eglot can compare.
  (with-temp-buffer
    (insert "# a\n")
    (let ((eglot--managed-mode t) called)
      (cl-letf (((symbol-function 'eglot-server-capable) (lambda (&rest _) t))
                ((symbol-function 'eglot-code-actions) (lambda (&rest args) (setq called args))))
        (ide-lang-code-action "source" (point) (point) t)
        (should (equal called (list (point) (point) "source" t)))
        (ide-lang-code-action "source" nil nil t)
        (should (equal called (list (point-min) (point-max) "source" t)))))))

;;;; Linters

(defun lang-lib-test--lint (fn &optional dir)
  "Run the Flymake backend FN in a temp buffer (in DIR) and return its report args."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory (or dir temporary-file-directory)))
    (insert "x\n")
    (let (report (deadline (+ (float-time) 10)))
      (funcall fn (lambda (&rest args) (setq report (or args '(nil)))))
      (while (and (not report) (< (float-time) deadline))
        (accept-process-output nil 0.05))
      report)))

(ert-deftest lang-lib-linter-crash-is-reported-not-cleared ()
  (let ((parse (lambda (out buf) (when (string-match-p "ISSUE" out)
                                   (list (ide-lang-diag buf "t" 1 1 "warning" "issue"))))))
    (cl-flet ((lint (script &optional ok)
                (lang-lib-test--lint
                 (apply #'ide-lang-define-linter 'lang-lib-test-lint :program "sh" :safe t :stdin nil
                        :args (list "-c" script) :parse parse
                        (and ok (list :ok-codes ok))))))
      ;; clean, and issues found (exit 1): normal results
      (should (equal (lint "exit 0") '(nil)))
      (should (= 1 (length (car (lint "echo ISSUE; exit 1")))))
      ;; issues parsed always stand, whatever the status
      (should (= 1 (length (car (lint "echo ISSUE; exit 2")))))
      ;; a configuration error (exit 2) or a crash (signal) with nothing parsed: one
      ;; warning quoting the output, not an all-clear and not a disabled backend (panic)
      (let ((r (car (lint "printf 'Oops! Something went wrong! :(\n\nESLint: 10\nNo config.\n' >&2; exit 2"))))
        (should (= (length r) 1))
        (should (eq (flymake-diagnostic-type (car r)) :warning))
        (should (string-match-p "failed (exited with status 2): Oops! .* | ESLint: 10 | No config\\."
                                (flymake-diagnostic-text (car r)))))
      (let ((r (car (lint "kill -9 $$"))))
        (should (string-match-p "killed by signal 9" (flymake-diagnostic-text (car r)))))
      ;; a tool whose "issues found" status is 2
      (should (equal (lint "exit 2" '(0 2)) '(nil))))))

(ert-deftest lang-lib-linter-runs-in-its-directory ()
  (let* ((dir (file-name-as-directory (make-temp-file "lint-dir" t)))
         (seen nil)
         (fn (ide-lang-define-linter 'lang-lib-test-dir :program "sh" :safe t :stdin nil
                                     :args '("-c" "pwd") :directory (lambda () dir)
                                     :parse (lambda (out _) (setq seen (string-trim out)) nil))))
    (unwind-protect
        (progn (lang-lib-test--lint fn)
               (should (equal (file-truename (file-name-as-directory seen)) (file-truename dir))))
      (delete-directory dir t))))

;;; An untrusted unsafe linter must not even compute its command: PROGRAM, ARGS and
;;; DIRECTORY may read or run project files.
(ert-deftest lang-lib-untrusted-linter-calls-none-of-its-functions ()
  (let* ((called nil)
         (fn (ide-lang-define-linter 'lang-lib-test-unsafe
                                     :program (lambda () (push 'program called) "sh")
                                     :args (lambda (_) (push 'args called) '("-c" "true"))
                                     :directory (lambda () (push 'directory called) "/")
                                     :parse #'ignore))
         (inhibit-message t))
    (cl-letf (((symbol-function 'trusted-content-p) (lambda (&rest _) nil)))
      (should (equal (lang-lib-test--lint fn) '(nil))))
    (should-not called)))

(ert-deftest lang-lib-linter-failure-in-an-empty-buffer-is-announced-each-time ()
  ;; Flymake drops a zero-width overlay, so the echo area must carry the failure.
  (let ((fn (ide-lang-define-linter 'lang-lib-test-empty :program "sh" :safe t :stdin nil
                                    :args '("-c" "echo broken; exit 3") :parse #'ignore))
        (said 0))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (when (string-match-p "broken" (apply #'format fmt args)) (cl-incf said)))))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (let ((noninteractive nil) report)
          (dotimes (_ 2)
            (setq report nil)
            (funcall fn (lambda (&rest args) (setq report args)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not report) (< (float-time) deadline)) (accept-process-output nil 0.05))))
          (should (= said 2))
          ;; with text, once is enough: the warning is on the first line
          (insert "x\n")
          (dotimes (_ 2)
            (setq report nil)
            (funcall fn (lambda (&rest args) (setq report args)))
            (let ((deadline (+ (float-time) 10)))
              (while (and (not report) (< (float-time) deadline)) (accept-process-output nil 0.05))))
          (should (= said 2)))))))

;;; A run superseded by the next check must never report, even when that next check
;;; stops early (tool gone, untrusted, unsaved): its report function is obsolete.
(ert-deftest lang-lib-superseded-lint-run-stays-silent ()
  (let* ((available t)
         (fn (ide-lang-define-linter 'lang-lib-test-superseded :safe t :stdin nil
                                     :program (lambda () (and available "sh"))
                                     :args '("-c" "sleep 5") :parse #'ignore))
         (stale-called nil) (said nil))
    (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (insert "x\n")
        (let ((noninteractive nil))
          (funcall fn (lambda (&rest _) (setq stale-called t)))
          (let ((proc (alist-get 'lang-lib-test-superseded ide-lang--lint-procs)))
            (should (process-live-p proc))
            (setq available nil)
            (let (second)
              (funcall fn (lambda (&rest args) (setq second (or args '(nil)))))
              (should (equal second '(nil))))
            (should-not (alist-get 'lang-lib-test-superseded ide-lang--lint-procs))
            (let ((deadline (+ (float-time) 5)))
              (while (and (process-live-p proc) (< (float-time) deadline)) (accept-process-output nil 0.05)))
            (accept-process-output nil 0.1)
            (should-not (process-live-p proc))
            (should-not stale-called)
            (should-not (memq 'lang-lib-test-superseded ide-lang--lint-failed))
            (should-not (cl-some (lambda (m) (string-match-p "killed by signal" m)) said))))))))

(ert-deftest lang-lib-skipped-linter-forgets-its-failure ()
  (let* ((available t)
         (fn (ide-lang-define-linter 'lang-lib-test-forget :safe t :stdin nil
                                     :program (lambda () (and available "sh"))
                                     :args '("-c" "exit 3") :parse #'ignore)))
    (cl-letf (((symbol-function 'message) #'ignore))
      (with-temp-buffer
        (setq default-directory temporary-file-directory)
        (insert "x\n")
        (let ((noninteractive nil))
          (lang-lib-test--report fn)
          (should (memq 'lang-lib-test-forget ide-lang--lint-failed))
          (setq available nil)
          (lang-lib-test--report fn)
          (should-not (memq 'lang-lib-test-forget ide-lang--lint-failed)))))))

(defun lang-lib-test--report (fn)
  "Call backend FN in the current buffer and wait for its report."
  (let (report (deadline (+ (float-time) 10)))
    (funcall fn (lambda (&rest args) (setq report (or args '(nil)))))
    (while (and (not report) (< (float-time) deadline)) (accept-process-output nil 0.05))
    report))

(ert-deftest lang-lib-trusting-a-project-restarts-all-its-buffers ()
  (let* ((root (file-name-as-directory (make-temp-file "trust" t)))
         (other (file-name-as-directory (make-temp-file "other" t)))
         (bufs (list (generate-new-buffer "a") (generate-new-buffer "b") (generate-new-buffer "c")))
         (started nil) (trusted-content nil))
    (unwind-protect
        (progn
          (cl-loop for b in bufs
                   for f in (list (expand-file-name "a.js" root) (expand-file-name "sub/b.js" root)
                                  (expand-file-name "c.js" other))
                   do (with-current-buffer b (setq buffer-file-name f) (setq-local flymake-mode t)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'customize-save-variable) (lambda (var val &rest _) (set var val)))
                    ((symbol-function 'ide-lang-root) (lambda (&optional _) root))
                    ((symbol-function 'flymake-start) (lambda (&rest _) (push (current-buffer) started)))
                    ((symbol-function 'message) #'ignore))
            (ide-lang-trust-project))
          (should (member (abbreviate-file-name root) trusted-content))
          (should (equal (sort (mapcar #'buffer-name started) #'string<) '("a" "b"))))
      (dolist (b bufs) (with-current-buffer b (setq buffer-file-name nil)) (kill-buffer b))
      (delete-directory root t) (delete-directory other t))))

;;;; Downloads

(ert-deftest lang-lib-failed-download-removes-a-fresh-directory ()
  (let* ((parent (make-temp-file "fetch" t))
         (dir (expand-file-name "tool" parent)))
    (unwind-protect
        (dolist (kind '(zip tar))
          (make-directory dir t)
          (let ((cmd (ide-lang-fetch-extract-command "file:///nonexistent/x.archive" dir kind t)))
            (should-not (zerop (call-process shell-file-name nil nil nil shell-command-switch cmd)))
            (should-not (file-exists-p dir))))
      (delete-directory parent t))
    ;; a directory that existed before is left alone
    (should-not (string-match-p "rm -rf" (ide-lang-fetch-extract-command "u" "/d" 'zip nil)))))

;;;; Mode setup

(ert-deftest lang-lib-go-indent-uses-the-current-variable ()
  (skip-unless (treesit-language-available-p 'go))
  (with-temp-buffer
    (let ((byte-compile-warnings nil))
      (cl-letf (((symbol-function 'eglot-ensure) #'ignore))
        (go-ts-mode)))
    (should (local-variable-p 'go-ts-indent-offset))
    (should (= go-ts-indent-offset 4))))

(ert-deftest lang-lib-markdown-fence-modes-map-to-grammars ()
  (should (eq (ide-markdown--ts-mode-language 'js-ts-mode) 'javascript))
  (should (eq (ide-markdown--ts-mode-language 'typescript-ts-mode) 'typescript))
  (should (eq (ide-markdown--ts-mode-language 'tsx-ts-mode) 'tsx))
  (should (eq (ide-markdown--ts-mode-language 'bash-ts-mode) 'bash))
  (should-not (ide-markdown--ts-mode-language 'hcl-mode))
  ;; every grammar a fence needs is one 05-treesitter knows how to install
  (dolist (mode '(js-ts-mode typescript-ts-mode tsx-ts-mode bash-ts-mode python-ts-mode json-ts-mode
                  yaml-ts-mode toml-ts-mode go-ts-mode rust-ts-mode lua-ts-mode css-ts-mode
                  html-ts-mode dockerfile-ts-mode))
    (should (assq (ide-markdown--ts-mode-language mode) treesit-language-source-alist)))
  (when (and (featurep 'markdown-mode) (treesit-language-available-p 'javascript))
    (should (eq (cdr (assoc "js" markdown-code-lang-modes)) 'js-ts-mode))))

(provide 'lang-lib-test)
;;; lang-lib-test.el ends here
