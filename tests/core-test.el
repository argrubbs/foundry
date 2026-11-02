;;; core-test.el --- Regression tests for the core modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/run.el -- "^core-"

;;; Code:

(require 'ert)
(require 'cl-lib)

;;;; 09-project: M-x grep with ripgrep

(ert-deftest core-grep-command-needs-no-null-device ()
  "rg prints file names even for one file, so grep never appends /dev/null."
  (require 'grep)
  (should (string-match-p " -H " grep-command))
  (let ((grep-host-defaults-alist nil)
        (grep-command grep-command)
        (grep-use-null-device grep-use-null-device))
    (grep-compute-defaults)
    (should-not grep-use-null-device))
  (skip-unless (executable-find "rg"))
  ;; Even when left to auto-detection, the probe finds rg prints the name.
  (let ((grep-host-defaults-alist nil)
        (grep-command grep-command)
        (grep-use-null-device 'auto-detect))
    (grep-compute-defaults)
    (should-not grep-use-null-device)))

;;;; 06-lsp / 04-corfu: `eglot-managed-mode-hook' runs on start AND stop

(ert-deftest core-eglot-hook-does-not-force-inlay-hints ()
  "Eglot enables inlay hints itself; forcing them from the hook re-enabled them on shutdown."
  (require 'eglot)
  (should-not (memq #'eglot-inlay-hints-mode (default-value 'eglot-managed-mode-hook))))

(ert-deftest core-eglot-capf-removed-when-eglot-stops ()
  "Starting Eglot wraps its capf; stopping leaves the mode's own capfs and `t'."
  (require 'eglot)
  (require 'cape)
  (with-temp-buffer
    (emacs-lisp-mode)
    (let ((before (copy-sequence completion-at-point-functions)))
      ;; Eglot on: it adds its capf locally, then runs the hook.
      (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () t)))
        (ide--eglot-capf-setup)
        (ide--eglot-capf-setup))          ; Eglot runs the hook twice on start
      (should-not (memq #'eglot-completion-at-point completion-at-point-functions))
      (should (memq #'cape-file completion-at-point-functions))
      (should (memq t completion-at-point-functions))
      (should (= (length ide--eglot-capfs) 2))
      ;; Eglot off: it removes its own capf, then runs the hook again.
      (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
      (cl-letf (((symbol-function 'eglot-managed-p) (lambda () nil)))
        (ide--eglot-capf-setup))
      (should (equal completion-at-point-functions before))
      (should-not ide--eglot-capfs))))

;;;; 00-mise: the install sentinel always calls back

(ert-deftest core-mise-callback-runs-when-post-install-fails ()
  "A signal in the post-install steps reports failure instead of hanging the batch loop."
  (let (result (called nil))
    (cl-letf (((symbol-function 'ide-mise--post-install)
               (lambda () (user-error "`mise bin-paths' failed: boom"))))
      (ide-mise--install-finished 0 (lambda (ok) (setq called t result ok))))
    (should called)
    (should-not result))
  (let (result)
    (ide-mise--install-finished 1 (lambda (ok) (setq result (list ok))))
    (should (equal result '(nil)))))

(ert-deftest core-mise-install-sentinel-calls-back ()
  "The real process sentinel reaches the callback even when post-install signals."
  (let ((done nil) (ok 'unset) (deadline (+ (float-time) 20)))
    (cl-letf (((symbol-function 'ide-mise-executable) (lambda () (executable-find "true")))
              ((symbol-function 'ide-mise--post-install)
               (lambda () (user-error "`mise bin-paths' failed: boom"))))
      (ide-mise-install (lambda (r) (setq done t ok r)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output nil 0.1)))
    (should done)
    (should-not ok)))

(ert-deftest core-mise-post-install-defers-grammars ()
  "Grammars are never compiled inside the process sentinel."
  (let ((noninteractive nil) (exec-path nil) (installed nil) (deferred nil))
    (cl-letf (((symbol-function 'ide-mise-refresh-path) #'ignore)
              ((symbol-function 'ide-treesit-install-grammars) (lambda () (setq installed t)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (_time _repeat fn &rest _) (push fn deferred))))
      (ide-mise--post-install))
    (should-not installed)
    (should (memq #'ide-mise--offer-grammars deferred))))

(ert-deftest core-mise-grammar-offer-never-prompts-over-the-minibuffer ()
  "With the minibuffer busy the offer becomes a hint instead of a prompt."
  (let ((asked nil) (installed nil))
    (cl-letf (((symbol-function 'ide-treesit-missing-grammars) (lambda () '(lua)))
              ((symbol-function 'active-minibuffer-window) (lambda () 'busy))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) (setq asked t)))
              ((symbol-function 'ide-treesit-install-grammars) (lambda () (setq installed t))))
      (ide-mise--offer-grammars))
    (should-not asked)
    (should-not installed)))

(ert-deftest core-vscode-indent-reads-real-offset-variables ()
  "vscode-indent reads Lua's width from the variable lua-ts-mode really uses."
  (require 'vsi-langs)
  (require 'lua-ts-mode)
  (should (memq 'lua-ts-indent-offset (vsi-lang-offset-vars (alist-get 'lua vsi--languages))))
  (dolist (v (vsi-lang-offset-vars (alist-get 'lua vsi--languages)))
    (when (string-prefix-p "lua-ts-" (symbol-name v)) (should (boundp v)))))

;;;; 05-treesitter: install failures are detected

(ert-deftest core-treesit-install-reports-failures ()
  "`treesit-install-language-grammar' never signals; success is judged by loading."
  (let ((treesit-language-source-alist '((ide-no-such-lang "https://invalid.example/x")))
        (calls nil))
    (cl-letf (((symbol-function 'treesit-install-language-grammar)
               (lambda (lang &rest _) (push lang calls))))
      (should (equal (ide-treesit-install-grammars) '(ide-no-such-lang))))
    (should (equal calls '(ide-no-such-lang)))))

;;;; 02-layout: window rules

(defun core-test--git (dir &rest args)
  "Run git with ARGS in DIR, failing the test on error."
  (let ((default-directory dir))
    (should (zerop (apply #'call-process "git" nil nil nil args)))))

(defun core-test--display-rule (buffer)
  "Return the first `display-buffer-alist' entry that matches BUFFER."
  (seq-find (lambda (entry) (buffer-match-p (car entry) buffer)) display-buffer-alist))

(ert-deftest core-magit-diff-buffer-docks-at-bottom ()
  "The magit rule matches what Magit really names its buffers (\"magit-diff: repo\")."
  (skip-unless (executable-find "git"))
  (require 'magit)
  (require 'magit-diff)
  (let* ((dir (file-name-as-directory (make-temp-file "coremagit" t)))
         (default-directory dir)
         (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null"
                                        "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                                        "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                                      process-environment))
         buf)
    (unwind-protect
        (progn
          (core-test--git dir "init" "-q" ".")
          (with-temp-file (expand-file-name "a.txt" dir) (insert "one\n"))
          (core-test--git dir "add" ".") (core-test--git dir "commit" "-q" "-m" "a")
          (with-temp-file (expand-file-name "a.txt" dir) (insert "two\n"))
          (core-test--git dir "commit" "-q" "-am" "b")
          (let ((magit-display-buffer-function #'display-buffer))
            (magit-diff-range "HEAD~1..HEAD"))
          (setq buf (magit-get-mode-buffer 'magit-diff-mode))
          (should buf)
          (should (string-prefix-p "magit-diff" (buffer-name buf)))
          (let ((rule (core-test--display-rule buf)))
            (should rule)
            (should (memq 'display-buffer-at-bottom (cadr rule)))
            (should (equal (alist-get 'window-height (cddr rule)) 0.40))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest core-no-dead-dap-ui-rules ()
  "dap-ui places its panes itself; `display-buffer-alist' has no rules for them."
  (should-not (seq-find (lambda (entry)
                          (and (stringp (car entry)) (string-match-p "dap-ui" (car entry))))
                        display-buffer-alist)))

;;;; init.el: custom.el loads after package configuration

(defvar core-test--custom-var 'default "Variable set by a test custom file.")

(ert-deftest core-custom-file-wins-over-package-config ()
  "Interactively, custom.el waits for elpaca to run the deferred use-package bodies."
  (let* ((custom-file (make-temp-file "core-custom" nil ".el"
                                      "(custom-set-variables '(core-test--custom-var 'user))\n"))
         (noninteractive nil)
         (elpaca-after-init-time nil)
         (elpaca-after-init-hook nil))
    (unwind-protect
        (progn
          (setq core-test--custom-var 'default)
          (ide--schedule-custom-file)
          (should (eq core-test--custom-var 'default))   ; not loaded yet
          ;; Elpaca then evaluates a package's :init/:config ...
          (setq core-test--custom-var 'package)
          ;; ... and runs its after-init hook once all queues are done.
          (run-hooks 'elpaca-after-init-hook)
          (should (eq core-test--custom-var 'user)))
      (delete-file custom-file)
      (put 'core-test--custom-var 'theme-value nil)
      (put 'core-test--custom-var 'saved-value nil))))

(ert-deftest core-batch-runs-leave-history-alone ()
  "Batch runs never save recentf, savehist or save-place over the user's files."
  (should noninteractive)
  (should-not (bound-and-true-p recentf-mode))
  (should-not (bound-and-true-p savehist-mode))
  (should-not (bound-and-true-p save-place-mode)))

;;; core-test.el ends here
