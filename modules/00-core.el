;;; 00-core.el --- Better defaults and state isolation -*- lexical-binding: t; -*-

;;; Commentary:
;; Sane defaults; every piece of runtime state is redirected into the sandbox.

;;; Code:

(defvar ide-cache-dir (expand-file-name ".cache/" user-emacs-directory))
(defvar ide-var-dir (expand-file-name "var/" user-emacs-directory))
(make-directory ide-cache-dir t)
(make-directory ide-var-dir t)

(prefer-coding-system 'utf-8)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" ide-cache-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" ide-cache-dir) t))
      auto-save-list-file-prefix (expand-file-name "auto-save/sessions/" ide-cache-dir)
      lock-file-name-transforms `((".*" ,(expand-file-name "locks/" ide-cache-dir) t))
      backup-by-copying t
      version-control t
      delete-old-versions t
      transient-levels-file (expand-file-name "transient/levels.el" ide-var-dir)
      transient-values-file (expand-file-name "transient/values.el" ide-var-dir)
      transient-history-file (expand-file-name "transient/history.el" ide-var-dir)
      url-cache-directory (expand-file-name "url/" ide-cache-dir)
      recentf-save-file (expand-file-name "recentf" ide-var-dir)
      savehist-file (expand-file-name "savehist" ide-var-dir)
      save-place-file (expand-file-name "saveplace" ide-var-dir)
      project-list-file (expand-file-name "projects" ide-var-dir)
      bookmark-default-file (expand-file-name "bookmarks" ide-var-dir)
      eshell-directory-name (expand-file-name "eshell/" ide-var-dir)
      tramp-persistency-file-name (expand-file-name "tramp" ide-var-dir)
      treesit-extra-load-path (list (expand-file-name "tree-sitter/" ide-var-dir)))

(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/ide-align/" user-emacs-directory))

(setq-default indent-tabs-mode nil
              fill-column 100
              standard-indent 2
              tab-width 2)

;; Indentation policy: ALWAYS use spaces, NEVER tabs.
;; 2 spaces default, unless the language's own style guide says otherwise
;; (e.g. Python and Rust keep 4). Modes without an offset variable take it from `tab-width'.
(add-hook 'prog-mode-hook (lambda () (unless (derived-mode-p 'makefile-mode) (setq indent-tabs-mode nil))))
(add-hook 'text-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'conf-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq sh-basic-offset 2
      json-ts-mode-indent-offset 2
      typescript-ts-mode-indent-offset 2
      js-indent-level 2
      css-indent-offset 2
      html-ts-indent-offset 2
      lua-ts-mode-indent-offset 2
      dockerfile-ts-mode-indent-offset 2
      yaml-indent-offset 2
      terraform-indent-level 2
      hcl-indent-level 2)

(require 'ide-align nil t)
(when (fboundp 'global-ide-align-mode)
  (global-ide-align-mode 1))

(setq ring-bell-function #'ignore
      use-short-answers t
      confirm-kill-processes nil
      sentence-end-double-space nil
      require-final-newline t
      scroll-conservatively 101
      scroll-margin 3
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;;; Bounded subprocesses
(defun ide-run-with-timeout (seconds program &rest args)
  "Run PROGRAM with ARGS, waiting at most SECONDS.
Return (CODE STDOUT STDERR).  CODE is the exit status, `timeout' if the
process had to be killed, or nil if PROGRAM could not be started."
  (let* ((out (generate-new-buffer " *ide-out*"))
         (err (generate-new-buffer " *ide-err*"))
         (proc (ignore-errors
                 (make-process :name "ide-run" :buffer out :stderr err
                               :command (cons program args)
                               :connection-type 'pipe :noquery t
                               ;; The default sentinel appends "Process ... exited" to the output.
                               :sentinel #'ignore)))
         (deadline (+ (float-time) seconds))
         (code nil))
    (when proc
      (when-let* ((errproc (get-buffer-process err)))
        (set-process-sentinel errproc #'ignore))
      (while (and (process-live-p proc) (< (float-time) deadline))
        (accept-process-output proc 0.1))
      (if (process-live-p proc)
          (progn (delete-process proc) (setq code 'timeout))
        (accept-process-output nil 0.05)
        (setq code (process-exit-status proc))))
    (prog1 (list code
                 (with-current-buffer out (buffer-string))
                 (with-current-buffer err (buffer-string)))
      (kill-buffer out)
      (kill-buffer err))))

(defun ide-env-flag-p (var)
  "Non-nil when environment variable VAR is set to a non-empty string."
  (let ((v (getenv var))) (and v (not (string-empty-p v)))))

(defun ide-slurp (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(provide '00-core)
;;; 00-core.el ends here
