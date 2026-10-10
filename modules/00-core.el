;;; 00-core.el --- Better defaults and state isolation -*- lexical-binding: t; -*-

;;; Commentary:
;; Sane defaults; every piece of runtime state is redirected into the sandbox.

;;; Code:

(defvar ide-cache-dir (expand-file-name ".cache/" user-emacs-directory))
(defvar ide-var-dir (expand-file-name "var/" user-emacs-directory))
(dolist (d (list ide-cache-dir ide-var-dir
                 (expand-file-name "backups/" ide-cache-dir)
                 (expand-file-name "auto-save/" ide-cache-dir)
                 (expand-file-name "locks/" ide-cache-dir)))
  (make-directory d t))

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" ide-cache-dir)))
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" ide-cache-dir) t))
      auto-save-list-file-prefix (expand-file-name "auto-save/sessions/" ide-cache-dir)
      lock-file-name-transforms `((".*" ,(expand-file-name "locks/" ide-cache-dir) t))
      backup-by-copying t
      version-control t
      delete-old-versions t
      create-lockfiles t
      custom-file (expand-file-name "custom.el" user-emacs-directory)
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

(setq-default indent-tabs-mode nil
              fill-column 100)
(setq ring-bell-function #'ignore
      use-short-answers t
      confirm-kill-processes nil
      sentence-end-double-space nil
      require-final-newline t
      scroll-conservatively 101
      scroll-margin 3
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil
      load-prefer-newer t
      auto-revert-verbose nil
      global-auto-revert-non-file-buffers t)

(electric-pair-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(dolist (h '(treemacs-mode-hook ghostel-mode-hook eshell-mode-hook shell-mode-hook
             term-mode-hook magit-mode-hook org-mode-hook imenu-list-major-mode-hook))
  (add-hook h (lambda () (display-line-numbers-mode -1))))

(with-eval-after-load 'package
  ;; Never let package.el write to ~/.emacs.d.
  (setq package-user-dir (expand-file-name "elpa/" ide-cache-dir)))

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

(provide '00-core)
;;; 00-core.el ends here
