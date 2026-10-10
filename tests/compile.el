;;; compile.el --- Byte-compile the config with packages on the load-path -*- lexical-binding: t; -*-

;;; Commentary:
;; Run after init.el so third-party macros resolve:
;;   emacs --init-directory . --batch -l init.el -l tests/compile.el
;; Exits non-zero on any warning or error.

;;; Code:

(require 'cl-lib)
(elpaca-wait)
(add-to-list 'load-path (expand-file-name "lisp/ansible-intelligence/" user-emacs-directory))

(let* ((root user-emacs-directory)
       (files (append (list (expand-file-name "early-init.el" root)
                            (expand-file-name "init.el" root))
                      (directory-files-recursively (expand-file-name "modules/" root) "\\.el\\'")
                      (directory-files-recursively (expand-file-name "lisp/" root) "\\.el\\'")))
       (byte-compile-error-on-warn t)
       (failed nil))
  (dolist (f files)
    (let ((elc (concat f "c"))) (when (file-exists-p elc) (delete-file elc))))
  (dolist (f files)
    ;; `byte-compile-file' returns `no-byte-compile' for files that opt out (init.el).
    (unless (byte-compile-file f) (push f failed)))
  (if failed
      (progn (message "COMPILE FAILED: %S" failed) (kill-emacs 1))
    (message "Compiled %d files cleanly." (length files))))

;;; compile.el ends here
