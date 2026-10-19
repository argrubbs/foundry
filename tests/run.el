;;; run.el --- Run every ERT test in one Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;   emacs --init-directory . --batch -l init.el -l tests/run.el
;; Pass a selector as the last argument to run a subset, e.g. `-- "^vsi-"'.

;;; Code:

(require 'ert)
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (f (directory-files dir t "-tests?\\.el\\'"))
    (load f nil t)))
(ert-run-tests-batch-and-exit (car (last command-line-args-left)))

;;; run.el ends here
