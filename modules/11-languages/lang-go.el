;;; lang-go.el --- Go (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; gopls, delve via dap-go, gofmt/goimports on save, go-test in the drawer.

;;; Code:

(eval-when-compile (require 'eglot))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode go-mod-ts-mode) . ("gopls"))))

(defun ide--go-organize-imports ()
  "Organize imports through gopls, then format."
  (when (and (bound-and-true-p eglot--managed-mode)
             (eglot-server-capable :codeActionProvider))
    (ignore-errors
      (eglot-code-actions nil nil "source.organizeImports" t))))

(add-hook 'go-ts-mode-hook
          (lambda ()
            (setq-local tab-width 4 indent-tabs-mode t go-ts-mode-indent-offset 4)
            (add-hook 'before-save-hook #'ide--go-organize-imports -10 t)))

(use-package gotest
  :commands (go-test-current-test go-test-current-file go-test-current-project
             go-test-current-benchmark))

(use-package go-gen-test :commands go-gen-test-dwim)

(defvar dap-go-debug-program)
(with-eval-after-load 'dap-mode
  (let ((inhibit-message t)) (require 'dap-go))
  (when (executable-find "dlv") (setq dap-go-debug-program (executable-find "dlv"))))

(provide 'lang-go)
;;; lang-go.el ends here
