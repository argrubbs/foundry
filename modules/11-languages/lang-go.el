;;; lang-go.el --- Go (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; gopls, delve via dap-go, gofmt/goimports on save, go-test in the drawer.

;;; Code:

(require '00-lang-lib)

(defun ide--go-organize-imports ()
  "Organize imports through gopls."
  (ide-lang-code-action "source.organizeImports"))

(ide-lang-setup '(go-mode go-ts-mode go-mod-ts-mode)
  :server '("gopls")
  :tab-width 4
  :indent-tabs nil
  :format-on-save nil
  :config (lambda ()
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
