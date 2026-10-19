;;; lang-go.el --- Go (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; gopls, delve via dap-go, gofmt/goimports on save, go-test in the drawer.

;;; Code:

(require '00-lang-lib)

(defun ide--go-organize-imports ()
  "Organize imports through gopls."
  (ide-lang-code-action "source.organizeImports"))

;; Eglot ships the gopls entry for the Go modes.
(ide-lang-setup '(go-mode go-ts-mode go-mod-ts-mode)
  :tab-width 4
  :indent-tabs nil
  :format-on-save nil
  :config (lambda ()
            (add-hook 'before-save-hook #'ide--go-organize-imports -10 t)))

(defvar dap-go-debug-program)
(with-eval-after-load 'dap-mode
  (let ((inhibit-message t)) (require 'dap-go))
  (when (executable-find "dlv") (setq dap-go-debug-program (executable-find "dlv"))))

(provide 'lang-go)
;;; lang-go.el ends here
