;;; lang-python.el --- Python (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; Pyright + Ruff, debugpy via dap-python, pytest into the bottom drawer.

;;; Code:

(require '00-lang-lib)

(defvar apheleia-mode-alist)

(ide-lang-setup '(python-mode python-ts-mode)
  :server '("pyright-langserver" "--stdio")
  :format-on-save nil)

(with-eval-after-load 'apheleia
  (when (executable-find "ruff")
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))))

(defvar dap-python-debugger)
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(provide 'lang-python)
;;; lang-python.el ends here
