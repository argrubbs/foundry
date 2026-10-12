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

(use-package python-pytest
  :commands (python-pytest-dispatch python-pytest-function python-pytest-file python-pytest)
  :init (setq python-pytest-executable "python -m pytest"))

(use-package envrc
  :hook (python-ts-mode . envrc-mode))

(defvar dap-python-debugger)
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(use-package pet
  :hook (python-ts-mode . pet-mode)
  :init (setq pet-debug nil))

(provide 'lang-python)
;;; lang-python.el ends here
