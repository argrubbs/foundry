;;; lang-python.el --- Python (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; Pyright + Ruff, debugpy via dap-python, pytest into the bottom drawer.

;;; Code:

(require '00-lang-lib)

(defvar apheleia-mode-alist)

(ide-lang-setup '(python-mode python-ts-mode)
  :server '("pyright-langserver" "--stdio")
  :format-on-save nil)

(defun ide-python--run-command ()
  "S-<f10>: run the file, or pytest for a test file."
  (when (and buffer-file-name (derived-mode-p 'python-base-mode))
    (format (if (string-match-p "test_\\|_test\\.py" buffer-file-name) "python -m pytest %s" "python %s")
            (shell-quote-argument buffer-file-name))))

(ide-lang-register '(python-mode python-ts-mode) :run #'ide-python--run-command)

(with-eval-after-load 'apheleia
  (when (executable-find "ruff")
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff))))

(defvar dap-python-debugger)
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(provide 'lang-python)
;;; lang-python.el ends here
