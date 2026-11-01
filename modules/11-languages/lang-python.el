;;; lang-python.el --- Python (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; Pyright + Ruff, debugpy via dap-python, pytest into the bottom drawer.
;; Pyright has no formatter, so Ruff formats on save through Apheleia.

;;; Code:

(require '00-lang-lib)

(defvar apheleia-mode-alist)

(defun ide-python--format-on-save ()
  "Who formats a Python buffer on save: Apheleia (Ruff) when ruff is installed.
Without it nothing would: pyright-langserver has no formatting."
  (if (executable-find "ruff") t 'off))

(ide-lang-setup '(python-mode python-ts-mode)
  :server '("pyright-langserver" "--stdio")
  :format-on-save #'ide-python--format-on-save)

(defun ide-python--test-file-p (file)
  "Non-nil when FILE is a pytest test module by name: test_*.py or *_test.py."
  (string-match-p "\\`\\(?:test_.*\\|.*_test\\)\\.py\\'" (file-name-nondirectory file)))

(defun ide-python--run-command ()
  "S-<f10>: run the file, or pytest for a test file."
  (when (and buffer-file-name (derived-mode-p 'python-base-mode))
    (format (if (ide-python--test-file-p buffer-file-name) "python -m pytest %s" "python %s")
            (shell-quote-argument buffer-file-name))))

(ide-lang-register '(python-mode python-ts-mode) :run #'ide-python--run-command)

;; Unconditional: ruff may join PATH (mise) after Apheleia loads, and Apheleia looks the
;; executable up when it formats.
(with-eval-after-load 'apheleia
  (dolist (mode '(python-mode python-ts-mode))
    (setf (alist-get mode apheleia-mode-alist) '(ruff-isort ruff))))

(defvar dap-python-debugger)
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(provide 'lang-python)
;;; lang-python.el ends here
