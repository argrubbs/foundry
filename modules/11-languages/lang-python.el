;;; lang-python.el --- Python (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; Pyright + Ruff, debugpy via dap-python, pytest into the bottom drawer.

;;; Code:

(eval-when-compile (require 'eglot))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio")))
  ;; Ruff runs as a Flymake-adjacent second server where available.
  (when (executable-find "ruff")
    (add-to-list 'eglot-server-programs
                 '((python-mode python-ts-mode)
                   . ("pyright-langserver" "--stdio")))))

(use-package apheleia
  :demand t
  :config
  (when (executable-find "ruff")
    (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))
  (apheleia-global-mode 1))

(use-package python-pytest
  :commands (python-pytest-dispatch python-pytest-function python-pytest-file python-pytest)
  :init (setq python-pytest-executable "python -m pytest"))

(use-package envrc
  :demand t
  :config (envrc-global-mode 1))

(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon)
  :hook (python-ts-mode . ide--auto-venv))

(defun ide--auto-venv ()
  "Activate a project-local .venv or venv, unless direnv already did."
  (unless (getenv "VIRTUAL_ENV")
    (when-let* ((root (locate-dominating-file default-directory ".venv")))
      (when (fboundp 'pyvenv-activate)
        (pyvenv-activate (expand-file-name ".venv" root))))))

(defvar dap-python-debugger)
(with-eval-after-load 'dap-mode
  (require 'dap-python)
  (setq dap-python-debugger 'debugpy))

(use-package pet
  :hook (python-ts-mode . pet-mode)
  :init (setq pet-debug nil))

(provide 'lang-python)
;;; lang-python.el ends here
