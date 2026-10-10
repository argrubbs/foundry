;;; 06-lsp.el --- Eglot, Flymake, Eldoc-box -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in LSP client with IDE-like hover docs and inlay hints.

;;; Code:

(eval-when-compile (require 'eglot))

(ide-builtin eglot
  :commands (eglot eglot-ensure eglot-code-actions eglot-format-buffer)
  :hook ((python-ts-mode go-ts-mode go-mod-ts-mode terraform-mode yaml-ts-mode
          bash-ts-mode dockerfile-ts-mode) . eglot-ensure)
  :init
  (setq eglot-events-buffer-config '(:size 0 :format short)
        eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-extend-to-xref t
        eglot-report-progress nil)
  :config
  (add-hook 'eglot-managed-mode-hook #'eglot-inlay-hints-mode)
  (setq-default eglot-workspace-configuration
                '(:gopls (:staticcheck t :usePlaceholders t
                          :analyses (:unusedparams t :shadow t)
                          :hints (:parameterNames t :assignVariableTypes t))
                  :pyright (:disableOrganizeImports t)
                  :python (:analysis (:typeCheckingMode "standard")))))

(ide-builtin flymake
  :hook ((prog-mode . flymake-mode))
  :init (setq flymake-no-changes-timeout 0.5
              flymake-fringe-indicator-position 'right-fringe))

(use-package eldoc-box
  :commands (eldoc-box-help-at-point eldoc-box-hover-mode)
  :init (setq eldoc-box-max-pixel-width 700
              eldoc-box-only-multi-line t))

(ide-builtin eldoc
  :init (setq eldoc-idle-delay 0.3
              eldoc-echo-area-use-multiline-p 1))

;; Format-on-save through the language server when it supports it.
(defvar-local ide-eglot-format-on-save t
  "When nil, Eglot does not format this buffer on save (another formatter owns it).")

(defun ide--eglot-format-on-save ()
  "Format the buffer with Eglot before saving."
  (when (and ide-eglot-format-on-save
             (bound-and-true-p eglot--managed-mode)
             (eglot-server-capable :documentFormattingProvider))
    (eglot-format-buffer)))
(add-hook 'eglot-managed-mode-hook
          (lambda () (add-hook 'before-save-hook #'ide--eglot-format-on-save nil t)))

(provide '06-lsp)
;;; 06-lsp.el ends here
