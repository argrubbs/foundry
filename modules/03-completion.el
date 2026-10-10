;;; 03-completion.el --- Vertico, Orderless, Consult, Marginalia, Embark -*- lexical-binding: t; -*-

;;; Commentary:
;; IDE-style search popups.

;;; Code:

(use-package vertico
  :demand t
  :config
  (setq vertico-count 14 vertico-cycle t)
  (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (eglot (styles orderless))
                                        (eglot-capf (styles orderless)))
        orderless-matching-styles '(orderless-literal orderless-regexp orderless-flex)))

(use-package marginalia
  :demand t
  :config (marginalia-mode 1))

(use-package nerd-icons-completion
  :demand t
  :after marginalia
  :config (nerd-icons-completion-mode 1))

(use-package consult
  :demand t
  :init
  (setq consult-narrow-key "<"
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-project-function
        (lambda (_may-prompt)
          (when-let* ((p (project-current))) (project-root p))))
  (consult-customize consult-ripgrep consult-grep :preview-key "M-."))

(use-package embark
  :demand t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :init (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :demand t
  :config (setq wgrep-auto-save-buffer t))

(provide '03-completion)
;;; 03-completion.el ends here
