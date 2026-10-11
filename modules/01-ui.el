;;; 01-ui.el --- Theme, modeline, breadcrumbs, folding -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual ergonomics.

;;; Code:

(declare-function ide-juice-initial-theme "17-juice")

(use-package nerd-icons :demand t)

(use-package doom-themes
  :demand t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  ;; 17-juice picks the theme (doom-ember by default); IDE_NO_JUICE=1 keeps doom-one.
  (load-theme (if (fboundp 'ide-juice-initial-theme) (ide-juice-initial-theme) 'doom-one) t)
  (doom-themes-org-config))

;; Clear contrast between active and inactive windows.
(use-package solaire-mode
  :demand t
  :config (solaire-global-mode 1))

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-height 26
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 24
        doom-modeline-env-version t
        doom-modeline-project-detection 'project
        doom-modeline-icon t)
  :config (doom-modeline-mode 1))

(use-package breadcrumb
  :demand t
  :config
  (setq breadcrumb-imenu-crumb-separator " › "
        breadcrumb-project-crumb-separator " › ")
  (breadcrumb-mode 1))

(use-package treesit-fold
  :hook ((prog-mode . treesit-fold-mode)
         (prog-mode . treesit-fold-indicators-mode))
  :init (setq treesit-fold-indicators-fringe 'left-fringe))

(use-package diff-hl
  :demand t
  :hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (setq diff-hl-flydiff-delay 0.3)
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1)
  (unless (display-graphic-p) (diff-hl-margin-mode 1)))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(set-face-attribute 'default nil :height 120)

;; Frame geometry: 1370x1011 pixels for graphical sessions.
(setq frame-resize-pixelwise t)
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (set-frame-size (selected-frame) 1370 1011 t))))

;; Branding.  Deliberately no `foundry-' symbols or feature: that namespace belongs to
;; the unrelated foundry.el package.
(setq frame-title-format '("Foundry — " (:eval (if buffer-file-name (abbreviate-file-name buffer-file-name) "%b")))
      icon-title-format frame-title-format
      initial-scratch-message ";; Foundry — a full-featured Emacs IDE\n;; C-c h d  documentation    C-c w 8  Services & DevOps    C-c a M  AI model\n\n")

;; Startup re-evaluates `scroll-bar-mode' after early-init; assert it again.
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; which-key (built in since Emacs 30): show the continuations of a pending prefix.
(use-package which-key
  :ensure nil
  :demand t
  :custom
  (which-key-idle-delay 0.6)
  (which-key-idle-secondary-delay 0.05)
  (which-key-sort-order #'which-key-key-order-alpha)
  :config
  (which-key-add-key-based-replacements
    "C-c a" "ai"
    "C-c d" "devops"
    "C-c h" "help/docs"
    "C-c l" "code (lsp)"
    "C-c o" "org"
    "C-c u" "user config"
    "C-c w" "tool windows")
  (which-key-mode 1))

(provide '01-ui)
;;; 01-ui.el ends here
