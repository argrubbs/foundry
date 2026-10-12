;;; 17-juice.el --- Juice: visual polish and themes -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual polish for Foundry: themes, typography, subtle line highlights,
;; indent guides, cursor pulsing, smooth scrolling and ligatures.
;;
;;   C-c j t   pick a theme (live preview via consult-theme)
;;
;; Escape hatch: start with IDE_NO_JUICE=1 (`./launch.sh --plain`) for the
;; unadorned experience.
;; The house theme `doom-ember' lives in lisp/themes/.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function ide-env-flag-p "00-core")
(declare-function pixel-scroll-precision-mode "pixel-scroll")
(declare-function doom-themes-visual-bell-config "doom-themes-ext-visual-bell")
(declare-function consult-theme "consult")
(declare-function ligature-set-ligatures "ligature")
(declare-function global-ligature-mode "ligature")
(declare-function pulsar-global-mode "pulsar")
(declare-function indent-bars-mode "indent-bars")
(declare-function global-hl-todo-mode "hl-todo")

(defvar ide-keys-mode-map)
(defvar consult-themes)
(defvar doom-modeline-hud)
(defvar doom-modeline-bar-width)
(defvar doom-modeline-height)
(defvar pulsar-delay)
(defvar pulsar-iterations)
(defvar pulsar-pulse-on-window-change)

(defgroup ide-juice nil
  "Visual polish for Foundry."
  :group 'faces)

(defcustom ide-juice-font-family "JetBrainsMono Nerd Font"
  "Font family for code buffers (used only when installed)."
  :type 'string)

(defcustom ide-juice-line-spacing 0.12
  "Extra line spacing (fraction of the line height)."
  :type 'number)

(defcustom ide-juice-frame-padding 14
  "Pixels of padding around the window area."
  :type 'integer)

(defcustom ide-juice-default-theme 'doom-ember
  "Theme used until you pick another with `ide-juice-pick-theme'."
  :type 'symbol)

(defcustom ide-juice-themes
  '(doom-ember doom-tokyo-night doom-outrun-electric doom-moonlight doom-ayu-mirage
    doom-dracula doom-palenight doom-nord-aurora doom-gruvbox doom-old-hope
    doom-one doom-one-light)
  "Themes offered by `ide-juice-pick-theme'."
  :type '(repeat symbol))

(defun ide-juice--plain-p ()
  "Non-nil when $IDE_NO_JUICE asks for the plain, unpolished experience."
  (ide-env-flag-p "IDE_NO_JUICE"))

;;;; Theme ----------------------------------------------------------------------

(defun ide-juice--theme-path ()
  (add-to-list 'custom-theme-load-path
               (expand-file-name "lisp/themes/" user-emacs-directory)))

(defun ide-juice--load-theme (theme)
  "Make THEME the only enabled theme; non-nil on success."
  (ide-juice--theme-path)
  (when (require 'doom-themes nil t)
    (unless (eq (car custom-enabled-themes) theme)
      (mapc #'disable-theme custom-enabled-themes)
      (load-theme theme t))
    t))

(defun ide-juice-initial-theme ()
  "Theme for startup: the juice theme when wanted, else plain doom-one.
Called by 01-ui so the first themed frame is already the right one."
  (ide-juice--theme-path)
  (if (ide-juice--plain-p) 'doom-one ide-juice-default-theme))

;;;###autoload
(defun ide-juice-pick-theme ()
  "Choose a theme with live preview."
  (interactive)
  (ide-juice--theme-path)
  (let ((consult-themes ide-juice-themes))
    (call-interactively #'consult-theme)))

;;;; Visual polish --------------------------------------------------------------

(unless (ide-juice--plain-p)
  ;; Theme
  (ide-juice--load-theme ide-juice-default-theme)

  ;; Font & spacing
  (when (and (display-graphic-p) (find-font (font-spec :family ide-juice-font-family)))
    (set-face-attribute 'default nil :family ide-juice-font-family)
    (setq-default line-spacing ide-juice-line-spacing))

  ;; Frame padding and thin window dividers
  (when (display-graphic-p)
    (modify-all-frames-parameters `((internal-border-width . ,ide-juice-frame-padding)))
    (setf (alist-get 'internal-border-width default-frame-alist) ide-juice-frame-padding)
    (setq window-divider-default-places t
          window-divider-default-right-width 1
          window-divider-default-bottom-width 1)
    (window-divider-mode 1))

  ;; Modeline
  (setq doom-modeline-hud t
        doom-modeline-bar-width 5
        doom-modeline-height 30)

  ;; Cursorline
  (setq global-hl-line-modes '(prog-mode text-mode conf-mode dired-mode))
  (global-hl-line-mode 1)

  ;; Pixel scrolling
  (when (display-graphic-p)
    (pixel-scroll-precision-mode 1))

  ;; Visual bell
  (use-package doom-themes-ext-visual-bell
    :ensure nil
    :config (doom-themes-visual-bell-config))

  ;; Pulse
  (use-package pulsar
    :init
    (setq pulsar-delay 0.035
          pulsar-iterations 14
          pulsar-pulse-on-window-change t)
    :config
    (pulsar-global-mode 1))

  ;; Indent bars
  (use-package indent-bars
    :hook ((prog-mode yaml-mode) . indent-bars-mode)
    :custom
    (indent-bars-treesit-support t)
    (indent-bars-width-frac 0.15)
    (indent-bars-pad-frac 0.1)
    (indent-bars-prefer-character (not (eq window-system 'x)))
    (indent-bars-color '(highlight :face-bg t :blend 0.2))
    (indent-bars-highlight-current-depth '(:blend 0.7)))

  ;; Todo highlighting
  (use-package hl-todo
    :config (global-hl-todo-mode 1))

  ;; Programming ligatures
  (use-package ligature
    :config
    (ligature-set-ligatures
     'prog-mode
     '("-->" "->" "->>" "=>" "==>" ">=" "<=" "==" "!=" "..." "/*" "*/" "<!--" "-->"))
    (when (display-graphic-p)
      (global-ligature-mode 1))))

;;;; Keys -----------------------------------------------------------------------

(with-eval-after-load '10-keybindings
  (define-key ide-keys-mode-map (kbd "C-c j t") #'ide-juice-pick-theme))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c j" "juice"))

(provide '17-juice)
;;; 17-juice.el ends here
