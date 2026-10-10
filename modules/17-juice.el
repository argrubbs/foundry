;;; 17-juice.el --- Juice: theme, motion and a forge-lit start screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Visual polish that is easy to back out of.  Everything is an "effect" that
;; can be switched on and off at runtime and the choice is remembered in
;; var/juice.eld:
;;
;;   C-c j j   toggle one effect          C-c j t   pick a theme (live preview)
;;   C-c j m   master switch (all off)    C-c j n   next theme
;;   C-c j s   start screen               C-c j r   forget choices, use defaults
;;
;; Escape hatches, from gentlest to strongest: toggle one effect; `C-c j m';
;; start with IDE_NO_JUICE=1 (`./launch.sh --plain'); `git switch main'.
;; The house theme `doom-ember' lives in lisp/themes/.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'color)

(defvar ide-var-dir)
(defvar ide-keys-mode-map)
(defvar elpaca-after-init-hook)
(defvar recentf-list)
(defvar doom-modeline-hud)
(defvar doom-modeline-bar-width)
(defvar doom-modeline-height)
(defvar pulsar-highlight-face)
(defvar pulsar-region-face)
(defvar pulsar-region-change-face)
(defvar pulsar-window-change-face)
(defvar pulsar-delay)
(defvar pulsar-iterations)
(defvar pulsar-face)
(defvar pulsar-pulse-on-window-change)
(defvar indent-bars-treesit-support)
(defvar indent-bars-width-frac)
(defvar indent-bars-pad-frac)
(defvar indent-bars-color)
(defvar indent-bars-highlight-current-depth)
(defvar indent-bars-prefer-character)
(defvar consult-themes)
(declare-function pixel-scroll-precision-mode "pixel-scroll")
(declare-function pulsar-global-mode "pulsar")
(declare-function indent-bars-mode "indent-bars")
(declare-function global-hl-todo-mode "hl-todo")
(declare-function global-ligature-mode "ligature")
(declare-function ligature-set-ligatures "ligature")
(declare-function doom-themes-visual-bell-config "doom-themes-ext-visual-bell")
(declare-function doom-modeline-refresh-bars "doom-modeline-segments")
(declare-function consult-theme "consult")
(declare-function nerd-icons-icon-for-file "nerd-icons")
(declare-function project-known-project-roots "project")

(defgroup ide-juice nil
  "Visual polish for Foundry."
  :group 'faces)

(defcustom ide-juice-font-family "JetBrainsMono Nerd Font"
  "Font family for the `font' effect (used only when installed)."
  :type 'string)

(defcustom ide-juice-line-spacing 0.12
  "Extra line spacing (fraction of the line height) for the `font' effect."
  :type 'number)

(defcustom ide-juice-frame-padding 14
  "Pixels of padding around the window area for the `frame' effect."
  :type 'integer)

(defcustom ide-juice-opacity 92
  "Background opacity (0-100) for the `transparency' effect."
  :type 'integer)

(defcustom ide-juice-default-theme 'doom-ember
  "Theme used until you pick another with `ide-juice-pick-theme'."
  :type 'symbol)

(defcustom ide-juice-themes
  '(doom-ember doom-tokyo-night doom-outrun-electric doom-moonlight doom-ayu-mirage
    doom-dracula doom-palenight doom-nord-aurora doom-gruvbox doom-old-hope
    doom-one doom-one-light)
  "Themes offered by `ide-juice-pick-theme' and `ide-juice-next-theme'."
  :type '(repeat symbol))

(defcustom ide-juice-indent-hooks '(prog-mode-hook yaml-mode-hook)
  "Mode hooks that get indent guides."
  :type '(repeat variable))

(defcustom ide-juice-line-highlight-hooks
  '(prog-mode-hook text-mode-hook conf-mode-hook dired-mode-hook)
  "Mode hooks that get a highlighted current line."
  :type '(repeat variable))

;;;; State -------------------------------------------------------------------

(defvar ide-juice-state-file (expand-file-name "juice.eld" ide-var-dir)
  "Where the effect choices are remembered.")

(defvar ide-juice--state :unread
  "Plist (:enabled BOOL :effects ALIST :theme SYMBOL); :unread until first use.")

(defvar ide-juice--effects nil
  "List of (NAME . PLIST) in definition order.  PLIST keys: :doc :on :off :default :gui.")

(defvar ide-juice--active nil "Names of the effects that are currently applied.")
(defvar ide-juice--stash nil "Alist of original values, restored when an effect turns off.")

(defun ide-juice--plain-p ()
  "Non-nil when $IDE_NO_JUICE asks for the plain, unpolished experience."
  (let ((v (getenv "IDE_NO_JUICE"))) (and v (not (string-empty-p v)))))

(defun ide-juice--state ()
  "Return the state plist, reading the file on first use."
  (when (eq ide-juice--state :unread)
    (setq ide-juice--state
          (or (ignore-errors
                (when (file-readable-p ide-juice-state-file)
                  (with-temp-buffer
                    (insert-file-contents ide-juice-state-file)
                    (read (current-buffer)))))
              nil)))
  ide-juice--state)

(defun ide-juice--save ()
  "Write the state plist to disk."
  (make-directory (file-name-directory ide-juice-state-file) t)
  (with-temp-file ide-juice-state-file
    (let ((print-length nil) (print-level nil))
      (prin1 (ide-juice--state) (current-buffer))
      (insert "\n"))))

(defun ide-juice--set (key value)
  "Remember KEY = VALUE in the state and save it."
  (setq ide-juice--state (plist-put (ide-juice--state) key value))
  (ide-juice--save))

(defun ide-juice--master-p ()
  "Non-nil when juice as a whole is wanted."
  (and (not (ide-juice--plain-p))
       (let ((s (ide-juice--state)))
         (if (plist-member s :enabled) (plist-get s :enabled) t))))

(defun ide-juice--effect (name) (cdr (assq name ide-juice--effects)))

(defun ide-juice--wanted-p (name)
  "Non-nil when effect NAME is wanted: the user's choice, else its default."
  (let ((choice (assq name (plist-get (ide-juice--state) :effects))))
    (if choice (cdr choice) (plist-get (ide-juice--effect name) :default))))

(defun ide-juice--stash (key value)
  "Remember the original VALUE under KEY, once."
  (unless (assq key ide-juice--stash) (push (cons key value) ide-juice--stash)))

(defun ide-juice--unstash (key &optional default)
  "Return, and forget, the value stashed under KEY (DEFAULT when absent)."
  (let ((cell (assq key ide-juice--stash)))
    (setq ide-juice--stash (delq cell ide-juice--stash))
    (if cell (cdr cell) default)))

;;;; Effect registry ----------------------------------------------------------

(defun ide-juice--define (name doc &rest props)
  "Register effect NAME.  PROPS: :on FN (non-nil when applied), :off FN,
:default BOOL (default t) and :gui BOOL (graphical frames only)."
  (setq ide-juice--effects
        (append (assq-delete-all name ide-juice--effects)
                (list (cons name (append (list :doc doc) props
                                         (unless (plist-member props :default) '(:default t))))))))

(defun ide-juice--apply (name)
  "Bring effect NAME in line with the master switch and its choice."
  (let* ((e (ide-juice--effect name))
         (want (and (ide-juice--master-p) (ide-juice--wanted-p name)
                    (or (not (plist-get e :gui)) (display-graphic-p))))
         (active (memq name ide-juice--active)))
    (condition-case err
        (cond ((and want (not active))
               (when (funcall (plist-get e :on)) (push name ide-juice--active)))
              ((and (not want) active)
               (when-let* ((off (plist-get e :off))) (funcall off))
               (setq ide-juice--active (delq name ide-juice--active))))
      (error
       (display-warning 'ide-juice (format "Effect %s failed: %s" name (error-message-string err)))))))

(defun ide-juice-apply ()
  "Apply every effect according to the current settings."
  (interactive)
  (dolist (e ide-juice--effects) (ide-juice--apply (car e)))
  (force-mode-line-update t))

;;;###autoload
(define-minor-mode ide-juice-mode
  "Master switch for the juice effects; turning it off undoes them all."
  :global t :group 'ide-juice
  (unless (ide-juice--plain-p)
    (ide-juice--set :enabled ide-juice-mode))
  (ide-juice-apply))

;;;###autoload
(defun ide-juice-toggle (name)
  "Toggle the juice effect NAME and remember the choice."
  (interactive
   (list (intern (completing-read
                  "Toggle effect: "
                  (lambda (str pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          (display-sort-function . identity)
                          (annotation-function
                           . ,(lambda (s)
                                (let ((n (intern-soft s)))
                                  (format "  %-4s %s" (if (memq n ide-juice--active) "on" "off")
                                          (plist-get (ide-juice--effect n) :doc))))))
                      (complete-with-action action (mapcar (lambda (e) (symbol-name (car e)))
                                                           ide-juice--effects)
                                            str pred)))
                  nil t))))
  (let* ((now (ide-juice--wanted-p name))
         (effects (assq-delete-all name (copy-sequence (plist-get (ide-juice--state) :effects)))))
    (ide-juice--set :effects (cons (cons name (not now)) effects))
    (unless (ide-juice--master-p) (ide-juice-mode 1))
    (ide-juice--apply name)
    (message "juice: %s %s" name (if (memq name ide-juice--active) "on" "off"))))

;;;###autoload
(defun ide-juice-reset ()
  "Forget every remembered choice (effects and theme) and apply the defaults."
  (interactive)
  (when (file-exists-p ide-juice-state-file) (delete-file ide-juice-state-file))
  (setq ide-juice--state nil)
  (ide-juice-apply)
  (message "juice: back to defaults"))

;;;; Theme ----------------------------------------------------------------------

(defun ide-juice-theme ()
  "The remembered theme, else `ide-juice-default-theme'."
  (or (plist-get (ide-juice--state) :theme) ide-juice-default-theme))

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
  (if (and (ide-juice--master-p) (ide-juice--wanted-p 'theme)) (ide-juice-theme) 'doom-one))

;;;###autoload
(defun ide-juice-pick-theme ()
  "Choose a theme with live preview and remember it."
  (interactive)
  (ide-juice--theme-path)
  (let ((old (car custom-enabled-themes)))
    (if (fboundp 'consult-theme)
        (let ((consult-themes ide-juice-themes))
          (call-interactively #'consult-theme))
      (ide-juice--load-theme (intern (completing-read "Theme: " ide-juice-themes nil t))))
    (let ((new (car custom-enabled-themes)))
      (when (and new (not (eq new old)))
        (ide-juice--set :theme new)
        (message "juice: theme %s" new)))))

;;;###autoload
(defun ide-juice-next-theme ()
  "Cycle to the next theme in `ide-juice-themes' and remember it."
  (interactive)
  (let* ((cur (car custom-enabled-themes))
         (rest (cdr (memq cur ide-juice-themes)))
         (next (or (car rest) (car ide-juice-themes))))
    (when (ide-juice--load-theme next)
      (ide-juice--set :theme next)
      (message "juice: theme %s" next))))

(defun ide-juice--blend (c1 c2 alpha)
  "Blend colour name C1 over C2 by ALPHA, as a hex string (nil if undefined)."
  (let ((a (and (stringp c1) (color-name-to-rgb c1)))
        (b (and (stringp c2) (color-name-to-rgb c2))))
    (when (and a b)
      (apply #'color-rgb-to-hex
             (append (cl-mapcar (lambda (x y) (+ (* alpha x) (* (- 1 alpha) y))) a b) '(2))))))

(defface ide-juice-pulse '((t :background "#4a3320" :extend t))
  "Flash drawn by the `pulse' effect (recomputed from the theme).")

(defun ide-juice--refresh-faces (&rest _)
  "Derive the pulse colour from the active theme's accent."
  (when-let* ((hex (ide-juice--blend (face-foreground 'font-lock-function-name-face nil t)
                                     (face-background 'default nil t) 0.38)))
    (set-face-attribute 'ide-juice-pulse nil :background hex :extend t)))

(add-hook 'enable-theme-functions #'ide-juice--refresh-faces)

;;;; Packages the effects can use --------------------------------------------------

(use-package pulsar)
(use-package indent-bars)
(use-package hl-todo)
(use-package ligature)

;;;; Effects -------------------------------------------------------------------------

(ide-juice--define
 'theme "House theme (doom-ember) and the theme picker"
 :on (lambda () (ide-juice--load-theme (ide-juice-theme)))
 :off (lambda () (ide-juice--load-theme 'doom-one)))

(ide-juice--define
 'font "JetBrains Mono Nerd Font with a little more line spacing"
 :gui t
 :on (lambda ()
       (when (find-font (font-spec :family ide-juice-font-family))
         (ide-juice--stash 'font-family (face-attribute 'default :family))
         (ide-juice--stash 'line-spacing (default-value 'line-spacing))
         (set-face-attribute 'default nil :family ide-juice-font-family)
         (setq-default line-spacing ide-juice-line-spacing)
         t))
 :off (lambda ()
        (let ((fam (ide-juice--unstash 'font-family)))
          (when (stringp fam) (set-face-attribute 'default nil :family fam)))
        (setq-default line-spacing (ide-juice--unstash 'line-spacing))))

(ide-juice--define
 'ligatures "Programming ligatures (-> => != ...)"
 :gui t
 :on (lambda ()
       (when (and (require 'ligature nil t) (ide-juice--wanted-p 'font))
         (ligature-set-ligatures
          'prog-mode
          '("-->" "->" "->>" "-<" "-<<" "--" "---" "<--" "<-" "<->" ">-" ">->"
            "=>" "==>" "=>>" ">=" "<=" "=<<" "==" "===" "!=" "!==" "=/=" "<>"
            "<<" ">>" "<<<" ">>>" "||" "|||" "&&" "::" ":::" ":=" "..." ".."
            "++" "+++" "//" "///" "/*" "*/" "<!--" "<$>" "|>" "<|" "<|>" "~>"
            "~~" "~~>" "<~" "#{" "#[" "##" "###" "?." "??" "!!" ";;" "|-" "-|"
            "<=>" "<==" "<==>" "=:=" "</" "/>" "</>" "{|" "|}" "[|" "|]"))
         (global-ligature-mode 1)
         t))
 :off (lambda () (when (fboundp 'global-ligature-mode) (global-ligature-mode -1))))

(ide-juice--define
 'frame "Breathing room around the text and thin window dividers"
 :gui t
 :on (lambda ()
       (modify-all-frames-parameters `((internal-border-width . ,ide-juice-frame-padding)))
       (setf (alist-get 'internal-border-width default-frame-alist) ide-juice-frame-padding)
       (setq window-divider-default-places t
             window-divider-default-right-width 1
             window-divider-default-bottom-width 1)
       (window-divider-mode 1)
       t)
 :off (lambda ()
        (window-divider-mode -1)
        (setf (alist-get 'internal-border-width default-frame-alist) 0)
        (modify-all-frames-parameters '((internal-border-width . 0)))))

(ide-juice--define
 'modeline "Scroll-position HUD in the mode line, a taller bar"
 :gui t
 :on (lambda ()
       (when (boundp 'doom-modeline-hud)
         (dolist (v '(doom-modeline-hud doom-modeline-bar-width doom-modeline-height))
           (ide-juice--stash v (symbol-value v)))
         (setq doom-modeline-hud t doom-modeline-bar-width 5 doom-modeline-height 30)
         (when (fboundp 'doom-modeline-refresh-bars) (doom-modeline-refresh-bars))
         t))
 :off (lambda ()
        (setq doom-modeline-hud (ide-juice--unstash 'doom-modeline-hud)
              doom-modeline-bar-width (ide-juice--unstash 'doom-modeline-bar-width 4)
              doom-modeline-height (ide-juice--unstash 'doom-modeline-height 26))
        (when (fboundp 'doom-modeline-refresh-bars) (doom-modeline-refresh-bars))))

(ide-juice--define
 'pulse "Flash the line you land on after jumps, scrolls and window changes"
 :on (lambda ()
       (when (require 'pulsar nil t)
         (setq pulsar-delay 0.035
               pulsar-iterations 14
               pulsar-face 'ide-juice-pulse
               pulsar-highlight-face 'ide-juice-pulse
               pulsar-region-face 'ide-juice-pulse
               pulsar-region-change-face 'ide-juice-pulse
               pulsar-window-change-face 'ide-juice-pulse
               pulsar-pulse-on-window-change t)
         (ide-juice--refresh-faces)
         (pulsar-global-mode 1)
         t))
 :off (lambda () (when (fboundp 'pulsar-global-mode) (pulsar-global-mode -1))))

(ide-juice--define
 'scroll "Pixel-smooth mouse and touchpad scrolling"
 :gui t
 :on (lambda () (require 'pixel-scroll) (pixel-scroll-precision-mode 1) t)
 :off (lambda () (pixel-scroll-precision-mode -1)))

(ide-juice--define
 'bell "Flash the mode line instead of beeping"
 :on (lambda ()
       (when (require 'doom-themes-ext-visual-bell nil t)
         (ide-juice--stash 'ring-bell-function ring-bell-function)
         (doom-themes-visual-bell-config)
         t))
 :off (lambda () (setq ring-bell-function (ide-juice--unstash 'ring-bell-function #'ignore))))

(defun ide-juice--each-hook (hooks fn on)
  "Add (ON non-nil) or remove FN on every hook in HOOKS."
  (dolist (h hooks) (if on (add-hook h fn) (remove-hook h fn))))

(defun ide-juice--indent-bars-on () (indent-bars-mode 1))

(ide-juice--define
 'indent-guides "Tree-sitter aware indent guides (the current level glows)"
 :gui t
 :on (lambda ()
       (when (require 'indent-bars nil t)
         (setq indent-bars-treesit-support t
               indent-bars-width-frac 0.15
               indent-bars-pad-frac 0.1
               indent-bars-prefer-character (not (eq window-system 'x))
               indent-bars-color '(highlight :face-bg t :blend 0.2)
               indent-bars-highlight-current-depth '(:blend 0.7))
         (ide-juice--each-hook ide-juice-indent-hooks #'ide-juice--indent-bars-on t)
         (dolist (b (buffer-list))
           (with-current-buffer b
             (when (apply #'derived-mode-p '(prog-mode yaml-mode)) (indent-bars-mode 1))))
         t))
 :off (lambda ()
        (ide-juice--each-hook ide-juice-indent-hooks #'ide-juice--indent-bars-on nil)
        (dolist (b (buffer-list))
          (with-current-buffer b
            (when (bound-and-true-p indent-bars-mode) (indent-bars-mode -1))))))

(ide-juice--define
 'todo "Colour TODO / FIXME / NOTE keywords"
 :on (lambda () (when (require 'hl-todo nil t) (global-hl-todo-mode 1) t))
 :off (lambda () (when (fboundp 'global-hl-todo-mode) (global-hl-todo-mode -1))))

(ide-juice--define
 'cursorline "Soft highlight on the current line"
 :on (lambda ()
       (ide-juice--each-hook ide-juice-line-highlight-hooks #'hl-line-mode t)
       (dolist (b (buffer-list))
         (with-current-buffer b
           (when (apply #'derived-mode-p '(prog-mode text-mode conf-mode dired-mode))
             (hl-line-mode 1))))
       t)
 :off (lambda ()
        (ide-juice--each-hook ide-juice-line-highlight-hooks #'hl-line-mode nil)
        (dolist (b (buffer-list))
          (with-current-buffer b (when (bound-and-true-p hl-line-mode) (hl-line-mode -1))))))

(ide-juice--define
 'transparency "See-through background (needs a compositor)"
 :gui t :default nil
 :on (lambda ()
       (modify-all-frames-parameters `((alpha-background . ,ide-juice-opacity)))
       (setf (alist-get 'alpha-background default-frame-alist) ide-juice-opacity)
       t)
 :off (lambda ()
        (setq default-frame-alist (assq-delete-all 'alpha-background default-frame-alist))
        (modify-all-frames-parameters '((alpha-background . 100)))))

;;;; Start screen ------------------------------------------------------------------------

(defconst ide-juice--glyphs
  '((?F "███████╗" "██╔════╝" "█████╗  " "██╔══╝  " "██║     " "╚═╝     ")
    (?O " ██████╗ " "██╔═══██╗" "██║   ██║" "██║   ██║" "╚██████╔╝" " ╚═════╝ ")
    (?U "██╗   ██╗" "██║   ██║" "██║   ██║" "██║   ██║" "╚██████╔╝" " ╚═════╝ ")
    (?N "███╗   ██╗" "████╗  ██║" "██╔██╗ ██║" "██║╚██╗██║" "██║ ╚████║" "╚═╝  ╚═══╝")
    (?D "██████╗ " "██╔══██╗" "██║  ██║" "██║  ██║" "██████╔╝" "╚═════╝ ")
    (?R "██████╗ " "██╔══██╗" "██████╔╝" "██╔══██╗" "██║  ██║" "╚═╝  ╚═╝")
    (?Y "██╗   ██╗" "╚██╗ ██╔╝" " ╚████╔╝ " "  ╚██╔╝  " "   ██║   " "   ╚═╝   "))
  "Block letters for the start-screen logo.")

(defconst ide-juice--heat
  ["#ffd166" "#ffb347" "#ff8c42" "#f5702f" "#e8502e" "#c93a33"]
  "Logo colours, top to bottom: white-hot to cooling ember.")

(defun ide-juice--logo ()
  "The FOUNDRY logo as a list of six lines."
  (let ((rows (make-list 6 "")))
    (dolist (ch (string-to-list "FOUNDRY"))
      (let* ((g (cdr (assq ch ide-juice--glyphs)))
             (w (apply #'max (mapcar #'string-width g))))
        (setq rows (cl-mapcar (lambda (row part)
                                (concat row part (make-string (- w (string-width part)) ?\s)))
                              rows g))))
    rows))

(defconst ide-juice--actions
  '(("f" "Find file"     find-file)
    ("r" "Recent files"  consult-recent-file)
    ("p" "Projects"      project-switch-project)
    ("m" "Magit"         magit-status)
    ("t" "Terminal"      ide-toggle-terminal)
    ("a" "AI chat"       gptel)
    ("d" "Docs"          ide-docs-open)
    ("u" "User config"   ide-user-open)
    ("j" "Juice"         ide-juice-toggle))
  "Start-screen actions: (KEY LABEL COMMAND).")

(defconst ide-juice--tips
  '("C-c w 1 shows the project tree, C-c w 0 the terminal drawer"
    "C-. runs embark-act on whatever is at point"
    "C-c a M picks the AI provider and model"
    "C-c u a adds a package from MELPA or a GitHub repo"
    "C-c h d opens the offline documentation"
    "C-c j t previews themes live; C-c j j toggles any effect"
    "M-s f searches everything in the project")
  "One of these is shown on the start screen.")

(defvar-local ide-juice--splash-width nil "Window width the start screen was rendered for.")

(defun ide-juice--run (cmd)
  (if (commandp cmd) (call-interactively cmd) (user-error "%s is not available yet" cmd)))

(defun ide-juice--splash-insert-line (text pad)
  (insert (make-string (max 0 pad) ?\s) text "\n"))

(defun ide-juice--splash-render ()
  "Draw the start screen into the current buffer, centred in its window."
  (let* ((inhibit-read-only t)
         (win (or (get-buffer-window (current-buffer)) (selected-window)))
         (width (window-body-width win))
         (height (window-body-height win))
         (logo (ide-juice--logo))
         (logo-w (string-width (car logo)))
         (center (lambda (s) (max 0 (/ (- width (string-width s)) 2))))
         (greeting (format "%s, %s" (pcase (string-to-number (format-time-string "%H"))
                                      ((pred (> 5)) "Burning the midnight oil")
                                      ((pred (> 12)) "Good morning")
                                      ((pred (> 18)) "Good afternoon")
                                      (_ "Good evening"))
                           (user-login-name)))
         (cell 19)
         (rows (seq-partition ide-juice--actions 3))
         (recent (seq-take (seq-filter (lambda (f) (and (not (file-remote-p f)) (file-exists-p f)
                                                         (not (file-in-directory-p f ide-var-dir))))
                                       (bound-and-true-p recentf-list))
                           5))
         (projects (ignore-errors (seq-take (project-known-project-roots) 5)))
         (est (+ 6 2 1 (* 2 (length rows)) 2
                 (if recent (+ 2 (length recent)) 0) (if projects (+ 2 (length projects)) 0) 3))
         (top (max 1 (/ (- height est) 3))))
    (erase-buffer)
    (setq ide-juice--splash-width width)
    (insert (make-string top ?\n))
    ;; Logo, cooling from white-hot to ember.
    (let ((i 0))
      (dolist (line logo)
        (ide-juice--splash-insert-line
         (propertize line 'face `(:foreground ,(aref ide-juice--heat i) :weight bold))
         (/ (- width logo-w) 2))
        (cl-incf i)))
    (insert "\n")
    (ide-juice--splash-insert-line (propertize greeting 'face 'font-lock-comment-face) (funcall center greeting))
    (insert "\n")
    ;; Actions
    (dolist (row rows)
      (insert (make-string (max 0 (/ (- width (* cell (length row))) 2)) ?\s))
      (dolist (a row)
        (pcase-let ((`(,key ,label ,cmd) a))
          (insert-text-button (format "[%s]" key)
                              'face 'font-lock-function-name-face 'follow-link t
                              'action (lambda (_) (ide-juice--run cmd)))
          (insert " ")
          (insert-text-button label 'face 'default 'follow-link t
                              'action (lambda (_) (ide-juice--run cmd)))
          (insert (make-string (max 1 (- cell 4 (string-width label))) ?\s))))
      (insert "\n\n"))
    ;; Recent files / projects: left-aligned blocks, centred as a block.
    (cl-flet ((section (title items opener)
                (when items
                  (let* ((labels (mapcar #'abbreviate-file-name items))
                         (bw (apply #'max (string-width title) (mapcar #'string-width labels)))
                         (pad (max 0 (/ (- width bw) 2))))
                    (ide-juice--splash-insert-line (propertize title 'face 'shadow) pad)
                    (cl-mapc (lambda (path label)
                               (insert (make-string pad ?\s))
                               (insert-text-button label 'face 'default 'follow-link t
                                                   'action (lambda (_) (funcall opener path)))
                               (insert "\n"))
                             items labels)
                    (insert "\n")))))
      (section "Recent" recent #'find-file)
      (section "Projects" projects #'project-switch-project))
    (let ((tip (format "tip · %s" (nth (random (length ide-juice--tips)) ide-juice--tips)))
          (ver (format "GNU Emacs %s" emacs-version)))
      (ide-juice--splash-insert-line (propertize tip 'face 'font-lock-comment-face) (funcall center tip))
      (ide-juice--splash-insert-line (propertize ver 'face 'shadow) (funcall center ver)))
    (goto-char (point-min))))

(defvar ide-juice-splash-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m special-mode-map)
    (dolist (a ide-juice--actions)
      (let ((cmd (nth 2 a)))
        (define-key m (kbd (car a)) (lambda () (interactive) (ide-juice--run cmd)))))
    (define-key m (kbd "g") #'revert-buffer)
    (define-key m (kbd "TAB") #'forward-button)
    (define-key m (kbd "<backtab>") #'backward-button)
    m)
  "Keymap of `ide-juice-splash-mode'.")

(define-derived-mode ide-juice-splash-mode special-mode "Foundry"
  "Start screen.  Press the key in brackets, or RET on a button."
  (setq-local cursor-type nil
              line-spacing 0
              truncate-lines t
              revert-buffer-function (lambda (&rest _) (ide-juice--splash-render))))

(defun ide-juice--splash-resize (frame)
  "Re-centre the start screen when its window changes width."
  (when-let* ((buf (get-buffer "*Foundry*")))
    (dolist (win (get-buffer-window-list buf nil frame))
      (with-current-buffer buf
        (unless (eql ide-juice--splash-width (window-body-width win))
          (ide-juice--splash-render))))))

;;;###autoload
(defun ide-juice-splash-buffer ()
  "Return the (re)rendered start-screen buffer."
  (with-current-buffer (get-buffer-create "*Foundry*")
    (ide-juice-splash-mode)
    (display-line-numbers-mode -1)
    (ide-juice--splash-render)
    (add-hook 'window-size-change-functions #'ide-juice--splash-resize)
    (current-buffer)))

;;;###autoload
(defun ide-juice-splash ()
  "Show the start screen."
  (interactive)
  (switch-to-buffer (ide-juice-splash-buffer)))

(defun ide-juice--splash-choice-p ()
  "Non-nil when the start screen is wanted at startup."
  (and (ide-juice--master-p) (ide-juice--wanted-p 'splash)))

(ide-juice--define
 'splash "Forge-lit start screen instead of *scratch*"
 :on (lambda () (setq initial-buffer-choice #'ide-juice-splash-buffer) t)
 :off (lambda ()
        (when (eq initial-buffer-choice #'ide-juice-splash-buffer)
          (setq initial-buffer-choice nil))))

;; `initial-buffer-choice' is read right after init.el, before elpaca finishes.
(when (ide-juice--splash-choice-p)
  (setq initial-buffer-choice #'ide-juice-splash-buffer))

;;;; Keys and startup ------------------------------------------------------------------------

(with-eval-after-load '10-keybindings
  (define-key ide-keys-mode-map (kbd "C-c j j") #'ide-juice-toggle)
  (define-key ide-keys-mode-map (kbd "C-c j m") #'ide-juice-mode)
  (define-key ide-keys-mode-map (kbd "C-c j t") #'ide-juice-pick-theme)
  (define-key ide-keys-mode-map (kbd "C-c j n") #'ide-juice-next-theme)
  (define-key ide-keys-mode-map (kbd "C-c j s") #'ide-juice-splash)
  (define-key ide-keys-mode-map (kbd "C-c j r") #'ide-juice-reset))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c j" "juice"))

(defun ide-juice--startup ()
  "Apply the effects once packages are installed and loaded."
  (setq ide-juice-mode (ide-juice--master-p))
  (ide-juice-apply))

;; Packages arrive asynchronously; apply when elpaca is done (and retry then for
;; effects whose package was not installed yet on the first run).
(add-hook 'elpaca-after-init-hook #'ide-juice--startup)

(provide '17-juice)
;;; 17-juice.el ends here
