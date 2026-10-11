;;; 18-telega.el --- Telegram client integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Telega client configuration for Foundry.
;; Documentation: https://zevlg.github.io/telega.el
;;
;; Telega runs on top of Telegram's TDLib. By default, this configuration uses
;; the official pre-built Docker container (`zevlg/telega-server:latest`),
;; avoiding the lengthy compilation of TDLib and its C++ dependencies.
;;
;; Keybindings:
;;   C-c t     `telega' (Open root chats overview)
;;   C-c T     `telega-prefix-map' (Navigation, chat folders, calls, etc.)
;;   C-c x t   `telega'
;;   C-c x T   `telega-prefix-map'
;;
;; Runtime data and database files are isolated in `var/telega/' within Foundry.

;;; Code:

(require 'cl-lib)

(defvar ide-var-dir)
(defvar ide-keys-mode-map)
(defvar telega-prefix-map)
(defvar telega-directory)
(defvar telega-database-dir)
(defvar telega-server-logfile)
(defvar telega-cache-dir)
(defvar telega-temp-dir)
(defvar telega-docker-volumes)
(defvar telega-use-docker)
(defvar telega-chat-mode-hook)
(defvar telega-chat-fill-column)
(defvar telega-emoji-use-images)
(defvar telega-completing-read-function)
(defvar telega-symbol-folder)

(declare-function ide-add-drawer-buffer "02-layout")
(declare-function telega "telega")
(declare-function telega-completions-setup-capf "telega")
(declare-function telega-transient-keymaps-mode "telega-transient")
(declare-function global-telega-mnz-mode "telega-mnz")
(declare-function telega-notifications-mode "telega-notifications")

;; Dock telega background/server logs into Foundry's bottom drawer
(when (fboundp 'ide-add-drawer-buffer)
  (ide-add-drawer-buffer "\\*telega-server\\*")
  (ide-add-drawer-buffer "\\*Telegram Notification Messages\\*"))

(use-package telega
  :commands (telega telega-server-version)
  :init
  ;; 1. Sandbox isolation: Redirect all session data and cache into var/telega/
  (setq telega-directory (expand-file-name "telega/" ide-var-dir))
  (unless (file-directory-p telega-directory)
    (make-directory telega-directory t))

  ;; Ensure TDLib database, logfile, cache, and temp directory match telega-directory
  (setq telega-database-dir telega-directory
        telega-server-logfile (expand-file-name "telega-server.log" telega-directory)
        telega-cache-dir (expand-file-name "cache" telega-directory)
        telega-temp-dir (expand-file-name "temp" telega-directory))
  (with-eval-after-load 'telega-customize
    (setq telega-database-dir telega-directory
          telega-server-logfile (expand-file-name "telega-server.log" telega-directory)
          telega-cache-dir (expand-file-name "cache" telega-directory)
          telega-temp-dir (expand-file-name "temp" telega-directory)))

  ;; 2. Docker integration: Use the official pre-built telega-server container
  ;; To use a locally compiled telega-server instead, set this to nil.
  (setq telega-use-docker t)

  ;; Bind mount ~/.telega into container as well (via telega-docker-volumes)
  ;; so telega-server can access both paths without permission denied errors.
  (let ((home-telega (expand-file-name "~/.telega")))
    (unless (file-directory-p home-telega)
      (make-directory home-telega t))
    (setq telega-docker-volumes
          (delete-dups (cons home-telega
                             (or (bound-and-true-p telega-docker-volumes)
                                 '("/usr/share/X11/xkb"))))))
  (with-eval-after-load 'telega-customize
    (add-to-list 'telega-docker-volumes (expand-file-name "~/.telega")))

  ;; Keybindings on ide-keys-mode-map (available once 10-keybindings loads)
  (with-eval-after-load '10-keybindings
    (define-key ide-keys-mode-map (kbd "C-c t") #'telega)
    (define-key ide-keys-mode-map (kbd "C-c T") 'telega-prefix-map)
    (define-key ide-keys-mode-map (kbd "C-c x t") #'telega)
    (define-key ide-keys-mode-map (kbd "C-c x T") 'telega-prefix-map))

  :config
  ;; In-buffer completions: Hook CAPF for Corfu integration
  ;; Provides @username mentions, #hashtags, :emoji:, /commands, etc.
  (add-hook 'telega-chat-mode-hook #'telega-completions-setup-capf)

  ;; Transient menus: Use transient popups for telega keymaps (matches Foundry style)
  (when (require 'telega-transient nil t)
    (when (fboundp 'telega-transient-keymaps-mode)
      (telega-transient-keymaps-mode 1)))

  ;; Code syntax highlighting inside messages (via telega-mnz)
  (when (require 'telega-mnz nil t)
    (when (fboundp 'global-telega-mnz-mode)
      (global-telega-mnz-mode 1)))

  ;; Org-mode link support (store and follow links to chats/messages)
  (require 'ol-telega nil t)

  ;; Dired integration: attach marked files to active chat via 'C' in Dired
  (require 'telega-dired-dwim nil t)

  ;; Desktop notifications via D-Bus on Linux
  (when (fboundp 'telega-notifications-mode)
    (telega-notifications-mode 1))

  ;; Sane defaults & visual adjustments
  (setq telega-chat-fill-column 80
        telega-emoji-use-images t
        telega-completing-read-function #'completing-read
        telega-symbol-folder "📁"))

(provide '18-telega)
;;; 18-telega.el ends here
