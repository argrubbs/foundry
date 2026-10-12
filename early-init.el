;;; early-init.el --- Sandboxed early init -*- lexical-binding: t; -*-

;;; Commentary:
;; GC tuning, UI inhibition and sandbox path anchoring.  Everything lives
;; under this directory; ~/.emacs.d is never touched.

;;; Code:

(defvar ide--early-init-loaded t "Non-nil once early-init.el has run.")

(setq user-emacs-directory
      (file-name-directory (or load-file-name buffer-file-name)))

(setq gc-cons-threshold 104857600
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 16777216
                  gc-cons-percentage 0.1)))

(setq package-enable-at-startup nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-startup-screen t
      native-comp-async-report-warnings-errors 'silent)

;; Never native-compile this config's own files.  JIT compilation runs in a
;; clean subprocess that lacks our macros (`ide-builtin', elpaca-aware
;; `use-package'), which silently produced broken code such as
;; "Cannot load popper".  Packages from elpaca are still native-compiled.
(setq native-comp-jit-compilation-deny-list
      (list (concat "\\`" (regexp-quote (expand-file-name user-emacs-directory))
                    "\\(?:modules/\\|init\\.el\\|early-init\\.el\\)")))

;; Third-party native-compiler warnings are noise in the *Warnings* popup.
(setq warning-suppress-types '((native-compiler))
      warning-suppress-log-types '((native-compiler)))

;; Keep native-compilation caches inside the sandbox.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name ".cache/eln-cache/" user-emacs-directory)))

;; Doom-ember theme bg/fg colors (modules/17-juice.el, lisp/themes/doom-ember-theme.el)
;; so the first frame does not flash white before theme loads.
(let ((plain (let ((v (getenv "IDE_NO_JUICE"))) (and v (not (string-empty-p v))))))
  (push `(background-color . ,(if plain "#282c34" "#1c1815")) default-frame-alist)
  (push `(foreground-color . ,(if plain "#bbc2cf" "#e3d9ca")) default-frame-alist)
  (unless plain (push '(internal-border-width . 14) default-frame-alist)))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(width . (text-pixels . 1370)) default-frame-alist)
(push '(height . (text-pixels . 1011)) default-frame-alist)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

(provide 'early-init)
;;; early-init.el ends here
