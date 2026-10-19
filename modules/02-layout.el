;;; 02-layout.el --- Tool-window layout (popper, treemacs, imenu-list) -*- lexical-binding: t; -*-

;;; Commentary:
;; Bottom drawer for transient/tool buffers, left project tree, right
;; structure view.  No split chaos.

;;; Code:

(defconst ide-bottom-buffer-regexps
  '("\\*compilation\\*" "\\*quickrun\\*" "\\*pytest\\*" "\\*go-test\\*"
    "\\*Flymake diagnostics.*" "\\*xref\\*" "\\*Occur\\*" "\\*grep\\*"
    "\\*ghostel.*" "\\*eat\\*" "\\*terminal\\*" "\\*shell\\*" "\\*Python\\*" "\\*ielm\\*"
    "\\*docker-.*" "\\*kubel.*" "\\*kubectl.*" "\\*azure-.*" "\\*azdo-.*"
    "\\*gptel-.*" "\\*minuet-.*"
    "\\*Warnings\\*" "\\*Messages\\*" "\\*Help\\*" "\\*Async Shell Command\\*"
    "\\*ide-mise.*" "\\*ide-docs.*" "\\*iac-.*" "\\*terraform.*")
  "Regexps of buffers that dock into the bottom drawer.")

(use-package popper
  :demand t
  :init
  (setq popper-reference-buffers
        (append ide-bottom-buffer-regexps
                '(compilation-mode help-mode xref--xref-buffer-mode
                  flymake-diagnostics-buffer-mode grep-mode occur-mode))
        popper-display-control nil   ; display-buffer-alist below owns placement
        popper-window-height (lambda (win)
                               (fit-window-to-buffer
                                win (floor (* 0.30 (frame-height))) 8)))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(defvar ide-bottom-slot-params
  '((side . bottom) (slot . 0) (window-height . 0.30)
    (window-parameters . ((no-other-window . nil)))))

;; Order matters: first match wins.
(setq display-buffer-alist
      (append
       `(((major-mode . treemacs-mode)
          (display-buffer-in-side-window)
          (side . left) (slot . 0) (window-width . 32)
          (window-parameters . ((no-delete-other-windows . t))))
         ("\\*Ilist\\*"
          (display-buffer-in-side-window)
          (side . right) (slot . 0) (window-width . 30)
          (window-parameters . ((no-delete-other-windows . t))))
         ("\\*magit-\\(diff\\|revision\\|log\\|process\\).*"
          (display-buffer-reuse-window display-buffer-at-bottom)
          (window-height . 0.40))
         ,@(mapcar (lambda (re)
                     `(,re (display-buffer-reuse-window display-buffer-in-side-window)
                           ,@ide-bottom-slot-params))
                   ide-bottom-buffer-regexps)
         ((derived-mode . compilation-mode)
          (display-buffer-reuse-window display-buffer-in-side-window)
          ,@ide-bottom-slot-params)
         ("\\*dap-ui-\\(locals\\|breakpoints\\|expressions\\|sessions\\|repl\\)\\*"
          (display-buffer-in-side-window)
          (side . right) (slot . 1) (window-width . 40))
         ("\\*dap-ui-.*"
          (display-buffer-in-side-window)
          (side . bottom) (slot . 1) (window-height . 0.30)))
       display-buffer-alist))

(defun ide-add-drawer-buffer (regexp)
  "Dock buffers whose name matches REGEXP in the bottom drawer.
For tools you add under user/: they then behave like the built-in ones."
  (add-to-list 'ide-bottom-buffer-regexps regexp)
  (add-to-list 'display-buffer-alist
               `(,regexp (display-buffer-reuse-window display-buffer-in-side-window)
                         ,@ide-bottom-slot-params))
  (when (boundp 'popper-reference-buffers)
    (add-to-list 'popper-reference-buffers regexp)
    (when (bound-and-true-p popper-mode) (popper-mode -1) (popper-mode 1)))
  regexp)

(setq window-combination-resize t
      switch-to-buffer-obey-display-actions t
      split-width-threshold 160
      split-height-threshold nil)

;;;; Project tool window
(use-package treemacs
  :commands (treemacs treemacs-select-window)
  :init
  (setq treemacs-persist-file (expand-file-name "treemacs-persist" ide-var-dir)
        treemacs-last-error-persist-file (expand-file-name "treemacs-last-error" ide-var-dir)
        treemacs-width 32
        treemacs-width-is-initially-locked t
        treemacs-position 'left
        treemacs-is-never-other-window t
        treemacs-display-in-side-window t)
  :config
  (treemacs-follow-mode 1)
  (treemacs-filewatch-mode 1)
  (treemacs-fringe-indicator-mode 'only-when-focused))

(use-package treemacs-nerd-icons
  :after treemacs
  :config (treemacs-load-theme "nerd-icons"))

;;;; Structure tool window
(use-package imenu-list
  :commands (imenu-list-smart-toggle imenu-list)
  :init
  (setq imenu-list-position 'right
        imenu-list-size 30
        imenu-list-focus-after-activation nil
        imenu-list-auto-resize nil)
  :config
  ;; The package installs its own split-window rule ahead of ours when it loads;
  ;; drop it so the outline is a side window like the other tool windows.
  (setq display-buffer-alist
        (cl-remove-if (lambda (e) (eq (cadr e) 'imenu-list-display-buffer)) display-buffer-alist)))

(defun ide-toggle-project-tree ()
  "Toggle the left project tree and focus it."
  (interactive)
  (if (and (fboundp 'treemacs-get-local-window) (treemacs-get-local-window))
      (delete-window (treemacs-get-local-window))
    (treemacs)))

(defun ide-toggle-structure ()
  "Toggle the right structure outline."
  (interactive)
  (imenu-list-smart-toggle))

(defun ide-focus-code-window ()
  "Return focus to the main code window, closing ephemeral popups."
  (interactive)
  (when (and (fboundp 'popper-close-latest) (bound-and-true-p popper-mode)
             (popper-popup-p (current-buffer)))
    (ignore-errors (popper-close-latest)))
  (let ((win (seq-find (lambda (w)
                         (and (not (window-parameter w 'window-side))
                              (not (window-dedicated-p w))
                              (not (minibufferp (window-buffer w)))))
                       (window-list))))
    (when win (select-window win))))

(provide '02-layout)
;;; 02-layout.el ends here
