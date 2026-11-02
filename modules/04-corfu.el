;;; 04-corfu.el --- In-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Corfu popup, Cape backends, Yasnippet templates.

;;; Code:

(use-package corfu
  :demand t
  :bind (:map corfu-map ("RET" . corfu-insert))
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-popupinfo-delay '(0.3 . 0.1)
        tab-always-indent 'complete
        text-mode-ispell-word-completion nil)
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :demand t
  :init
  (setq yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)))
  :config
  (add-hook 'yas-before-expand-snippet-hook (lambda () (setq-local indent-tabs-mode nil)))
  (add-hook 'yas-after-exit-snippet-hook (lambda () (setq-local indent-tabs-mode nil)))
  (yas-global-mode 1))

(use-package cape
  :demand t
  :config
  ;; Compose Eglot's capf with file and buffer symbols.
  (add-hook 'eglot-managed-mode-hook #'ide--eglot-capf-setup))

(declare-function eglot-managed-p "eglot")
(declare-function eglot-completion-at-point "eglot")
(declare-function cape-capf-super "cape")
(declare-function cape-file "cape")
(declare-function cape-dabbrev "cape")

(defvar-local ide--eglot-capfs nil
  "Capfs `ide--eglot-capf-setup' added to this buffer, to remove when Eglot stops.")

(defun ide--eglot-capf-setup ()
  "Swap Eglot's capf for one that also offers dabbrev words, plus file names.
`eglot-managed-mode-hook' runs both when Eglot starts and when it stops
managing the buffer: on the way out, take our capfs back out, leaving the
mode's own capfs (and the global fallback `t') as they were."
  (dolist (f ide--eglot-capfs)
    (remove-hook 'completion-at-point-functions f t))
  (setq ide--eglot-capfs nil)
  (when (eglot-managed-p)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (unless (memq #'cape-file completion-at-point-functions)
      (add-hook 'completion-at-point-functions #'cape-file nil t)
      (push #'cape-file ide--eglot-capfs))
    (let ((super (cape-capf-super #'eglot-completion-at-point #'cape-dabbrev)))
      (add-hook 'completion-at-point-functions super nil t)
      (push super ide--eglot-capfs))))

(use-package yasnippet-capf :after (cape yasnippet))

(defun ide-complete-snippet ()
  "Complete snippets at point via Corfu, or prompt to insert a snippet."
  (interactive)
  (unless (bound-and-true-p yas-minor-mode)
    (yas-minor-mode 1))
  (if (and (fboundp 'yasnippet-capf) (yasnippet-capf))
      (cape-interactive #'yasnippet-capf)
    (call-interactively #'yas-insert-snippet)))

(provide '04-corfu)
;;; 04-corfu.el ends here
