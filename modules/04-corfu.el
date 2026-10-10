;;; 04-corfu.el --- In-buffer completion -*- lexical-binding: t; -*-

;;; Commentary:
;; Corfu popup, Cape backends, Yasnippet templates.

;;; Code:

(use-package corfu
  :demand t
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
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'corfu
    (define-key corfu-map (kbd "RET") #'corfu-insert)))

(use-package nerd-icons-corfu
  :after corfu
  :config (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package yasnippet
  :demand t
  :init (setq yas-snippet-dirs (list (expand-file-name "snippets/" user-emacs-directory)))
  :config (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

(use-package cape
  :demand t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :config
  ;; Compose Eglot's capf with snippets, file and buffer symbols.
  (defun ide--eglot-capf-setup ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-dabbrev)
                      #'cape-file)))
  (add-hook 'eglot-managed-mode-hook #'ide--eglot-capf-setup))

(use-package yasnippet-capf :after (cape yasnippet))

(provide '04-corfu)
;;; 04-corfu.el ends here
