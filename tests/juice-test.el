;;; juice-test.el --- Tests for modules/17-juice.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/juice-test.el

;;; Code:

(require 'ert)
(require '17-juice)

(ert-deftest juice-env-var-means-plain ()
  (let ((process-environment (cons "IDE_NO_JUICE=1" process-environment)))
    (should (ide-juice--plain-p))
    (should (eq (ide-juice-initial-theme) 'doom-one))))

(ert-deftest juice-house-theme-loads ()
  "doom-ember loads without error and defines the warm background."
  (ide-juice--theme-path)
  (require 'doom-themes)
  (let ((custom-enabled-themes nil))
    (load-theme 'doom-ember t)
    (unwind-protect
        (should (string-match-p "#1c1815" (format "%S" (get 'default 'theme-face))))
      (disable-theme 'doom-ember))))

(ert-deftest juice-keys-bound ()
  (should (eq (lookup-key ide-keys-mode-map (kbd "C-c j t")) #'ide-juice-pick-theme)))

;; The cursorline belongs to code, text, config and dired buffers, never
;; terminals or other special buffers.
(ert-deftest juice-hl-line-only-in-editing-buffers ()
  (let ((global-hl-line-buffers (bound-and-true-p global-hl-line-buffers))
        (was (bound-and-true-p global-hl-line-mode)))
    (unwind-protect
        (progn
          (ide-juice--setup-hl-line)
          (should global-hl-line-mode)
          (require 'shell)
          (dolist (case '((emacs-lisp-mode . t) (text-mode . t) (conf-unix-mode . t)
                          (special-mode . nil) (fundamental-mode . nil) (shell-mode . nil)))
            (with-temp-buffer
              (rename-buffer (format "juice-%s" (car case)) t)
              (delay-mode-hooks (funcall (car case)))
              (should (eq (cdr case) (and (buffer-match-p global-hl-line-buffers (current-buffer)) t))))))
      (unless was (global-hl-line-mode -1)))))

(provide 'juice-test)
;;; juice-test.el ends here
