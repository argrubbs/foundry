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

(provide 'juice-test)
;;; juice-test.el ends here
