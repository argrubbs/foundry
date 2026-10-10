;;; juice-test.el --- Tests for modules/17-juice.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/juice-test.el
;; Uses throw-away effects and a temporary state file; no frame is touched.

;;; Code:

(require 'ert)

(defmacro juice-test--with-sandbox (&rest body)
  "Run BODY with an empty state file and two counting effects."
  `(let* ((ide-juice-state-file (make-temp-file "juice" nil ".eld"))
          (ide-juice--state :unread)
          (ide-juice--active nil)
          (ide-juice--stash nil)
          (ide-juice--effects nil)
          (juice-test--log nil))
     (delete-file ide-juice-state-file)
     (ide-juice--define 'alpha "a" :on (lambda () (push 'alpha-on juice-test--log) t)
                        :off (lambda () (push 'alpha-off juice-test--log)))
     (ide-juice--define 'beta "b" :default nil
                        :on (lambda () (push 'beta-on juice-test--log) t))
     (ide-juice--define 'later "c" :on (lambda () nil))
     (unwind-protect (progn ,@body)
       (when (file-exists-p ide-juice-state-file) (delete-file ide-juice-state-file)))))

(defvar juice-test--log nil)

(ert-deftest juice-defaults-decide-what-is-on ()
  (juice-test--with-sandbox
   (ide-juice-apply)
   (should (memq 'alpha ide-juice--active))
   (should-not (memq 'beta ide-juice--active))))

(ert-deftest juice-effect-that-is-not-ready-is-retried ()
  "An :on returning nil (package not installed yet) stays inactive, no error."
  (juice-test--with-sandbox
   (ide-juice-apply)
   (should-not (memq 'later ide-juice--active))))

(ert-deftest juice-toggle-persists-and-undoes ()
  (juice-test--with-sandbox
   (ide-juice-apply)
   (ide-juice-toggle 'alpha)
   (should-not (memq 'alpha ide-juice--active))
   (should (memq 'alpha-off juice-test--log))
   (ide-juice-toggle 'beta)
   (should (memq 'beta ide-juice--active))
   ;; A fresh read of the file sees both choices.
   (setq ide-juice--state :unread)
   (should-not (ide-juice--wanted-p 'alpha))
   (should (ide-juice--wanted-p 'beta))))

(ert-deftest juice-master-switch-turns-everything-off-and-back ()
  (juice-test--with-sandbox
   (ide-juice-apply)
   (ide-juice-mode -1)
   (should-not ide-juice--active)
   (ide-juice-mode 1)
   (should (memq 'alpha ide-juice--active))))

(ert-deftest juice-reset-forgets-choices ()
  (juice-test--with-sandbox
   (ide-juice-apply)
   (ide-juice-toggle 'alpha)
   (ide-juice-reset)
   (should-not (file-exists-p ide-juice-state-file))
   (should (memq 'alpha ide-juice--active))))

(ert-deftest juice-env-var-means-plain ()
  (juice-test--with-sandbox
   (let ((process-environment (cons "IDE_NO_JUICE=1" process-environment)))
     (ide-juice-apply)
     (should-not ide-juice--active)
     (should (eq (ide-juice-initial-theme) 'doom-one))
     ;; Plain mode never writes the state file.
     (ide-juice-mode -1)
     (should-not (file-exists-p ide-juice-state-file)))))

(ert-deftest juice-stash-restores-original-once ()
  (juice-test--with-sandbox
   (ide-juice--stash 'x 1)
   (ide-juice--stash 'x 2)
   (should (= 1 (ide-juice--unstash 'x)))
   (should (= 9 (ide-juice--unstash 'x 9)))))

(ert-deftest juice-house-theme-loads ()
  "doom-ember loads without error and defines the warm background."
  (ide-juice--theme-path)
  (require 'doom-themes)
  (let ((custom-enabled-themes nil))
    (load-theme 'doom-ember t)
    (unwind-protect
        (should (string-match-p "#1c1815" (format "%S" (get 'default 'theme-face))))
      (disable-theme 'doom-ember))))

(ert-deftest juice-logo-is-rectangular ()
  (let ((w (mapcar #'string-width (ide-juice--logo))))
    (should (= 6 (length w)))
    (should (= 1 (length (delete-dups w))))))

(ert-deftest juice-splash-renders-and-lists-actions ()
  (let ((buf (ide-juice-splash-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (should (derived-mode-p 'ide-juice-splash-mode))
          (should (string-match-p "Find file" (buffer-string)))
          (should (string-match-p "██" (buffer-string)))
          (should (commandp (lookup-key ide-juice-splash-mode-map "f"))))
      (kill-buffer buf))))

(ert-deftest juice-keys-bound ()
  (should (eq (lookup-key ide-keys-mode-map (kbd "C-c j j")) #'ide-juice-toggle))
  (should (eq (lookup-key ide-keys-mode-map (kbd "C-c j t")) #'ide-juice-pick-theme)))

;;; juice-test.el ends here
