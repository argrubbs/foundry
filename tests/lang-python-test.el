;;; lang-python-test.el --- Tests for modules/11-languages/lang-python.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-python-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar apheleia-mode-alist)

(ert-deftest python-test-files-by-pytest-names ()
  (should (ide-python--test-file-p "/p/tests/test_api.py"))
  (should (ide-python--test-file-p "/p/api_test.py"))
  ;; directories and other names do not count
  (should-not (ide-python--test-file-p "/p/contest_app/views.py"))
  (should-not (ide-python--test-file-p "/p/test_data/load.py"))
  (should-not (ide-python--test-file-p "/p/attest_x.py"))
  (should-not (ide-python--test-file-p "/p/test_api.pyc")))

(ert-deftest python-run-command-runs-pytest-only-for-tests ()
  (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t)))
    (let ((buffer-file-name "/p/contest_app/main.py"))
      (should (equal (ide-python--run-command) "python /p/contest_app/main.py")))
    (let ((buffer-file-name "/p/tests/test_x.py"))
      (should (equal (ide-python--run-command) "python -m pytest /p/tests/test_x.py")))))

(ert-deftest python-ruff-formats-on-save ()
  ;; Pyright has no formatting: Apheleia (Ruff) formats, whenever ruff is installed.
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/ruff")))
    (should (eq (ide-python--format-on-save) t)))
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
    (should (eq (ide-python--format-on-save) 'off)))
  (require 'apheleia)
  (dolist (mode '(python-mode python-ts-mode))
    (should (equal (alist-get mode apheleia-mode-alist) '(ruff-isort ruff)))))

(ert-deftest python-buffer-enables-apheleia ()
  (skip-unless (and (treesit-language-available-p 'python) (executable-find "ruff")))
  (with-temp-buffer
    (cl-letf (((symbol-function 'eglot-ensure) #'ignore))
      (python-ts-mode))
    (should (bound-and-true-p apheleia-mode))))

(provide 'lang-python-test)
;;; lang-python-test.el ends here
