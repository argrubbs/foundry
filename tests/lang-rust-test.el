;;; lang-rust-test.el --- Tests for modules/11-languages/lang-rust.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-rust-test.el -f ert-run-tests-batch-and-exit
;; Pure logic only (no server); the rust-analyzer round trip was checked by hand.

;;; Code:

(require 'ert)

(ert-deftest rust-menu-and-hooks-registered ()
  (should (eq (cdr (assoc '(rust-ts-mode rust-mode) ide-lang-menus)) #'ide-rust-dispatch))
  (should (memq #'ide-rust--run-command ide-run-command-functions))
  (should (eq (alist-get "\\.rs\\'" auto-mode-alist nil nil #'equal) 'rust-ts-mode)))

(ert-deftest rust-runnable-becomes-a-cargo-command ()
  (let ((r '(:label "test a::b" :kind "cargo"
             :args (:cwd "/p" :cargoArgs ["test" "--package" "x" "--lib"]
                    :executableArgs ["a::b" "--exact"]))))
    (should (equal (ide-rust--runnable-command r)
                   '(("cargo" "test" "--package" "x" "--lib" "--" "a::b" "--exact") . "/p")))))

(ert-deftest rust-debug-builds-turn-run-and-test-into-builds ()
  (should (equal (ide-rust--debug-build-args '("run" "--bin" "x")) '("build" "--bin" "x")))
  (should (equal (ide-rust--debug-build-args '("test" "-p" "x")) '("test" "--no-run" "-p" "x"))))

(ert-deftest rust-artifact-is-the-last-executable ()
  (let ((out (concat "{\"reason\":\"compiler-artifact\",\"executable\":null}\n"
                     "{\"reason\":\"compiler-message\"}\n"
                     "{\"reason\":\"compiler-artifact\",\"executable\":\"/t/debug/x\"}\n"
                     "{\"reason\":\"build-finished\",\"success\":true}\n")))
    (should (equal (ide-rust--artifact out) "/t/debug/x"))
    (should-not (ide-rust--artifact "{\"reason\":\"build-finished\"}\n"))))

(ert-deftest rust-file-test-filter ()
  (progn
    (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) "/p/")))
      (let ((buffer-file-name "/p/tests/api.rs"))
        (should (equal (ide-rust--file-test-filter) '("--test" "api"))))
      (let ((buffer-file-name "/p/src/net/client.rs"))
        (should (equal (ide-rust--file-test-filter) '("net::client"))))
      (let ((buffer-file-name "/p/src/net/mod.rs"))
        (should (equal (ide-rust--file-test-filter) '("net"))))
      (let ((buffer-file-name "/p/src/lib.rs"))
        (should-not (ide-rust--file-test-filter))))))

(ert-deftest rust-test-name-qualified-by-modules ()
  (skip-unless (treesit-language-available-p 'rust))
  (with-temp-buffer
    (insert "mod a {\n  mod b {\n    #[test]\n    fn it_works() {\n      assert!(true);\n    }\n  }\n}\n")
    (rust-ts-mode)
    (goto-char (point-min)) (search-forward "assert")
    (should (equal (ide-rust--test-name-at-point) "a::b::it_works"))))

(ert-deftest rust-test-runner-nextest-falls-back-to-cargo ()
  (let ((ide-rust-test-runner 'nextest))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should (equal (ide-rust--test-command "x") '("cargo" "test" "x"))))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/cargo-nextest")))
      (should (equal (ide-rust--test-command "x") '("cargo" "nextest" "run" "x"))))))

;;; lang-rust-test.el ends here
