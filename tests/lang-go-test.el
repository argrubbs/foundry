;;; lang-go-test.el --- Tests for modules/11-languages/lang-go.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-go-test.el -f ert-run-tests-batch-and-exit
;; The debugger is not started; the launch it gets is checked.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defvar dap-dlv-go-delve-path)
(defvar ide-debug-functions)

(ert-deftest go-debug-launch-for-package-and-tests ()
  (let ((main (ide-go--debug-launch "/p/cmd/app/main.go"))
        (test (ide-go--debug-launch "/p/pkg/x_test.go")))
    (should (equal (plist-get main :type) "go"))
    (should (equal (plist-get main :mode) "debug"))
    (should (equal (plist-get main :program) "/p/cmd/app/"))
    (should (equal (plist-get test :mode) "test"))
    (should (equal (plist-get test :program) "/p/pkg/"))))

(ert-deftest go-f5-debugs-with-dlv-dap ()
  ;; F5 goes through `ide-debug-functions' to dap-dlv-go (not the deprecated dap-go,
  ;; whose bare dlv is no debug adapter).
  ;; Loaded before the stubs go in: loading dap-mode later would redefine `dap-debug'.
  (require 'dap-mode)
  (require 'dap-dlv-go)
  (let (launched (dap-dlv-go-delve-path dap-dlv-go-delve-path))
    (cl-letf (((symbol-function 'dap-debug) (lambda (conf) (setq launched conf)))
              ((symbol-function 'executable-find)
               (lambda (name &rest _) (and (equal name "dlv") "/bin/dlv"))))
      (with-temp-buffer
        (setq buffer-file-name "/p/main.go")
        (setq major-mode 'go-ts-mode)
        (should (run-hook-with-args-until-success 'ide-debug-functions))
        (setq buffer-file-name nil))
      (should (equal dap-dlv-go-delve-path "/bin/dlv")))
    (should (equal (plist-get launched :program) "/p/"))
    (should (equal (plist-get launched :mode) "debug"))))

(provide 'lang-go-test)
;;; lang-go-test.el ends here
