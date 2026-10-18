;;; lang-lua-test.el --- Tests for modules/11-languages/lang-lua.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-lua-test.el -f ert-run-tests-batch-and-exit
;; Pure logic plus stylua; the lua-language-server round trip was checked by hand.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst lua-test--spec
  "describe(\"math (x)\", function()
  it(\"adds\", function()
    assert.are.equal(2, 1 + 1)
  end)
end)
")

(defun lua-test--title-at (needle)
  "Test path at the first occurrence of NEEDLE in the spec sample."
  (with-temp-buffer
    (insert lua-test--spec)
    (lua-ts-mode)
    (goto-char (point-min))
    (search-forward needle)
    (ide-lua--test-path)))

(ert-deftest lua-test-path-names-the-it-and-the-describe ()
  (skip-unless (treesit-ready-p 'lua t))
  (should (equal (lua-test--title-at "assert") '((describe . "math (x)") (it . "adds"))))
  (should (equal (lua-test--title-at "describe") '((describe . "math (x)")))))

(ert-deftest lua-filter-pattern-escapes-and-anchors ()
  (should (equal (ide-lua--filter-pattern '((describe . "math (x)") (it . "adds")))
                 "^math %(x%) adds$"))
  (should (equal (ide-lua--filter-pattern '((describe . "a-b"))) "^a%-b"))
  (should-not (ide-lua--filter-pattern nil)))

(ert-deftest lua-luarc-presets ()
  (let ((rc (ide-lua--luarc '(:globals ["vim"] :runtime "LuaJIT") '("foo"))))
    (should (equal (plist-get rc :runtime.version) "LuaJIT"))
    (should (equal (plist-get rc :diagnostics.globals) ["vim" "foo"])))
  (should-not (plist-member (ide-lua--luarc nil) :diagnostics.globals)))

(ert-deftest lua-manual-urls ()
  (let ((ide-lua-manual-version "5.4"))
    (should (string-suffix-p "#pdf-print" (ide-lua--manual-url "print")))
    (should (string-suffix-p "#pdf-string.format" (ide-lua--manual-url "string.format")))
    (should (string-suffix-p "#index" (ide-lua--manual-url "my_function")))
    (should (string-suffix-p "#index" (ide-lua--manual-url nil)))))

(ert-deftest lua-run-command-picks-lua-or-busted ()
  (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) "/p/")))
    (with-temp-buffer
      (setq major-mode 'lua-ts-mode)
      (let ((buffer-file-name "/p/src/a.lua")) (should (equal (ide-lua--run-command) "lua src/a.lua")))
      (cl-letf (((symbol-function 'executable-find) (lambda (n &rest _) (and (equal n "busted") "/bin/busted"))))
        (let ((buffer-file-name "/p/spec/a_spec.lua"))
          (should (equal (ide-lua--run-command) "busted spec/a_spec.lua")))))))

(ert-deftest lua-stylua-args-honour-project-config ()
  (let* ((dir (make-temp-file "lua-test" t))
         (default-directory (file-name-as-directory dir))
         (buffer-file-name (expand-file-name "a.lua" dir)))
    (unwind-protect
        (progn
          (should (equal (ide-lua--stylua-args)
                         (list "--indent-type" "Spaces" "--indent-width" "2" "--stdin-filepath" buffer-file-name)))
          (write-region "" nil (expand-file-name ".stylua.toml" dir))
          (should (equal (ide-lua--stylua-args)
                         (list "--config-path" (expand-file-name ".stylua.toml" dir)
                               "--stdin-filepath" buffer-file-name))))
      (delete-directory dir t))))

(ert-deftest lua-stylua-formats-with-two-spaces ()
  (skip-unless (executable-find "stylua"))
  (with-temp-buffer
    (insert "local function f()\n    return 1\nend\n")
    (let ((default-directory temporary-file-directory)
          (out (generate-new-buffer " *stylua-out*")))
      (unwind-protect
          (progn
            (apply #'call-process-region (point-min) (point-max) "stylua" nil out nil
                   (append (ide-lua--stylua-args) '("-")))
            (should (equal (with-current-buffer out (buffer-string))
                           "local function f()\n  return 1\nend\n")))
        (kill-buffer out)))))

(provide 'lang-lua-test)
;;; lang-lua-test.el ends here
