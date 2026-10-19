;;; lang-web-test.el --- Tests for modules/11-languages/lang-web.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-web-test.el -f ert-run-tests-batch-and-exit
;; Pure logic and tree-sitter only (no server); the Eglot, ESLint, prettier and debugger
;; round trips were checked by hand.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defmacro lang-web-test--with-project (files &rest body)
  "Run BODY in a temp directory holding FILES, an alist of (NAME . CONTENT)."
  (declare (indent 1))
  `(let ((dir (make-temp-file "web-test" t)))
     (unwind-protect
         (let ((default-directory (file-name-as-directory dir)))
           (dolist (f ,files)
             (make-directory (file-name-directory (expand-file-name (car f) dir)) t)
             (write-region (cdr f) nil (expand-file-name (car f) dir) nil 'silent))
           ,@body)
       (delete-directory dir t))))

(ert-deftest web-package-manager-from-lockfile ()
  (dolist (case '(("pnpm-lock.yaml" . "pnpm") ("yarn.lock" . "yarn")
                  ("bun.lockb" . "bun") ("package-lock.json" . "npm")))
    (lang-web-test--with-project (list '("package.json" . "{}") (cons (car case) ""))
      (should (equal (ide-web--package-manager default-directory) (cdr case))))))

(ert-deftest web-package-manager-from-field-and-default ()
  (lang-web-test--with-project '(("package.json" . "{\"packageManager\":\"pnpm@9.1.0\"}"))
    (should (equal (ide-web--package-manager default-directory) "pnpm")))
  (lang-web-test--with-project '(("package.json" . "{}"))
    (should (equal (ide-web--package-manager default-directory) "npm"))))

(ert-deftest web-package-manager-finds-lockfile-in-parent ()
  (lang-web-test--with-project '(("yarn.lock" . "") ("packages/a/package.json" . "{}"))
    (should (equal (ide-web--package-manager (expand-file-name "packages/a" default-directory)) "yarn"))))

(ert-deftest web-add-command-per-manager ()
  (should (equal (ide-web--add-command "npm" '("zod") nil) '("npm" "install" nil "zod")))
  (should (equal (ide-web--add-command "npm" '("vitest") t) '("npm" "install" "--save-dev" "vitest")))
  (should (equal (ide-web--add-command "pnpm" '("a" "b") t) '("pnpm" "add" "-D" "a" "b")))
  (should (equal (ide-web--add-command "yarn" '("a") nil) '("yarn" "add" nil "a")))
  (should (equal (ide-web--add-command "bun" '("a") t) '("bun" "add" "--dev" "a"))))

(ert-deftest web-scripts-and-dev-script ()
  (lang-web-test--with-project
      '(("package.json" . "{\"scripts\":{\"build\":\"tsc\",\"start\":\"node .\",\"dev\":\"vite\"}}"))
    (let ((d default-directory))
      (should (equal (ide-web--scripts d) '(("build" . "tsc") ("start" . "node .") ("dev" . "vite"))))
      (let ((default-directory d))
        (cl-letf (((symbol-function 'ide-web--project-dir) (lambda () d)))
          (should (equal (ide-web--dev-script) "dev")))))))

(ert-deftest web-test-runner-detection ()
  (cl-flet ((runner (json)
              (lang-web-test--with-project (list (cons "package.json" json))
                (let ((d default-directory))
                  (cl-letf (((symbol-function 'ide-web--project-dir) (lambda () d)))
                    (ide-web--test-runner))))))
    (should (eq (runner "{\"devDependencies\":{\"vitest\":\"^3\"}}") 'vitest))
    (should (eq (runner "{\"devDependencies\":{\"jest\":\"^30\"}}") 'jest))
    (should (eq (runner "{\"scripts\":{\"test\":\"jest --ci\"}}") 'jest))
    (should-not (runner "{\"scripts\":{\"test\":\"node --test\"}}"))))

(ert-deftest web-test-command-shapes ()
  (lang-web-test--with-project
      '(("package.json" . "{\"devDependencies\":{\"vitest\":\"1\"}}") ("src/a.test.ts" . ""))
    (let ((d default-directory) (file (expand-file-name "src/a.test.ts" default-directory)))
      (cl-letf (((symbol-function 'ide-web--project-dir) (lambda () d))
                ((symbol-function 'ide-lang-node-exe) (lambda (n &optional _) (concat "/bin/" n))))
        (should (equal (ide-web--test-command :file file :name "adds")
                       '("/bin/vitest" "run" "src/a.test.ts" "-t" "adds")))
        (should (equal (ide-web--test-command :file file :watch t) '("/bin/vitest" "src/a.test.ts")))
        (should (equal (ide-web--test-command) '("/bin/vitest" "run")))))))

(ert-deftest web-regexp-quote-for-test-names ()
  (should (equal (ide-web--regexp-quote "adds (1 + 2) [x]") "adds \\(1 \\+ 2\\) \\[x\\]"))
  (should (equal (ide-web--regexp-quote "plain name") "plain name")))

(defun lang-web-test--title-at (text marker)
  "Test title at the position just after MARKER in TEXT, using typescript-ts-mode."
  (with-temp-buffer
    (insert text)
    (typescript-ts-mode)
    (goto-char (point-min))
    (search-forward marker)
    (ide-web--test-title-at-point)))

(ert-deftest web-test-title-at-point ()
  (skip-unless (treesit-language-available-p 'typescript))
  (let ((src (concat "describe('math', () => {\n"
                     "  it('adds numbers', () => {\n    expect(add(1, 2)).toBe(3);\n  });\n"
                     "  test.skip(\"subtracts\", () => { run(); });\n"
                     "});\n"
                     "const unrelated = foo('x');\n")))
    (should (equal (lang-web-test--title-at src "toBe") "adds numbers"))
    (should (equal (lang-web-test--title-at src "run") "subtracts"))
    (should (equal (lang-web-test--title-at src "describe(") "math"))
    (should (equal (lang-web-test--title-at src "'math'") "math"))
    (should-not (lang-web-test--title-at src "foo("))))

(ert-deftest web-counterpart-candidates ()
  (let ((c (ide-web--counterpart-candidates "/p/src/a.ts")))
    (should (equal (car c) "/p/src/a.test.ts"))
    (should (member "/p/src/a.spec.ts" c))
    (should (member "/p/src/__tests__/a.test.ts" c)))
  (let ((c (ide-web--counterpart-candidates "/p/src/__tests__/a.test.tsx")))
    (should (member "/p/src/__tests__/a.tsx" c))
    (should (member "/p/src/a.tsx" c)))
  (should (equal (car (ide-web--counterpart-candidates "/p/a.spec.js")) "/p/a.ts")))

(ert-deftest web-run-file-command ()
  (cl-letf (((symbol-function 'ide-lang-node-exe) (lambda (n &optional _) (concat "/bin/" n))))
    (should (equal (ide-web--run-file-command "/p/x y.ts") "/bin/tsx /p/x\\ y.ts"))
    (should (equal (ide-web--run-file-command "/p/a.mjs") "node /p/a.mjs"))))

(ert-deftest web-eslint-needs-a-config ()
  (lang-web-test--with-project '(("src/a.js" . "") ("package.json" . "{}"))
    (let ((buffer-file-name (expand-file-name "src/a.js" default-directory)))
      (should-not (ide-lang-config-file ide-web--eslint-configs))
      (write-region "export default [];" nil (expand-file-name "eslint.config.js" default-directory))
      (should (ide-lang-config-file ide-web--eslint-configs)))))

(ert-deftest web-eslint-output-becomes-diagnostics ()
  (with-temp-buffer
    (insert "const unused = 1\nexport const x = 2;\n")
    (let* ((json (concat "(node:1) Warning: noise\n"
                         "[{\"filePath\":\"/p/a.js\",\"messages\":["
                         "{\"ruleId\":\"no-unused-vars\",\"severity\":2,\"message\":\"'unused' is assigned a value but never used.\","
                         "\"line\":1,\"column\":7,\"endLine\":1,\"endColumn\":13},"
                         "{\"ruleId\":\"semi\",\"severity\":1,\"message\":\"Missing semicolon.\",\"line\":1,\"column\":17},"
                         "{\"ruleId\":null,\"severity\":1,\"message\":\"File ignored because of a matching ignore pattern.\"}"
                         "]}]\n"))
           (diags (ide-web--eslint-parse json (current-buffer))))
      (should (= (length diags) 2))
      (should (eq (flymake-diagnostic-type (car diags)) :error))
      (should (string-match-p "no-unused-vars" (flymake-diagnostic-text (car diags))))
      (should (equal (buffer-substring (flymake-diagnostic-beg (car diags)) (flymake-diagnostic-end (car diags)))
                     "unused"))
      (should (eq (flymake-diagnostic-type (cadr diags)) :warning)))
    (should-not (ide-web--eslint-parse "Oops: no config\n" (current-buffer)))))

(ert-deftest web-launch-plists-for-dap ()
  (lang-web-test--with-project '(("package.json" . "{}"))
    (let ((d default-directory))
      (cl-letf (((symbol-function 'ide-web--project-dir) (lambda () d))
                ((symbol-function 'ide-lang-node-exe) (lambda (n &optional _) (concat "/bin/" n))))
        (let ((ts (ide-web--file-launch "/p/a.ts")) (js (ide-web--file-launch "/p/a.js"))
              (script (ide-web--script-launch "dev")))
          (should (equal (plist-get ts :type) "pwa-node"))
          (should (equal (plist-get ts :runtimeExecutable) "/bin/tsx"))
          (should (equal (plist-get ts :program) "/p/a.ts"))
          (should-not (plist-get js :runtimeExecutable))
          (should (equal (plist-get script :runtimeArgs) ["run" "dev"]))
          (should (equal (plist-get script :runtimeExecutable) "npm")))))))

(ert-deftest web-tsc-errors-are-clickable ()
  (require 'compile)
  (let ((line "src/a.ts(1,7): error TS2322: Type 'string' is not assignable to type 'number'."))
    (should (cl-some (lambda (sym)
                       (string-match (cadr (assq sym compilation-error-regexp-alist-alist)) line))
                     '(typescript-tsc-plain)))
    (should (memq 'typescript-tsc-plain compilation-error-regexp-alist))))

(ert-deftest web-static-preview-url ()
  (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) "/p/")))
    (should (equal (ide-web--static-url "/p/pages/my page.html")
                   (format "http://localhost:%d/pages/my%%20page.html" ide-web-preview-port)))))

(ert-deftest web-native-tsc-detection ()
  (skip-unless (executable-find "tsc"))
  (let ((tsc (executable-find "tsc")))
    (should (eq (and (ide-web--tsc-native-p tsc) t)
                (with-temp-buffer
                  (call-process tsc nil t nil "--version")
                  (and (string-match-p "Version [7-9]" (buffer-string)) t))))))

(provide 'lang-web-test)
;;; lang-web-test.el ends here
