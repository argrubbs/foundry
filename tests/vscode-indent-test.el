;;; vscode-indent-test.el --- Tests for lisp/vscode-indent -*- lexical-binding: t; -*-

;;; Commentary:
;; Each test builds a buffer, runs a command and compares the text, with `|' marking point.
;; The expected results are what VS Code does with the same language configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../lisp/vscode-indent"
                                          (file-name-directory (or load-file-name buffer-file-name))))
(require 'vscode-indent)

(defmacro vsi-test--buffer (mode lang size text &rest body)
  "Run BODY in a MODE buffer using rule set LANG and indent size SIZE, holding TEXT.
TEXT marks point with `|'."
  (declare (indent 4))
  `(with-temp-buffer
     (insert ,text)
     (,mode)
     (setq-local vsi-language ,lang vsi-indent-size ,size tab-width ,size indent-tabs-mode nil)
     (goto-char (point-min))
     (search-forward "|")
     (delete-char -1)
     (vscode-indent-mode 1)
     ,@body))

(defun vsi-test--text ()
  "The buffer text with `|' at point."
  (concat (buffer-substring-no-properties (point-min) (point)) "|"
          (buffer-substring-no-properties (point) (point-max))))

(defun vsi-test--type (ch)
  "Type CH the way a user would, running `post-self-insert-hook'."
  (let ((last-command-event ch)) (self-insert-command 1)))

(defmacro vsi-test--enter (mode lang size text expected)
  "Press Enter in TEXT and check that the result is EXPECTED."
  `(vsi-test--buffer ,mode ,lang ,size ,text
     (vsi-newline)
     (should (equal ,expected (vsi-test--text)))))

;;;; Enter ---------------------------------------------------------------------------------

(ert-deftest vsi-enter-js-brace ()
  (vsi-test--enter js-mode 'javascript 4 "function f() {|" "function f() {\n    |")
  (vsi-test--enter js-mode 'javascript 4 "if (x) {|}" "if (x) {\n    |\n}")
  (vsi-test--enter js-mode 'javascript 4 "  foo(|" "  foo(\n      |")
  (vsi-test--enter js-mode 'javascript 4 "  foo();|" "  foo();\n  |"))

(ert-deftest vsi-enter-js-indent-next-line ()
  "`if (x)' without a brace indents one line only."
  (vsi-test--enter js-mode 'javascript 4 "if (x)|" "if (x)\n    |")
  (vsi-test--enter js-mode 'javascript 4 "if (x)\n    foo();|" "if (x)\n    foo();\n|")
  (vsi-test--enter js-mode 'javascript 4 "const f = () =>|" "const f = () =>\n    |"))

(ert-deftest vsi-enter-js-ignores-brackets-in-strings ()
  (vsi-test--enter js-mode 'javascript 4 "  const s = \"{\";|" "  const s = \"{\";\n  |")
  (vsi-test--enter js-mode 'javascript 4 "  // see {|" "  // see {\n  |"))

(ert-deftest vsi-enter-js-doc-comment ()
  (vsi-test--enter js-mode 'javascript 4 "/**|" "/**\n * |")
  (vsi-test--enter js-mode 'javascript 4 "/**\n * text|" "/**\n * text\n * |")
  (vsi-test--enter js-mode 'javascript 4 "/**| */" "/**\n * |\n */")
  (vsi-test--enter js-mode 'javascript 4 "  /** text\n   */|" "  /** text\n   */\n  |"))

(ert-deftest vsi-enter-js-line-comment-continues ()
  "A `//' comment continues only when there is text after the cursor."
  (vsi-test--enter js-mode 'javascript 4 "  // one |two" "  // one \n  // |two")
  (vsi-test--enter js-mode 'javascript 4 "  // one two|" "  // one two\n  |"))

(ert-deftest vsi-enter-js-switch ()
  (vsi-test--enter js-mode 'javascript 4 "switch (x) {\n    case 1:|" "switch (x) {\n    case 1:\n        |"))

(ert-deftest vsi-enter-python ()
  (vsi-test--enter python-mode 'python 4 "def f():|" "def f():\n    |")
  (vsi-test--enter python-mode 'python 4 "    if x:|" "    if x:\n        |")
  (vsi-test--enter python-mode 'python 4 "    return 1|" "    return 1\n    |")
  (vsi-test--enter python-mode 'python 4 "x = foo(|" "x = foo(\n    |")
  (vsi-test--enter python-mode 'python 4 "x = [|]" "x = [\n    |\n]"))

(ert-deftest vsi-enter-yaml-keeps-indentation ()
  "YAML runs at `advanced': Enter copies the indentation and does not indent after a colon."
  (vsi-test--enter prog-mode 'yaml 2 "key:|" "key:\n|")
  (vsi-test--enter prog-mode 'yaml 2 "  - name: x|" "  - name: x\n  |")
  (vsi-test--enter prog-mode 'yaml 2 "list: [|" "list: [\n  |")
  (vsi-test--enter prog-mode 'yaml 2 "map: {|}" "map: {\n  |\n}"))

(ert-deftest vsi-enter-yaml-full-level ()
  "At `full' the YAML indent pattern applies after a line ending in a colon."
  (let ((vsi-auto-indent-alist '((yaml . full))))
    (vsi-test--enter prog-mode 'yaml 2 "key:|" "key:\n  |")))

(ert-deftest vsi-enter-json ()
  (vsi-test--enter js-mode 'json 2 "{|" "{\n  |")
  (vsi-test--enter js-mode 'json 2 "{\n  \"a\": [|" "{\n  \"a\": [\n    |")
  (vsi-test--enter js-mode 'json 2 "{\n  \"a\": \"{\",|" "{\n  \"a\": \"{\",\n  |")
  (vsi-test--enter js-mode 'json 2 "{\n  \"a\": 1\n}|" "{\n  \"a\": 1\n}\n|"))

(ert-deftest vsi-enter-go-and-rust ()
  (vsi-test--enter prog-mode 'go 4 "func main() {|" "func main() {\n    |")
  (vsi-test--enter prog-mode 'go 4 "    case 1:|" "    case 1:\n        |")
  (vsi-test--enter prog-mode 'rust 4 "fn main() {|" "fn main() {\n    |"))

(ert-deftest vsi-enter-lua ()
  (vsi-test--enter prog-mode 'lua 2 "function f()|" "function f()\n  |")
  (vsi-test--enter prog-mode 'lua 2 "  if x then|" "  if x then\n    |")
  (vsi-test--enter prog-mode 'lua 2 "  x = 1 -- then|" "  x = 1 -- then\n  |")
  (vsi-test--enter prog-mode 'lua 2 "  if x then y() end|" "  if x then y() end\n  |"))

(ert-deftest vsi-enter-css ()
  (vsi-test--enter prog-mode 'css 2 "a {|" "a {\n  |")
  (vsi-test--enter prog-mode 'css 2 "  color: red;|" "  color: red;\n  |"))

(ert-deftest vsi-enter-html ()
  (vsi-test--enter prog-mode 'html 2 "<div>|" "<div>\n  |")
  (vsi-test--enter prog-mode 'html 2 "<div>|</div>" "<div>\n  |\n</div>")
  (vsi-test--enter prog-mode 'html 2 "  <br>|" "  <br>\n  |")
  (vsi-test--enter prog-mode 'html 2 "  <p>hi</p>|" "  <p>hi</p>\n  |"))

(ert-deftest vsi-enter-no-rules-languages ()
  "Languages without indentation rules keep the indentation and honour brackets."
  (vsi-test--enter prog-mode 'powershell 4 "    $x = 1|" "    $x = 1\n    |")
  (vsi-test--enter prog-mode 'powershell 4 "    foreach ($i in $a) {|" "    foreach ($i in $a) {\n        |")
  (vsi-test--enter prog-mode 'powershell 4 "    foreach ($i in $a) {|}" "    foreach ($i in $a) {\n        |\n    }"))

(ert-deftest vsi-enter-levels ()
  (let ((vsi-auto-indent-alist '((javascript . none))))
    (vsi-test--enter js-mode 'javascript 4 "  foo {|" "  foo {\n|"))
  (let ((vsi-auto-indent-alist '((javascript . keep))))
    (vsi-test--enter js-mode 'javascript 4 "  foo {|" "  foo {\n  |"))
  (let ((vsi-auto-indent-alist '((javascript . brackets))))
    (vsi-test--enter js-mode 'javascript 4 "  foo {|" "  foo {\n      |")
    (vsi-test--enter js-mode 'javascript 4 "  /**|" "  /**\n  |")))

(ert-deftest vsi-enter-inside-indentation ()
  "Enter inside the leading whitespace moves the text down, indented, and keeps the column."
  (vsi-test--enter js-mode 'javascript 4 "function f() {\n  | foo();\n}" "function f() {\n  \n  |  foo();\n}"))

(ert-deftest vsi-enter-trims-unused-indentation ()
  (vsi-test--buffer js-mode 'javascript 4 "function f() {|"
    (vsi-newline)
    (should (equal "function f() {\n    " (buffer-string)))
    (vsi--trim-auto-ws)
    (should (equal "function f() {\n    " (buffer-string)))
    (goto-char (point-min))
    (vsi--trim-auto-ws)
    (should (equal "function f() {\n" (buffer-string)))))

;;;; Typing --------------------------------------------------------------------------------

(ert-deftest vsi-type-outdents-closing-brace ()
  (vsi-test--buffer js-mode 'javascript 4 "function f() {\n    foo();\n    |"
    (vsi-test--type ?})
    (should (equal "function f() {\n    foo();\n}|" (vsi-test--text)))))

(ert-deftest vsi-type-leaves-deliberate-indentation ()
  "A line that already matched the outdent rule is not re-indented when you type more."
  (vsi-test--buffer js-mode 'javascript 4 "function f() {\n        }|"
    (vsi-test--type ?\;)
    (should (equal "function f() {\n        };|" (vsi-test--text)))))

(ert-deftest vsi-type-lua-end ()
  (vsi-test--buffer prog-mode 'lua 2 "function f()\n  x()\n  en|"
    (vsi-test--type ?d)
    (should (equal "function f()\n  x()\nend|" (vsi-test--text)))))

(ert-deftest vsi-type-closing-bracket-lines-up-with-opener ()
  "Python has no indent rules; a closing bracket still lines up with the line of its opener."
  (vsi-test--buffer python-mode 'python 4 "x = foo(\n    1,\n    |"
    (vsi-test--type ?\))
    (should (equal "x = foo(\n    1,\n)|" (vsi-test--text)))))

(ert-deftest vsi-type-skips-strings ()
  (vsi-test--buffer js-mode 'javascript 4 "const s = `\n    |"
    (vsi-test--type ?})
    (should (equal "const s = `\n    }|" (vsi-test--text)))))

;;;; Tab -----------------------------------------------------------------------------------

(ert-deftest vsi-tab-blank-line-goes-where-code-belongs ()
  (vsi-test--buffer js-mode 'javascript 4 "function f() {\n|"
    (vsi-tab)
    (should (equal "function f() {\n    |" (vsi-test--text)))
    (vsi-tab)
    (should (equal "function f() {\n        |" (vsi-test--text)))))

(ert-deftest vsi-tab-mid-line-moves-to-the-next-stop ()
  (vsi-test--buffer js-mode 'javascript 4 "ab|c"
    (vsi-tab)
    (should (equal "ab  |c" (vsi-test--text)))
    (vsi-tab)
    (should (equal "ab      |c" (vsi-test--text)))))

(ert-deftest vsi-tab-does-not-reindent-the-line ()
  (vsi-test--buffer js-mode 'javascript 4 "function f() {\n        foo|();\n}"
    (vsi-tab)
    (should (equal "function f() {\n        foo |();\n}" (vsi-test--text)))))

(ert-deftest vsi-tab-shifts-selected-lines ()
  (vsi-test--buffer js-mode 'javascript 4 "|a\nb\n\nc"
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (vsi-tab)
    ;; Several lines selected: all shift, the empty line stays empty.
    (should (equal "    a\n    b\n\n    c" (buffer-string)))
    (vsi-backtab)
    (should (equal "a\nb\n\nc" (buffer-string)))))

(ert-deftest vsi-tab-snaps-to-stops ()
  (vsi-test--buffer js-mode 'javascript 4 "  |a"
    (vsi-tab)
    (should (equal "    a" (buffer-string)))))

(ert-deftest vsi-backtab-single-line ()
  (vsi-test--buffer js-mode 'javascript 4 "        fo|o"
    (vsi-backtab)
    (should (equal "    foo" (buffer-string)))
    (should (eq (point) (+ (point-min) 6)))))

;;;; Reindent and paste --------------------------------------------------------------------

(ert-deftest vsi-reindent-js ()
  (vsi-test--buffer js-mode 'javascript 4 "|function f() {\nfoo();\nif (x) {\n        bar();\n}\n}"
    (vsi-reindent-lines (point-min) (point-max))
    (should (equal "function f() {\n    foo();\n    if (x) {\n        bar();\n    }\n}" (buffer-string)))))

(ert-deftest vsi-reindent-leaves-strings-alone ()
  (vsi-test--buffer js-mode 'javascript 4 "|function f() {\nconst s = `\n   keep\n`;\n}"
    (vsi-reindent-lines (point-min) (point-max))
    (should (string-match-p "\n   keep\n" (buffer-string)))))

(ert-deftest vsi-paste-reindents-block ()
  (vsi-test--buffer js-mode 'javascript 4 "function f() {\n    |\n}"
    (let ((beg (point)))
      (insert "if (x) {\n            foo();\n        }")
      (vsi-indent-pasted beg (point))
      (should (equal "function f() {\n    if (x) {\n        foo();\n    }\n}" (buffer-string))))))

;;;; Guessing ------------------------------------------------------------------------------

(ert-deftest vsi-guess-spaces ()
  (with-temp-buffer
    (insert "a:\n  b:\n    c: 1\n    d: 2\n  e: 3\n")
    (should (equal '(2 . t) (vsi-guess-indentation 4 t))))
  (with-temp-buffer
    (insert "def f():\n    if x:\n        y\n    z\n")
    (should (equal '(4 . t) (vsi-guess-indentation 2 t)))))

(ert-deftest vsi-guess-tabs-and-defaults ()
  (with-temp-buffer
    (insert "a {\n\tb;\n\tc {\n\t\td;\n\t}\n}\n")
    (should (equal 8 (car (vsi-guess-indentation 8 nil))))
    (should-not (cdr (vsi-guess-indentation 4 t))))
  (with-temp-buffer
    (insert "no indentation here\n")
    (should (equal '(3 . t) (vsi-guess-indentation 3 t)))))

(ert-deftest vsi-guess-ignores-alignment ()
  (with-temp-buffer
    (insert "const a = 1,\n      b = 2;\nfoo(\n  1);\n")
    (should (equal 2 (car (vsi-guess-indentation 4 t))))))

(ert-deftest vsi-guess-prefers-two-over-four-when-close ()
  (with-temp-buffer
    (insert "a:\n  b:\n    c:\n      d: 1\n  e:\n      f: 2\n")
    (should (equal 2 (car (vsi-guess-indentation 4 t))))))

;;;; Registry ------------------------------------------------------------------------------

(ert-deftest vsi-languages-resolve-from-the-mode ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (should-not (vsi--known-mode-p)))
  (with-temp-buffer
    (python-mode)
    (should (vsi--known-mode-p))
    (should (eq 'python (vsi-lang-name (vsi--lang))))))

(ert-deftest vsi-size-defaults-to-the-languages-offset ()
  "With no detected size, a language uses its own offset variable, then `tab-width'."
  (with-temp-buffer
    (setq-local vsi-language 'python tab-width 8)
    (let ((python-indent-offset 4)) (should (= 4 (vsi--size))))
    (let ((python-indent-offset nil)) (should (= 8 (vsi--size))))
    (setq-local vsi-indent-size 3)
    (should (= 3 (vsi--size)))))

(ert-deftest vsi-yaml-tree-sitter-string-contents ()
  "A bracket inside a quoted YAML scalar is not a bracket (that node has no leaf)."
  (skip-unless (and (fboundp 'yaml-ts-mode) (treesit-ready-p 'yaml t)))
  (with-temp-buffer
    (insert "a: \"{\"")
    (yaml-ts-mode)
    (should (vsi--in-string-or-comment-p 5))
    (should-not (vsi--in-string-or-comment-p 2))))

(provide 'vscode-indent-test)
;;; vscode-indent-test.el ends here
