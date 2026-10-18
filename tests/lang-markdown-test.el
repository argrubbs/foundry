;;; lang-markdown-test.el --- Tests for modules/11-languages/lang-markdown.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-markdown-test.el -f ert-run-tests-batch-and-exit
;; Pure logic plus pandoc, prettier and markdownlint-cli2 where installed.  The preview in
;; the xwidget browser, marksman and the clipboard were checked by hand.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst md-test--doc
  "---
title: T
---

# Title

## Install `now`

Text [guide](other.md#sec) and [gone](nope.md) and [[wiki]].

```python
# not a heading
print(1)
```

Setext
------

### Deep [link](http://x.org)

## Install `now`

- [ ] a
- [x] b
")

(defmacro md-test--with (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer (insert ,text) (markdown-mode) (goto-char (point-min)) ,@body))

(ert-deftest markdown-fences-and-headings-ignore-code ()
  (md-test--with md-test--doc
    (should (equal (mapcar (lambda (b) (plist-get b :lang)) (ide-markdown--fences)) '("python")))
    (let ((heads (ide-markdown--headings)))
      (should (equal (mapcar #'car heads) '(1 2 2 3 2)))
      (should (equal (mapcar (lambda (h) (nth 3 h)) heads)
                     '("title" "install-now" "setext" "deep-link" "install-now-1")))
      (should-not (member "not a heading" (mapcar #'cadr heads))))))

(ert-deftest markdown-fence-at-point ()
  (md-test--with md-test--doc
    (search-forward "print")
    (let ((b (ide-markdown--fence-at)))
      (should (equal (plist-get b :lang) "python"))
      (should (equal (buffer-substring (plist-get b :body-start) (plist-get b :body-end))
                     "# not a heading\nprint(1)\n")))
    (goto-char (point-min))
    (should-not (ide-markdown--fence-at))))

(ert-deftest markdown-fence-info-forms ()
  (should (equal (ide-markdown--fence-info " python title=x") "python"))
  (should (equal (ide-markdown--fence-info "{.Python}") "python"))
  (should-not (ide-markdown--fence-info "")))

(ert-deftest markdown-slugs ()
  (should (equal (ide-markdown--slug "Hello, World! (v2)") "hello-world-v2"))
  (should (equal (ide-markdown--slug "The `code` and [a link](x)") "the-code-and-a-link"))
  (should (equal (ide-markdown--slug "snake_case Ünï") "snake_case-ünï")))

(ert-deftest markdown-link-targets-skip-code-and-footnotes ()
  (md-test--with "[a](x.md) `[b](y.md)`\n\n```\n[c](z.md)\n```\n\n[^1]: note\n[ref]: r.md\n[[w|alias]]\n![img](<my img.png>)\n"
    (should (equal (mapcar #'caddr (ide-markdown--link-targets))
                   '("x.md" "r.md" "w" "my img.png")))))

(defun md-test--project ()
  "A scratch tree with two notes; returns its root."
  (let ((root (make-temp-file "mdproj" t)))
    (make-directory (expand-file-name "docs" root))
    (with-temp-file (expand-file-name "README.md" root)
      (insert "# R\n\n[ok](docs/guide.md#setup) [anchor](docs/guide.md#nope) [gone](docs/x.md)\n"
              "[web](https://e.org) [self](#r) [selfbad](#zz) [[guide]] [[ghost]] [abs](/docs/guide.md)\n"))
    (with-temp-file (expand-file-name "docs/guide.md" root)
      (insert "# Guide\n\n## Setup\n\n[up](../README.md#r)\n"))
    root))

(ert-deftest markdown-check-links-finds-broken-ones ()
  (let* ((root (md-test--project)) (default-directory root))
    (unwind-protect
        (let ((n (ide-markdown-check-links))
              (report (with-current-buffer "*run: markdown links*" (buffer-string))))
          (should (= n 4))
          (should (string-match-p "README.md:3:.*docs/x.md does not exist" report))
          (should (string-match-p "README.md:3:.*no heading #nope" report))
          (should (string-match-p "README.md:4:.*no heading #zz" report))
          (should (string-match-p "broken wiki link \\[\\[ghost\\]\\]" report))
          (should-not (string-match-p "guide.md:" (replace-regexp-in-string "docs/guide.md#" "" report))))
      (when (get-buffer "*run: markdown links*") (kill-buffer "*run: markdown links*"))
      (delete-directory root t))))

(ert-deftest markdown-word-count-ignores-code-and-front-matter ()
  (md-test--with "---\ntitle: a b c\n---\n\none two [three four](http://u.rl)\n\n```\nfive six\n```\n\n<!-- seven -->\n"
    (let ((inhibit-message t)) (should (= (ide-markdown-word-count) 4)))))

(ert-deftest markdown-insert-code-block-wraps-region ()
  (md-test--with "x = 1\n"
    (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
    (ide-markdown-insert-code-block "python")
    (should (equal (buffer-string) "```python\nx = 1\n```\n")))
  (should (member "python" (ide-markdown--code-languages))))

(ert-deftest markdown-align-tables-skips-code ()
  (md-test--with "|a|b|\n|-|-|\n|long cell|x|\n\n```\n|a|b|\n```\n"
    (let ((inhibit-message t)) (ide-markdown-align-tables))
    (should (string-match-p "| a  *| b *|" (buffer-string)))
    (should (string-match-p "```\n|a|b|\n```" (buffer-string)))))

(ert-deftest markdown-orgtbl-keeps-gfm-rules ()
  (md-test--with "| a | b |\n|---|---|\n| longer | x |\n"
    (ide-markdown-orgtbl-mode 1)
    (goto-char (point-min))
    (org-table-align)
    (should (string-match-p "^|-+|-+|$" (buffer-string)))
    (should-not (string-match-p "+" (buffer-string)))
    (ide-markdown-orgtbl-mode -1)))

(ert-deftest markdown-lint-config-detection ()
  (let* ((root (make-temp-file "mdlint" t)) (sub (expand-file-name "a/b/" root)))
    (unwind-protect
        (progn
          (make-directory sub t)
          (should-not (ide-markdown--lint-config sub))
          (with-temp-file (expand-file-name ".markdownlint.json" root) (insert "{}"))
          (should (equal (ide-markdown--lint-config sub) (expand-file-name ".markdownlint.json" root)))
          (should (file-exists-p (ide-markdown--relaxed-config))))
      (delete-directory root t))))

(ert-deftest markdown-lint-backend-runs-markdownlint ()
  (skip-unless (executable-find "markdownlint-cli2"))
  (let ((root (make-temp-file "mdlint" t)))
    (unwind-protect
        (let ((default-directory root) (trusted-content :all) res)  ; markdownlint waits for a trusted project
          (with-temp-buffer
            (setq buffer-file-name (expand-file-name "a.md" root))
            (insert "# T\n\n### Skipped   \n\n" (make-string 150 ?x) "\n")
            (markdown-mode)
            (ide-lang-flymake-markdownlint (lambda (d &rest _) (setq res (or d 'none))))
            (let ((n 0)) (while (and (null res) (< n 150)) (accept-process-output nil 0.1) (cl-incf n)))
            (let ((texts (mapcar #'flymake-diagnostic-text res)))
              (should (cl-some (lambda (s) (string-match-p "MD001" s)) texts))
              (should (cl-some (lambda (s) (string-match-p "MD009" s)) texts))
              ;; The relaxed default turns line length off.
              (should-not (cl-some (lambda (s) (string-match-p "MD013" s)) texts))
              (set-buffer-modified-p nil))))
      (delete-directory root t))))

(ert-deftest markdown-lint-fix-applies-fixes ()
  (skip-unless (executable-find "markdownlint-cli2"))
  (md-test--with "# T\n\ntrailing   \nline\n\n\n\nmuch\n"
    (let ((default-directory temporary-file-directory) (inhibit-message t))
      (ide-markdown-lint-fix))
    (should (equal (buffer-string) "# T\n\ntrailing\nline\n\nmuch\n"))))

(ert-deftest markdown-prettier-preserves-prose-wrap ()
  (skip-unless (executable-find "prettier"))
  (with-temp-buffer
    (insert "# T\n\n*  a long line that must not be rewrapped even when it is quite long indeed, longer than eighty columns\n*  b\n")
    (let ((status (call-process-region (point-min) (point-max) "prettier" t t nil
                                       "--stdin-filepath" "x.md" "--prose-wrap" "preserve")))
      (should (zerop status))
      (should (equal (buffer-string)
                     "# T\n\n- a long line that must not be rewrapped even when it is quite long indeed, longer than eighty columns\n- b\n")))))

(ert-deftest markdown-apheleia-formatter-registered ()
  (require 'apheleia)
  (should (member "prettier" (alist-get 'ide-markdown-prettier apheleia-formatters)))
  (should (member "preserve" (alist-get 'ide-markdown-prettier apheleia-formatters)))
  (should (eq (alist-get 'gfm-mode apheleia-mode-alist) 'ide-markdown-prettier)))

(ert-deftest markdown-theme-palette-and-css ()
  (let ((pal (ide-markdown--palette)))
    (dolist (k '(:bg :fg :link :head :muted :codebg :border :quote))
      (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" (plist-get pal k))))
    (should (string-match-p "color-scheme:\\(light\\|dark\\)" (ide-markdown--css pal)))
    (let ((json (json-parse-string (ide-markdown--theme-json pal) :object-type 'alist)))
      (should (alist-get 'text-styles json))
      (should (alist-get 'Keyword (alist-get 'text-styles json))))))

(ert-deftest markdown-dark-palette-follows-theme ()
  (let ((orig-bg (face-attribute 'default :background)) (orig-fg (face-attribute 'default :foreground)))
    (unwind-protect
        (progn
          (set-face-attribute 'default nil :background "#101010" :foreground "#eeeeee")
          (should (plist-get (ide-markdown--palette) :dark))
          (set-face-attribute 'default nil :background "#ffffff" :foreground "#111111")
          (should-not (plist-get (ide-markdown--palette) :dark)))
      (set-face-attribute 'default nil :background orig-bg :foreground orig-fg))))

(ert-deftest markdown-pandoc-renders-themed-html ()
  (skip-unless (executable-find "pandoc"))
  (let* ((root (make-temp-file "mdprev" t)) (default-directory root) done)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "p.md" root))
          (insert "# Hi\n\ntext $x^2$\n\n```python\nprint(1)\n```\n\n- [x] done\n")
          (markdown-mode)
          (ide-markdown--render (lambda (f) (setq done f)))
          (let ((n 0)) (while (and (null done) (< n 200)) (accept-process-output nil 0.1) (cl-incf n)))
          (should done)
          (set-buffer-modified-p nil)
          (let ((html (with-temp-buffer (insert-file-contents done) (buffer-string))))
            (should (string-match-p "<math" html))
            (should (string-match-p "data-pos=\"1:1" html))
            (should (string-match-p "mdscroll" html))
            (should (string-match-p "color-scheme" html))
            (should (string-match-p "code span\\.kw\\|span\\.kw" html))
            (should (string-match-p "type=\"checkbox\"" html))))
      (delete-directory root t))))

(ert-deftest markdown-org-conversion ()
  (skip-unless (executable-find "pandoc"))
  (let ((org (ide-markdown--md-to-org "# H\n\n- [ ] a\n- [x] b\n\n| a | b |\n|---|---|\n| 1 | 2 |\n\n[l](http://x.org) `c` **b**\n\n```python\nx\n```\n")))
    (should (string-match-p "^\\* H$" org))
    (should-not (string-match-p ":PROPERTIES:" org))
    (should (string-match-p "- \\[ \\] a" org))
    (should (string-match-p "- \\[X\\] b" org))
    (should (string-match-p "\\[\\[http://x.org\\]\\[l\\]\\]" org))
    (should (string-match-p "=c=" org))
    (should (string-match-p "#\\+begin_src python" org)))
  (let ((md (ide-markdown--org-to-md "* TODO H :t:\n:PROPERTIES:\n:ID: 1\n:END:\ntext =c= [[http://x.org][l]]\n#+begin_src sh\nls\n#+end_src\n")))
    (should (string-match-p "^# TODO H" md))
    (should (string-match-p "`c`" md))
    (should (string-match-p "\\[l\\](http://x.org)" md))
    ;; Lossy by nature: properties drawers do not survive.
    (should-not (string-match-p "PROPERTIES\\|:ID:" md))))

(ert-deftest markdown-round-trip-keeps-structure ()
  (skip-unless (executable-find "pandoc"))
  (let* ((md "# Title\n\n## Sec\n\n- one\n- two\n\n1. a\n2. b\n\n> quote\n\n`code` and [l](http://x.org)\n")
         (back (ide-markdown--org-to-md (ide-markdown--md-to-org md))))
    (dolist (frag '("# Title" "## Sec" "- one" "1. a" "> quote" "`code`" "[l](http://x.org)"))
      (should (string-match-p (if (equal frag "1. a") "1\\. +a" (regexp-quote frag)) back)))))

(ert-deftest markdown-region-to-org-replaces-in-place ()
  (skip-unless (executable-find "pandoc"))
  (with-temp-buffer
    (insert "## Head\n\n**b**\n")
    (ide-markdown-region-to-org (point-min) (point-max))
    (should (equal (buffer-string) "** Head\n*b*\n"))))

(ert-deftest markdown-html-clipboard-conversion ()
  (skip-unless (executable-find "pandoc"))
  (should (string-match-p "\\[\\[http://x\\]\\[c\\]\\]" (ide-markdown--html-to "<p><a href='http://x'>c</a></p>" "org")))
  (should (string-match-p "\\*\\*b\\*\\*" (ide-markdown--html-to "<p><b>b</b></p>" "gfm"))))

(ert-deftest markdown-clipboard-paste-falls-back-to-kill-ring ()
  (skip-unless (executable-find "pandoc"))
  (cl-letf (((symbol-function 'ide-markdown--clipboard-types) (lambda () nil)))
    (with-temp-buffer
      (kill-new "# From clipboard")
      (ide-org-paste-markdown)
      (should (string-match-p "^\\* From clipboard" (buffer-string))))))

(ert-deftest markdown-export-refuses-pdf-without-engine ()
  (skip-unless (not (ide-markdown--pdf-engine)))
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "x.md" temporary-file-directory))
    (cl-letf (((symbol-function 'save-buffer) #'ignore))
      (should-error (ide-markdown-export "pdf") :type 'user-error))
    (set-buffer-modified-p nil)))

(ert-deftest markdown-run-block-languages ()
  (should (ide-markdown--run-spec "bash"))
  (should-not (ide-markdown--run-spec "cobol"))
  (should (equal (car (ide-markdown--run-spec "sql")) '("sqlite3" "-header" "-column" ":memory:" ".read"))))

(ert-deftest markdown-run-block-asks-before-running ()
  (skip-unless (executable-find "bash"))
  (md-test--with "```bash\necho hi\n```\n"
    (let (asked)
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) (setq asked t) nil)))
        (should-error (ide-markdown--block-command (ide-markdown--fence-at (point))) :type 'user-error))
      (should asked))))

(ert-deftest markdown-run-block-inserts-output-below ()
  (skip-unless (executable-find "bash"))
  (let ((root (make-temp-file "mdrun" t)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name (expand-file-name "r.md" root))
          (insert "```bash\necho hello-from-block\n```\n\ntail\n")
          (markdown-mode)
          (goto-char 5)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (ide-markdown-run-block t))
          (let ((n 0)) (while (and (not (string-match-p "```output" (buffer-string))) (< n 100))
                         (accept-process-output nil 0.1) (cl-incf n)))
          (should (string-match-p "```bash\necho hello-from-block\n```\n\n```output\nhello-from-block\n```" (buffer-string)))
          (should (string-match-p "\ntail\n" (buffer-string)))
          ;; Running again replaces the previous output instead of stacking.
          (goto-char 5)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
            (ide-markdown-run-block t))
          (let ((n 0)) (while (< n 15) (accept-process-output nil 0.1) (cl-incf n)))
          (should (= 1 (cl-count-if (lambda (b) (equal (plist-get b :lang) "output")) (ide-markdown--fences))))
          (set-buffer-modified-p nil))
      (delete-directory root t))))

(ert-deftest markdown-run-hook-offers-block-or-lint ()
  (skip-unless (executable-find "bash"))
  (md-test--with "# T\n\n```bash\necho 1\n```\n"
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_) t)))
      (search-forward "echo")
      (should (string-match-p "\\`bash .*block-[0-9]+\\.sh\\'" (ide-markdown--run-command))))))

(ert-deftest markdown-paste-image-needs-an-image ()
  (cl-letf (((symbol-function 'ide-markdown--clipboard-types) (lambda () '("text/plain"))))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "i.md" temporary-file-directory))
      (should-error (ide-markdown-paste-image "alt") :type 'user-error)
      (set-buffer-modified-p nil))))

(ert-deftest markdown-lsp-start-policy ()
  (with-temp-buffer
    (setq buffer-file-name (expand-file-name "x.md" temporary-file-directory))
    (let ((ide-markdown-lsp nil)) (should-not (ide-markdown--lsp-wanted-p)))
    (let ((ide-markdown-lsp 'auto) (default-directory (expand-file-name "~/"))) (should-not (ide-markdown--lsp-wanted-p)))
    (set-buffer-modified-p nil)))

(provide 'lang-markdown-test)
;;; lang-markdown-test.el ends here
