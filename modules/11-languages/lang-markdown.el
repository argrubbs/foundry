;;; lang-markdown.el --- Markdown: marksman, markdownlint, prettier, pandoc preview, Org interop -*- lexical-binding: t; -*-

;;; Commentary:
;; Markdown is everywhere (READMEs, wikis, issues, docs) even though Org is home here.
;;
;;   mode      markdown-mode (gfm-mode for README, CHANGELOG, CONTRIBUTING, .github/**).  Emacs 31's
;;             markdown-ts-mode was tried: it is good (table editing, outline cycling, checkbox
;;             toggling, subtree moves, fenced-block injection) but has no footnotes, wiki links,
;;             link/image insertion, GFM variant, math or pandoc-style front matter, which are
;;             what READMEs, wikis and marksman notes use; markdown-mode has them all, and its
;;             fenced blocks are fontified natively through the tree-sitter modes
;;             (python-ts-mode ...).  M-x markdown-ts-mode stays available per buffer.
;;             05-treesitter remaps markdown-mode to markdown-ts-mode; this file removes that
;;             remap.  Prose soft-wraps (visual-line-mode); fill-column stays 100 for M-q.
;;             .mdx is not associated: JSX renders as noise in markdown-mode and the linter
;;             and formatter would mangle it.
;;   server    marksman: link/heading completion, rename, references, broken-link diagnostics,
;;             table-of-contents code action.  Started automatically inside a project or next
;;             to a .marksman.toml (never for a stray file in $HOME).
;;   lint      markdownlint-cli2 as a Flymake backend (stdin, so unsaved edits are checked),
;;             honouring .markdownlint*/.markdownlint-cli2*; without a project config the
;;             line-length rule is off.  `ide-markdown-lint-fix' applies the autofixes.
;;   format    prettier through Apheleia with --prose-wrap preserve (prose is never rewrapped);
;;             table alignment and list renumbering commands.
;;   preview   pandoc renders GFM to HTML with the colours of the current Emacs theme (light or
;;             dark), syntax-highlighted code, MathML maths; shown in the in-Emacs browser beside
;;             the editor, refreshed on save or while idle, scrolled to the cursor line.
;;             Also: open the rendered file externally, export to HTML/DOCX/PDF/EPUB/... .
;;   Org       Markdown <-> Org conversion through pandoc (region, buffer, new buffer, file,
;;             clipboard), Org export to GFM, and Org-style editing inside Markdown (orgtbl,
;;             TAB folding, task toggling).  The round trip is lossy, see the docstrings.
;;   author    TOC, code blocks by grammar, paste an image from the clipboard, word count,
;;             relative-link checker, task toggling, spell checking when it works here.
;;   run       C-u/S-<f10> runs the fenced block at point (after a confirmation) through the
;;             interpreters found on PATH; the output goes to the drawer or below the block.
;;
;; C-c m opens the menu.  Tools (pandoc, marksman, markdownlint-cli2, prettier) come from mise.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'transient)
(require 'project)
(require 'color)
(require 'url-util)

(defvar ide-cache-dir)
(defvar ide-var-dir)
(defvar ide-eglot-format-on-save)
(defvar ide-run-command-functions)
(defvar markdown-mode-map)
(defvar markdown-mode-command-map)
(defvar markdown-code-lang-modes)
(defvar org-mode-map)
(defvar xwidget-webkit-last-session-buffer)
(defvar org-export-with-broken-links)
(defvar ispell-personal-dictionary)
(declare-function apheleia-mode "apheleia")
(declare-function eglot-ensure "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-rename "eglot")
(declare-function eglot-shutdown "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function markdown-mode "markdown-mode")
(declare-function gfm-mode "markdown-mode")
(declare-function markdown-table-align "markdown-mode")
(declare-function markdown-table-at-point-p "markdown-mode")
(declare-function markdown-cleanup-list-numbers "markdown-mode")
(declare-function markdown-cycle "markdown-mode")
(declare-function markdown-promote-subtree "markdown-mode")
(declare-function markdown-demote-subtree "markdown-mode")
(declare-function markdown-follow-thing-at-point "markdown-mode")
(declare-function xwidget-webkit-reload "xwidget")
(declare-function xwidget-webkit-goto-uri "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function xwidget-webkit-execute-script "xwidget")
(declare-function xwidget-webkit-uri "xwidget")
(declare-function orgtbl-mode "org-table")
(declare-function org-table-align "org-table")
(declare-function org-mode "org")
(declare-function org-export-to-file "ox")
(declare-function org-export-backend-p "ox")
(require '00-lang-lib)
(require 'treesit)

(defgroup ide-markdown nil "Markdown support." :group 'ide-lang)

(defcustom ide-markdown-fill-column 100
  "Column used by the hard-wrap commands (M-q); prose is shown soft-wrapped."
  :type 'integer)

(defcustom ide-markdown-preview-auto 'save
  "When the preview refreshes: `save', `idle' (about a second after typing) or nil."
  :type '(choice (const save) (const idle) (const :tag "manual" nil)))

(defcustom ide-markdown-preview-follow t
  "When non-nil the preview scrolls to the heading or block around point."
  :type 'boolean)

(defcustom ide-markdown-toc-depth 3 "Deepest heading level in the generated TOC." :type 'integer)

(defcustom ide-markdown-toc-update-on-save t
  "Refresh the table of contents (between <!-- toc --> markers) before saving."
  :type 'boolean)

(defcustom ide-markdown-image-dir "assets"
  "Folder, relative to the Markdown file, that pasted clipboard images go into."
  :type 'string)

(defcustom ide-markdown-words-per-minute 230 "Reading speed for `ide-markdown-word-count'." :type 'integer)

(defcustom ide-markdown-lsp 'auto
  "Start marksman with the buffer: `auto' (in a project or next to .marksman.toml),
t (always) or nil (only on M-x eglot)."
  :type '(choice (const auto) (const t) (const nil)))

(defcustom ide-markdown-format-on-save t "Format Markdown buffers with prettier on save." :type 'boolean)

(defcustom ide-markdown-spell-on-open nil "Turn Flyspell on in Markdown buffers." :type 'boolean)

(defconst ide-markdown-pandoc-reader "gfm+wikilinks_title_after_pipe"
  "Pandoc reader for Markdown: GitHub-flavoured, with [[target|title]] wiki links.")

;;;; Mode, server, formatting -------------------------------------------------------------------

;; 05-treesitter remaps markdown-mode to markdown-ts-mode; see the commentary for why not.
;; Emacs 31 does not autoload markdown-ts-mode: make M-x markdown-ts-mode work.
(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)
(setq major-mode-remap-alist (assq-delete-all 'markdown-mode major-mode-remap-alist))

(use-package markdown-mode
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\)\\'" . markdown-mode)
         ("\\(?:README\\|CHANGELOG\\|CONTRIBUTING\\|CODE_OF_CONDUCT\\|SECURITY\\|SUPPORT\\|HISTORY\\|CHANGES\\|PULL_REQUEST_TEMPLATE\\)\\.\\(?:md\\|markdown\\)\\'" . gfm-mode)
         ("/\\.github/.*\\.md\\'" . gfm-mode))
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(parent-directories)
        markdown-italic-underscore t
        markdown-unordered-list-item-prefix "- "
        markdown-list-indent-width 2
        markdown-asymmetric-header nil
        markdown-header-scaling t
        markdown-hide-urls nil
        markdown-gfm-use-electric-backquote nil
        markdown-use-pandoc-style-yaml-metadata t
        markdown-spaces-after-code-fence 0
        markdown-indent-on-enter 'indent-and-new-item)
  :config
  ;; Fenced blocks are fontified by the tree-sitter modes where the grammar exists.
  (dolist (entry '(("sh" . bash-ts-mode) ("bash" . bash-ts-mode) ("shell" . bash-ts-mode)
                   ("console" . bash-ts-mode) ("python" . python-ts-mode) ("py" . python-ts-mode)
                   ("js" . js-ts-mode) ("javascript" . js-ts-mode) ("jsx" . tsx-ts-mode)
                   ("ts" . typescript-ts-mode) ("typescript" . typescript-ts-mode)
                   ("tsx" . tsx-ts-mode) ("json" . json-ts-mode) ("jsonc" . json-ts-mode)
                   ("yaml" . yaml-ts-mode) ("yml" . yaml-ts-mode) ("toml" . toml-ts-mode)
                   ("go" . go-ts-mode) ("rust" . rust-ts-mode) ("rs" . rust-ts-mode)
                   ("lua" . lua-ts-mode) ("css" . css-ts-mode) ("html" . html-ts-mode)
                   ("dockerfile" . dockerfile-ts-mode) ("hcl" . hcl-mode)
                   ("terraform" . hcl-mode) ("nix" . nix-ts-mode) ("sql" . sql-mode)
                   ("elisp" . emacs-lisp-mode) ("el" . emacs-lisp-mode)
                   ("org" . org-mode) ("diff" . diff-mode)))
    (when (or (not (string-suffix-p "-ts-mode" (symbol-name (cdr entry))))
              (let ((lang (replace-regexp-in-string
                           "-ts-mode\\'" "" (symbol-name (cdr entry)))))
                (treesit-language-available-p (intern (if (equal lang "tsx") "tsx" lang)))))
      (setf (alist-get (car entry) markdown-code-lang-modes nil nil #'equal) (cdr entry)))))

(defun ide-markdown--lsp-wanted-p ()
  "Whether marksman should start with this buffer."
  (and buffer-file-name
       (pcase ide-markdown-lsp
         ('auto (and (executable-find "marksman")
                     (not (file-equal-p default-directory "~/"))
                     (or (project-current)
                         (locate-dominating-file default-directory ".marksman.toml"))))
         (_ (and ide-markdown-lsp (executable-find "marksman"))))
       t))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((markdown-mode :language-id "markdown")
                  (gfm-mode :language-id "markdown")
                  (markdown-ts-mode :language-id "markdown"))
                 . ("marksman" "server"))))

;; A notes folder without a VCS root: .marksman.toml marks the workspace, so marksman sees
;; every note (links and wiki-link completion, rename and references across files).
(defun ide-markdown--marksman-root (dir)
  "Project root for DIR when a .marksman.toml lies above it, for `project-find-functions'."
  (when-let* ((root (locate-dominating-file dir ".marksman.toml")))
    (cons 'transient root)))

(defun ide-markdown--setup ()
  "Buffer-local settings for Markdown buffers."
  (setq-local fill-column ide-markdown-fill-column
              tab-width 2
              indent-tabs-mode nil
              ;; prettier (Apheleia) owns formatting; marksman has no formatter anyway.
              ide-eglot-format-on-save nil
              ispell-personal-dictionary (expand-file-name "ispell-personal" ide-var-dir)
              require-final-newline t)
  (visual-line-mode 1)
  (add-hook 'project-find-functions #'ide-markdown--marksman-root 90 t)
  (when (ide-markdown--lsp-wanted-p) (eglot-ensure))
  (when (and ide-markdown-format-on-save (fboundp 'apheleia-mode)) (apheleia-mode 1))
  (add-hook 'before-save-hook #'ide-markdown--toc-on-save nil t)
  (when ide-markdown-spell-on-open (ignore-errors (flyspell-mode 1))))

(add-hook 'markdown-mode-hook #'ide-markdown--setup)
(add-hook 'gfm-mode-hook #'ide-markdown--setup)

;; prettier --prose-wrap preserve: never rewrap the author's lines.  .prettierrc is found
;; from the file path, so a project's own settings still apply.
(defun ide-markdown--stdin-name ()
  "File name prettier should assume for the buffer (its .prettierrc lookup and parser)."
  (or buffer-file-name "buffer.md"))

(ide-lang-define-formatter 'ide-markdown-prettier
  '("apheleia-npx" "prettier" "--stdin-filepath" (ide-markdown--stdin-name) "--prose-wrap" "preserve")
  '(markdown-mode gfm-mode))

(defun ide-markdown-toggle-format-on-save ()
  "Toggle prettier-on-save in this buffer."
  (interactive)
  (if (bound-and-true-p apheleia-mode)
      (progn (apheleia-mode -1) (message "Format on save off"))
    (apheleia-mode 1) (message "Format on save on")))

;;;; Linting ------------------------------------------------------------------------------------

(defconst ide-markdown--lint-config-names
  '(".markdownlint.json" ".markdownlint.jsonc" ".markdownlint.yaml" ".markdownlint.yml"
    ".markdownlint.cjs" ".markdownlint.mjs" ".markdownlint-cli2.jsonc"
    ".markdownlint-cli2.yaml" ".markdownlint-cli2.cjs" ".markdownlint-cli2.mjs")
  "Files markdownlint-cli2 reads its configuration from.")

(defun ide-markdown--lint-config (&optional dir)
  "The nearest markdownlint configuration file above DIR (default: here), or nil."
  (let ((dir (or dir default-directory)))
    (cl-loop for name in ide-markdown--lint-config-names
             for d = (locate-dominating-file dir name)
             when d collect (expand-file-name name d) into found
             finally return (car (sort found (lambda (a b) (> (length a) (length b))))))))

(defun ide-markdown--relaxed-config ()
  "Path of the relaxed default rule set (line length, inline HTML, first-line h1 off)."
  (let ((file (expand-file-name "markdown/markdownlint-relaxed.jsonc" ide-cache-dir)))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "{\n  \"default\": true,\n  \"MD013\": false,\n  \"MD033\": false,\n"
                "  \"MD041\": false,\n  \"MD025\": { \"front_matter_title\": \"\" },\n  \"MD024\": { \"siblings_only\": true }\n}\n")))
    file))

(defun ide-markdown--lint-args (&rest tail)
  "markdownlint-cli2 arguments for the current buffer followed by TAIL.
A project config wins; a .markdownlint.* file outside the project root is passed
explicitly (stdin has no directory of its own); with none the relaxed defaults apply."
  (let ((cfg (ide-markdown--lint-config)))
    (append (cond ((null cfg) (list "--config" (ide-markdown--relaxed-config)))
                  ((string-match-p "markdownlint-cli2" cfg) nil)
                  ((not (file-equal-p (file-name-directory cfg) (ide-lang-root)))
                   (list "--config" cfg)))
            tail)))

(ide-lang-define-linter 'markdownlint
  :modes '(markdown-mode gfm-mode)
  :program "markdownlint-cli2"
  :args (lambda (_file) (ide-markdown--lint-args "-"))
  ;; All findings are style advice: warnings, not errors.
  :parse (ide-lang-parse-regexp
          "markdownlint" "^stdin:\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)? \\(?:error\\|warning\\) \\(.*\\)$"
          :line 1 :col 2 :message 3))



(defun ide-markdown-lint-fix ()
  "Apply markdownlint's autofixes to the buffer (unsaved text included)."
  (interactive)
  (ide-lang-exe-or-error "markdownlint-cli2")
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (default-directory (ide-lang-root))
         (fixed (with-temp-buffer
                  (insert text)
                  (let ((status (apply #'call-process-region (point-min) (point-max)
                                       "markdownlint-cli2" t '(t nil) nil
                                       (ide-markdown--lint-args "--format"))))
                    (unless (zerop status) (user-error "markdownlint-cli2 --format exited %s" status)))
                  (buffer-string))))
    (if (equal fixed text)
        (message "markdownlint: nothing to fix automatically")
      (let ((pt (point)))
        (replace-region-contents (point-min) (point-max) (lambda () fixed))
        (goto-char (min pt (point-max)))
        (message "markdownlint: fixes applied")))))

(defun ide-markdown-lint-project ()
  "Lint every Markdown file of the project; findings go to the drawer."
  (interactive)
  (ide-lang-exe-or-error "markdownlint-cli2")
  (ide-lang-run "markdownlint" (append '("markdownlint-cli2")
                                       (ide-markdown--lint-args "**/*.{md,markdown}"
                                                                "#node_modules" "#.git"))))

(defun ide-markdown-lint-fix-project ()
  "Autofix every Markdown file of the project (asks first; files are rewritten)."
  (interactive)
  (ide-lang-exe-or-error "markdownlint-cli2")
  (when (yes-or-no-p "Rewrite every Markdown file in the project with markdownlint --fix? ")
    (save-some-buffers t)
    (ide-lang-run "markdownlint --fix"
                  (append '("markdownlint-cli2" "--fix")
                          (ide-markdown--lint-args "**/*.{md,markdown}" "#node_modules" "#.git")))))

;;;; Structure parsing (independent of font-lock) --------------------------------------------------

(defun ide-markdown--fence-info (info)
  "The language of a fence info string INFO (\"python title=x\", \"{.python}\")."
  (let ((w (car (split-string (string-trim info) "[ \t,]+" t))))
    (when w (downcase (replace-regexp-in-string "\\`{?\\.?\\|}\\'" "" w)))))

(defun ide-markdown--front-matter-end ()
  "Position after a leading YAML front matter block, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "---[ \t]*$")
      (forward-line 1)
      (when (re-search-forward "^\\(?:---\\|\\.\\.\\.\\)[ \t]*$" nil t) (line-end-position)))))

(defun ide-markdown--fences ()
  "Fenced code blocks of the buffer: a list of plists (:start :end :lang :body-start :body-end).
:start and :end bound the fence lines; the body lies between :body-start and :body-end."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let (blocks open)
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (cond
             ((and (null open)
                   (string-match "\\`[ \t]\\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)\\([^`\n]*\\)\\'" line))
              (setq open (list :start (line-beginning-position) :char (aref (match-string 1 line) 0)
                               :len (length (match-string 1 line))
                               :lang (ide-markdown--fence-info (match-string 2 line))
                               :body-start (min (point-max) (1+ (line-end-position))))))
             ((and open
                   (string-match (format "\\`[ \t]\\{0,3\\}%c\\{%d,\\}[ \t]*\\'"
                                         (plist-get open :char) (plist-get open :len))
                                 line))
              (push (list :start (plist-get open :start) :end (line-end-position)
                          :lang (plist-get open :lang) :body-start (plist-get open :body-start)
                          :body-end (line-beginning-position))
                    blocks)
              (setq open nil))))
          (forward-line 1))
        ;; An unclosed fence runs to the end of the buffer, like CommonMark says.
        (when open
          (push (list :start (plist-get open :start) :end (point-max) :lang (plist-get open :lang)
                      :body-start (plist-get open :body-start) :body-end (point-max))
                blocks))
        (nreverse blocks)))))

(defun ide-markdown--fence-at (&optional pos)
  "The fenced block containing POS (default point), as a plist, or nil."
  (let ((pos (or pos (point))))
    (cl-find-if (lambda (b) (and (>= pos (plist-get b :start)) (<= pos (plist-get b :end))))
                (ide-markdown--fences))))

(defun ide-markdown--in-code-p (pos fences)
  "Whether POS is inside one of FENCES or the front matter."
  (or (cl-some (lambda (b) (and (>= pos (plist-get b :start)) (<= pos (plist-get b :end)))) fences)
      (let ((fm (ide-markdown--front-matter-end))) (and fm (<= pos fm)))))

(defun ide-markdown--strip-inline (text)
  "TEXT of a heading without Markdown inline syntax."
  (let ((s text))
    (setq s (replace-regexp-in-string "!?\\[\\([^]]*\\)\\]([^)]*)" "\\1" s))
    (setq s (replace-regexp-in-string "\\[\\([^]]*\\)\\]\\[[^]]*\\]" "\\1" s))
    (setq s (replace-regexp-in-string "`\\([^`]*\\)`" "\\1" s))
    (setq s (replace-regexp-in-string "<[^>]+>" "" s))
    (string-trim (replace-regexp-in-string "[*~]+" "" s))))

(defun ide-markdown--slug (text)
  "GitHub's anchor for the heading TEXT (before the -1, -2 duplicate suffix)."
  (let* ((s (downcase (ide-markdown--strip-inline text)))
         (s (replace-regexp-in-string "[^[:alnum:] _-]" "" s)))
    (replace-regexp-in-string " " "-" s)))

(defun ide-markdown--headings ()
  "Headings outside code: a list of (LEVEL TEXT POS SLUG) in buffer order, SLUG deduplicated."
  (save-excursion
    (save-restriction
      (widen)
      (let ((fences (ide-markdown--fences)) (seen (make-hash-table :test 'equal)) heads)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((bol (line-beginning-position)))
            (unless (ide-markdown--in-code-p bol fences)
              (cond
               ((looking-at "\\(#\\{1,6\\}\\)[ \t]+\\(.*?\\)\\(?:[ \t]+#+\\)?[ \t]*$")
                (push (list (length (match-string 1)) (match-string-no-properties 2) bol) heads))
               ((and (looking-at "[^ \t\n>#*+`~|-][^\n]*$")
                     (not (looking-at "[0-9]+[.)] "))
                     (save-excursion
                       (forward-line 1)
                       (looking-at "\\(=+\\|-+\\)[ \t]*$"))
                     (or (bobp) (save-excursion (forward-line -1) (looking-at "[ \t]*$"))))
                (let ((text (string-trim (buffer-substring-no-properties bol (line-end-position))))
                      (lvl (save-excursion (forward-line 1) (if (eq (char-after) ?=) 1 2))))
                  (push (list lvl text bol) heads))))))
          (forward-line 1))
        (mapcar (lambda (h)
                  (let* ((slug (ide-markdown--slug (nth 1 h)))
                         (n (gethash slug seen 0)))
                    (puthash slug (1+ n) seen)
                    (append h (list (if (zerop n) slug (format "%s-%d" slug n))))))
                (nreverse heads))))))

;;;; Table of contents ---------------------------------------------------------------------------

(defconst ide-markdown--toc-start "<!-- toc -->")
(defconst ide-markdown--toc-end "<!-- tocstop -->")

(defun ide-markdown--toc-text ()
  "The TOC as Markdown list text for the buffer's headings."
  (let* ((heads (cl-remove-if (lambda (h) (> (car h) ide-markdown-toc-depth)) (ide-markdown--headings)))
         (h1s (cl-count 1 heads :key #'car))
         ;; With a single h1 (the document title) the TOC starts at h2.
         (floor (if (= h1s 1) 2 1))
         (heads (cl-remove-if (lambda (h) (< (car h) floor)) heads))
         (base (if heads (apply #'min (mapcar #'car heads)) 1)))
    (mapconcat (lambda (h)
                 (format "%s- [%s](#%s)" (make-string (* 2 (- (car h) base)) ?\s)
                         (ide-markdown--strip-inline (nth 1 h)) (nth 3 h)))
               heads "\n")))

(defun ide-markdown--toc-region ()
  "Bounds (START . END) of the text between the TOC markers, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (search-forward ide-markdown--toc-start nil t)
      (let ((start (point)))
        (when (search-forward ide-markdown--toc-end nil t)
          (cons start (match-beginning 0)))))))

(defun ide-markdown-toc ()
  "Insert a table of contents at point, or update the one between the markers.
The markers are <!-- toc --> and <!-- tocstop -->; the list is rebuilt from the headings."
  (interactive)
  (let ((toc (ide-markdown--toc-text)))
    (if-let* ((reg (ide-markdown--toc-region)))
        (save-excursion
          (goto-char (car reg))
          (delete-region (car reg) (cdr reg))
          (insert "\n\n" toc "\n\n"))
      (insert ide-markdown--toc-start "\n\n" toc "\n\n" ide-markdown--toc-end "\n"))))

(defun ide-markdown--toc-on-save ()
  "Keep an existing TOC current when saving."
  (when (and ide-markdown-toc-update-on-save (ide-markdown--toc-region))
    (let ((old (buffer-substring-no-properties (car (ide-markdown--toc-region))
                                               (cdr (ide-markdown--toc-region)))))
      (unless (equal (string-trim old) (string-trim (ide-markdown--toc-text)))
        (ide-markdown-toc)))))

;;;; Editing helpers -------------------------------------------------------------------------------

(defun ide-markdown--code-languages ()
  "Languages for code blocks: the installed grammars first, then common extras."
  (delete-dups
   (append (cl-loop for entry in (bound-and-true-p treesit-language-source-alist)
                    for lang = (symbol-name (car entry))
                    when (and (treesit-language-available-p (car entry))
                              (not (string-prefix-p "markdown" lang)))
                    collect lang)
           (list "bash" "sh" "console" "python" "sql" "diff" "text" "elisp" "ini" "xml" "c" "cpp"
                 "java" "ruby" "php" "powershell" "mermaid" "csv"))))

(defun ide-markdown-insert-code-block (lang)
  "Insert a fenced code block of LANG, around the region when one is active."
  (interactive (list (completing-read "Language: " (ide-markdown--code-languages))))
  (let* ((use-region (use-region-p))
         (body (when use-region (buffer-substring (region-beginning) (region-end)))))
    (when use-region (delete-region (region-beginning) (region-end)))
    (unless (bolp) (insert "\n"))
    (insert "```" lang "\n")
    (let ((here (point)))
      (insert (or body "") (if (or (null body) (string-suffix-p "\n" body)) "" "\n") "```\n")
      (goto-char here))))

(defun ide-markdown-align-tables ()
  "Align every pipe table of the buffer (those in code blocks are left alone)."
  (interactive)
  (let ((fences (ide-markdown--fences)) (n 0))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*|" nil t)
        (let ((pos (line-beginning-position)))
          (if (or (ide-markdown--in-code-p pos fences) (not (markdown-table-at-point-p)))
              (forward-line 1)
            (markdown-table-align)
            (cl-incf n)
            (while (and (not (eobp)) (looking-at "[ \t]*|")) (forward-line 1))))))
    (message "Aligned %d table%s" n (if (= n 1) "" "s"))))

(defun ide-markdown-toggle-task ()
  "Cycle the list item at point: plain, [ ], [x], plain (Org-style task toggling)."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "\\([ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\)\\[\\([ xX]\\)\\][ \t]")
      (if (equal (match-string 2) " ")
          (replace-match "\\1[x] " t)
        (delete-region (+ (match-beginning 1) (length (match-string 1)))
                       (+ (match-end 0)))))
     ((looking-at "\\([ \t]*\\(?:[-*+]\\|[0-9]+[.)]\\)[ \t]+\\)")
      (goto-char (match-end 1))
      (insert "[ ] "))
     (t (user-error "Not on a list item")))))

(defun ide-markdown-word-count (&optional beg end)
  "Report words and reading time of the region or buffer, ignoring code and front matter."
  (interactive (when (use-region-p) (list (region-beginning) (region-end))))
  (let* ((text (buffer-substring-no-properties (or beg (point-min)) (or end (point-max))))
         (words (with-temp-buffer
                  (insert text)
                  (let ((fm (progn (goto-char (point-min)) (ide-markdown--front-matter-end))))
                    (when fm (delete-region (point-min) fm)))
                  (dolist (b (reverse (ide-markdown--fences)))
                    (delete-region (plist-get b :start) (plist-get b :end)))
                  (goto-char (point-min))
                  (while (re-search-forward "<!--\\(?:.\\|\n\\)*?-->\\|!?\\[\\([^]]*\\)\\]([^)]*)" nil t)
                    (replace-match (or (match-string 1) "") t t))
                  (goto-char (point-min))
                  (let ((n 0))
                    (while (re-search-forward "\\w+" nil t) (cl-incf n))
                    n)))
         (minutes (max 1 (ceiling (/ (float words) ide-markdown-words-per-minute)))))
    (message "%d words, about %d min to read" words minutes)
    words))

(defun ide-markdown-spell ()
  "Toggle Flyspell here, reporting when no working spell checker is installed."
  (interactive)
  (condition-case err
      (flyspell-mode 'toggle)
    (error (user-error "Spell checking is not available here: %s" (error-message-string err)))))

;;;; Clipboard images -------------------------------------------------------------------------------

(defun ide-markdown--clipboard-types ()
  "MIME types the Wayland clipboard offers (nil without wl-paste)."
  (when (executable-find "wl-paste")
    (with-temp-buffer
      (when (zerop (call-process "wl-paste" nil '(t nil) nil "--list-types"))
        (split-string (buffer-string) "\n" t)))))

(defun ide-markdown-paste-image (alt)
  "Save the clipboard image under `ide-markdown-image-dir' and insert a link to it with ALT text."
  (interactive "sAlt text: ")
  (ide-lang-require-file)
  (let* ((type (or (cl-find-if (lambda (ty) (string-prefix-p "image/" ty)) (ide-markdown--clipboard-types))
                   (user-error "No image on the clipboard (needs wl-paste)")))
         (ext (pcase type ("image/jpeg" "jpg") ("image/svg+xml" "svg") (_ (substring type 6))))
         (dir (expand-file-name ide-markdown-image-dir (file-name-directory buffer-file-name)))
         (file (expand-file-name (format-time-string "image-%Y%m%d-%H%M%S.") dir)))
    (make-directory dir t)
    (setq file (concat file ext))
    (unless (zerop (call-process "wl-paste" nil (list :file file) nil "--type" type))
      (user-error "wl-paste failed"))
    (insert (format "![%s](%s)" alt (url-hexify-string
                                     (file-relative-name file (file-name-directory buffer-file-name))
                                     (cons ?/ url-unreserved-chars))))
    (message "Saved %s" (file-relative-name file))))

;;;; Link checking ------------------------------------------------------------------------------------

(defun ide-markdown--project-files ()
  "Markdown files of the project (or the directory tree), absolute names."
  (let* ((root (ide-lang-root))
         (all (or (when-let* ((p (project-current nil root)))
                    (ignore-errors (project-files p)))
                  (directory-files-recursively root "\\.\\(?:md\\|markdown\\)\\'" nil
                                               (lambda (d) (not (string-match-p "/\\(?:\\.git\\|node_modules\\)\\'" d)))))))
    (cl-remove-if-not (lambda (f) (string-match-p "\\.\\(?:md\\|markdown\\)\\'" f)) all)))

(defvar ide-markdown--slug-cache nil "Hash of file to its heading slugs during one link check.")

(defun ide-markdown--file-slugs (file)
  "Heading anchors of FILE."
  (unless ide-markdown--slug-cache (setq ide-markdown--slug-cache (make-hash-table :test 'equal)))
  (or (gethash file ide-markdown--slug-cache)
      (puthash file
               (with-temp-buffer
                 (insert-file-contents file)
                 (let ((slugs (mapcar (lambda (h) (nth 3 h)) (ide-markdown--headings))))
                   ;; Raw HTML anchors and {#custom-id} attributes also count.
                   (goto-char (point-min))
                   (while (re-search-forward "\\(?:id\\|name\\)=\"\\([^\"]+\\)\"\\|{#\\([^} ]+\\)}" nil t)
                     (push (or (match-string 1) (match-string 2)) slugs))
                   slugs))
               ide-markdown--slug-cache)))

(defun ide-markdown--link-targets ()
  "Links of the current buffer outside code: a list of (LINE COL TARGET KIND)."
  (let ((fences (ide-markdown--fences)) found)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "!?\\[[^]\n]*\\](\\(<[^>\n]+>\\|[^) \t\n]+\\)\\(?:[ \t]+\"[^\"\n]*\"\\)?)\\|^ \\{0,3\\}\\[[^]^\n][^]\n]*\\]:[ \t]*\\(<[^>\n]+>\\|[^ \t\n]+\\)\\|\\[\\[\\([^]|\n]+\\)\\(?:|[^]\n]*\\)?\\]\\]"
              nil t)
        (let ((pos (match-beginning 0))
              (target (or (match-string-no-properties 1) (match-string-no-properties 2)
                          (match-string-no-properties 3)))
              (kind (if (match-string 3) 'wiki 'link))
              (bol (line-beginning-position)))
          (unless (or (ide-markdown--in-code-p pos fences)
                      ;; Inside an inline code span?
                      (cl-oddp (cl-count ?` (buffer-substring-no-properties bol pos))))
            (push (list (line-number-at-pos pos) (1+ (- pos bol))
                        (string-trim (string-remove-suffix ">" (string-remove-prefix "<" target)))
                        kind)
                  found)))))
    (nreverse found)))

(defun ide-markdown--check-target (file target kind root all-names)
  "A problem message for TARGET (of KIND) found in FILE, or nil when it resolves."
  (let* ((dir (file-name-directory file))
         (path (car (split-string target "[?#]")))
         (frag (when (string-match "#\\(.*\\)\\'" target) (match-string 1 target))))
    (cond
     ((and (eq kind 'link) (string-match-p "\\`\\(?:[a-zA-Z][a-zA-Z0-9+.-]*:\\|//\\)" target)) nil)
     ((string-empty-p target) nil)
     ((eq kind 'wiki)
      (let* ((name (downcase (car (split-string target "#"))))
             (hit (cl-find-if (lambda (f) (member name (list f (concat f ".md")))) all-names)))
        (cond ((string-empty-p name) nil)
              ((null hit) (format "broken wiki link [[%s]]" target)))))
     (t
      (let* ((decoded (url-unhex-string path))
             (abs (cond ((string-empty-p decoded) file)
                        ((string-prefix-p "/" decoded) (expand-file-name (substring decoded 1) root))
                        (t (expand-file-name decoded dir)))))
        (cond
         ((not (file-exists-p abs)) (format "broken link: %s does not exist" path))
         ((and frag (not (string-empty-p frag)) (not (string-match-p "\\`L[0-9]" frag))
               (string-match-p "\\.\\(?:md\\|markdown\\)\\'" abs)
               (not (member (downcase (url-unhex-string frag)) (mapcar #'downcase (ide-markdown--file-slugs abs)))))
          (format "broken anchor: no heading #%s in %s" frag (file-relative-name abs root)))))))))

(defun ide-markdown-check-links ()
  "Report broken relative links, wiki links and anchors in the project's Markdown files.
The report opens in the drawer in `compilation-mode', so \\[next-error] visits each one."
  (interactive)
  (save-some-buffers t (lambda () (derived-mode-p 'markdown-mode)))
  (let* ((root (ide-lang-root))
         (files (ide-markdown--project-files))
         (all-names (mapcar (lambda (f) (downcase (file-name-base f))) files))
         (ide-markdown--slug-cache nil)
         (buf (get-buffer-create "*run: markdown links*"))
         (count 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq default-directory (file-name-as-directory root))
        (insert (format "-*- mode: compilation; default-directory: %S -*-\nChecking links in %d Markdown files\n\n"
                        default-directory (length files)))
        (dolist (f files)
          (let ((rel (file-relative-name f root)))
            (dolist (l (with-temp-buffer
                         (insert-file-contents f)
                         (ide-markdown--link-targets)))
              (when-let* ((msg (ide-markdown--check-target f (nth 2 l) (nth 3 l) root all-names)))
                (cl-incf count)
                (insert (format "%s:%d:%d: warning: %s\n" rel (nth 0 l) (nth 1 l) msg))))))
        (insert (format "\n%d problem%s.\n" count (if (= count 1) "" "s"))))
      (compilation-mode)
      (goto-char (point-min)))
    (display-buffer buf)
    (message "%d broken link%s" count (if (= count 1) "" "s"))
    count))

;;;; Pandoc ---------------------------------------------------------------------------------------------

(defun ide-markdown--pandoc (text args)
  "Run pandoc with ARGS on TEXT and return its output; signal the error output on failure."
  (let ((pandoc (ide-lang-exe-or-error "pandoc")) (err (make-temp-file "pandoc-err")))
    (unwind-protect
        (with-temp-buffer
          (insert text)
          (let ((status (apply #'call-process-region (point-min) (point-max) pandoc t (list t err) nil args)))
            (unless (zerop status)
              (user-error "pandoc failed: %s" (string-trim (with-temp-buffer (insert-file-contents err) (buffer-string)))))
            (buffer-string)))
      (delete-file err))))

(defun ide-markdown--source-text ()
  "The active region, or the whole buffer; and whether it was the region."
  (if (use-region-p)
      (cons (buffer-substring-no-properties (region-beginning) (region-end)) t)
    (cons (buffer-substring-no-properties (point-min) (point-max)) nil)))

;;;; Preview theme -----------------------------------------------------------------------------------------

(defun ide-markdown--color (face attr &optional fallback)
  "ATTR of FACE as a #rrggbb string, or FALLBACK."
  (let ((c (ignore-errors (face-attribute face attr nil 'default))))
    (if (and (stringp c) (color-defined-p c) (color-name-to-rgb c))
        (apply #'color-rgb-to-hex (append (color-name-to-rgb c) '(2)))
      fallback)))

(defun ide-markdown--mix (a b ratio)
  "Colour A mixed with B, RATIO (0..1) of the way towards B."
  (let ((ca (color-name-to-rgb a)) (cb (color-name-to-rgb b)))
    (apply #'color-rgb-to-hex (append (cl-mapcar (lambda (x y) (+ (* x (- 1 ratio)) (* y ratio))) ca cb) '(2)))))

(defun ide-markdown--palette ()
  "Colours of the current Emacs theme as a plist for the preview."
  (let* ((bg (ide-markdown--color 'default :background "#ffffff"))
         (fg (ide-markdown--color 'default :foreground "#000000"))
         (dark (< (cl-reduce #'+ (color-name-to-rgb bg)) 1.5)))
    (list :dark dark :bg bg :fg fg
          :link (or (ide-markdown--color 'link :foreground) (ide-markdown--color 'font-lock-function-name-face :foreground) fg)
          :head (or (ide-markdown--color 'font-lock-keyword-face :foreground) fg)
          :muted (ide-markdown--mix bg fg 0.55)
          :codebg (ide-markdown--mix bg fg 0.07)
          :border (ide-markdown--mix bg fg 0.2)
          :quote (or (ide-markdown--color 'font-lock-comment-face :foreground) (ide-markdown--mix bg fg 0.5)))))

(defconst ide-markdown--token-faces
  '(("Alert" font-lock-warning-face) ("Annotation" font-lock-preprocessor-face)
    ("Attribute" font-lock-variable-name-face) ("BaseN" font-lock-constant-face)
    ("BuiltIn" font-lock-builtin-face) ("Comment" font-lock-comment-face)
    ("CommentVar" font-lock-comment-face) ("Constant" font-lock-constant-face)
    ("ControlFlow" font-lock-keyword-face) ("DataType" font-lock-type-face)
    ("DecVal" font-lock-constant-face) ("Documentation" font-lock-doc-face)
    ("Error" error) ("Extension" font-lock-builtin-face) ("Float" font-lock-constant-face)
    ("Function" font-lock-function-name-face) ("Import" font-lock-preprocessor-face)
    ("Information" font-lock-comment-face) ("Keyword" font-lock-keyword-face)
    ("Operator" default) ("Other" font-lock-preprocessor-face)
    ("Preprocessor" font-lock-preprocessor-face) ("RegionMarker" font-lock-comment-face)
    ("SpecialChar" font-lock-constant-face) ("SpecialString" font-lock-string-face)
    ("String" font-lock-string-face) ("Variable" font-lock-variable-name-face)
    ("VerbatimString" font-lock-string-face) ("Warning" font-lock-warning-face))
  "Pandoc highlighting token names and the Emacs face each takes its style from.")

(defun ide-markdown--theme-json (pal)
  "Pandoc highlighting theme (JSON text) built from font-lock faces and palette PAL."
  (let ((styles (mapcar
                 (lambda (entry)
                   (let* ((face (cadr entry))
                          (fg (or (ide-markdown--color face :foreground) (plist-get pal :fg)))
                          (w (ignore-errors (face-attribute face :weight nil 'default)))
                          (sl (ignore-errors (face-attribute face :slant nil 'default))))
                     (cons (intern (car entry))
                           `((text-color . ,fg)
                             (bold . ,(if (memq w '(bold semi-bold extra-bold ultra-bold)) t :false))
                             (italic . ,(if (memq sl '(italic oblique)) t :false))
                             (underline . :false)))))
                 ide-markdown--token-faces)))
    (json-serialize
     `((text-color . ,(plist-get pal :fg)) (background-color . ,(plist-get pal :codebg))
       (line-number-color . ,(plist-get pal :muted)) (line-number-background-color . ,(plist-get pal :codebg))
       (text-styles . ,styles))
     :false-object :false)))

(defun ide-markdown--css (pal)
  "Stylesheet text for palette PAL."
  (format "html{color-scheme:%s}
body{background:%s;color:%s;font-family:system-ui,-apple-system,'Segoe UI',sans-serif;line-height:1.65;max-width:52rem;margin:2rem auto;padding:0 1.5rem;font-size:16px}
h1,h2,h3,h4,h5,h6{color:%s;line-height:1.25;margin:1.6em 0 .6em}
h1,h2{border-bottom:1px solid %s;padding-bottom:.3em}
a{color:%s}
code,pre{font-family:ui-monospace,'JetBrains Mono','Fira Code',monospace;font-size:.9em}
code{background:%s;padding:.15em .35em;border-radius:4px}
pre{background:%s;padding:1em;border-radius:6px;overflow:auto;border:1px solid %s}
pre code{background:none;padding:0}
blockquote{border-left:4px solid %s;margin:1em 0;padding:.1em 1em;color:%s}
table{border-collapse:collapse;margin:1em 0}
th,td{border:1px solid %s;padding:.4em .8em}
th{background:%s}
hr{border:0;border-top:1px solid %s}
img{max-width:100%%}
.task-list-item{list-style:none}
.task-list-item input{margin:0 .5em 0 -1.4em}
.footnotes{font-size:.9em;color:%s}
header#title-block-header h1.title{border:0}
"
          (if (plist-get pal :dark) "dark" "light")
          (plist-get pal :bg) (plist-get pal :fg) (plist-get pal :head) (plist-get pal :border)
          (plist-get pal :link) (plist-get pal :codebg) (plist-get pal :codebg) (plist-get pal :border)
          (plist-get pal :quote) (plist-get pal :muted) (plist-get pal :border) (plist-get pal :codebg)
          (plist-get pal :border) (plist-get pal :muted)))

(defconst ide-markdown--sync-script
  "<script>
window.addEventListener('scroll',function(){window.name='mdscroll:'+window.scrollY;});
window.addEventListener('load',function(){
  var m=/^mdscroll:([0-9.]+)$/.exec(window.name);if(m)window.scrollTo(0,+m[1]);
});
</script>
"
  "Header snippet: the scroll offset survives reloads (a refresh keeps your place).")

(defconst ide-markdown--scroll-js
  "(function(n){
  var els=document.querySelectorAll('[data-pos]'),best=null,bl=-1;
  for(var i=0;i<els.length;i++){
    var m=/^(?:.*@)?(\\d+):/.exec(els[i].getAttribute('data-pos')||''),l=m?+m[1]:0;
    if(l<=n&&l>=bl){best=els[i];bl=l;}
  }
  if(!best){window.scrollTo(0,0);return;}
  window.scrollTo(0,Math.max(0,window.scrollY+best.getBoundingClientRect().top-window.innerHeight*0.3));
})(%d);"
  "Script (a format string taking a line number) scrolling to the last element whose
pandoc data-pos starts at or before that line.  It is sent whole each time: scripts run
by Emacs do not see the page's own globals.")

(defun ide-markdown--preview-dir ()
  (let ((d (expand-file-name "markdown-preview/" ide-cache-dir))) (make-directory d t) d))

(defun ide-markdown--write-theme ()
  "Write the CSS, highlighting theme and header snippet; return their paths (CSS THEME HEADER)."
  (let* ((dir (ide-markdown--preview-dir)) (pal (ide-markdown--palette))
         (css (expand-file-name "theme.css" dir))
         (theme (expand-file-name "highlight.theme" dir))
         (header (expand-file-name "header.html" dir)))
    (with-temp-file css (insert (ide-markdown--css pal)))
    (with-temp-file theme (insert (ide-markdown--theme-json pal)))
    (with-temp-file header (insert ide-markdown--sync-script))
    (list css theme header)))

(defun ide-markdown--html-args (&optional output title)
  "pandoc arguments for the themed standalone HTML written to OUTPUT, titled TITLE."
  (pcase-let ((`(,css ,theme ,header) (ide-markdown--write-theme)))
    (append (list "-f" (concat ide-markdown-pandoc-reader "+sourcepos") "-t" "html5" "--standalone"
                  "--embed-resources" "--mathml" "--resource-path=." "--css" css
                  "--syntax-highlighting" theme "--include-in-header" header
                  "-M" (concat "pagetitle=" (or title "Preview")))
            (when output (list "-o" output)))))

;;;; Live preview ----------------------------------------------------------------------------------------------

(defvar ide-markdown--preview-buffer nil "The xwidget buffer showing the preview.")
(defvar-local ide-markdown--preview-file nil "Rendered HTML file of this buffer.")
(defvar-local ide-markdown--render-process nil)
(defvar-local ide-markdown--idle-timer nil)
(defvar-local ide-markdown--follow-timer nil)
(defvar-local ide-markdown--last-line nil)

(defun ide-markdown--preview-live-p ()
  (and (buffer-live-p ide-markdown--preview-buffer)
       (get-buffer-window ide-markdown--preview-buffer t)))

(defun ide-markdown--file-url (file) (url-encode-url (concat "file://" file)))

(defun ide-markdown--render (&optional then)
  "Render this buffer to its preview file in the background; call THEN with the file when done."
  (let* ((src (current-buffer))
         (out (or ide-markdown--preview-file
                  (setq ide-markdown--preview-file
                        (expand-file-name (concat (md5 (or buffer-file-name (buffer-name))) ".html")
                                          (ide-markdown--preview-dir)))))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (default-directory (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
         (errbuf (generate-new-buffer " *pandoc-preview*"))
         (args (ide-markdown--html-args out (file-name-nondirectory (or buffer-file-name (buffer-name))))))
    (when (process-live-p ide-markdown--render-process) (kill-process ide-markdown--render-process))
    (let ((proc (make-process
                 :name "markdown-preview" :buffer errbuf :noquery t :connection-type 'pipe
                 :command (cons (ide-lang-exe-or-error "pandoc") args)
                 :sentinel (lambda (p _e)
                             (when (memq (process-status p) '(exit signal))
                               (unwind-protect
                                   (if (and (eq (process-status p) 'exit) (zerop (process-exit-status p)))
                                       (when (buffer-live-p src)
                                         (with-current-buffer src
                                           (when then (funcall then out))))
                                     (message "markdown preview: pandoc failed: %s"
                                              (string-trim (with-current-buffer errbuf (buffer-string)))))
                                 (kill-buffer errbuf)))))))
      (setq ide-markdown--render-process proc)
      (process-send-string proc text)
      (process-send-eof proc))))

(defun ide-markdown--show-preview (file)
  "Show FILE in the preview browser: reload when it is already there, else open it beside the editor."
  (let ((url (ide-markdown--file-url file)))
    (if (and (buffer-live-p ide-markdown--preview-buffer) (display-graphic-p))
        (with-current-buffer ide-markdown--preview-buffer
          (let ((session (xwidget-webkit-current-session)))
            (if (equal (xwidget-webkit-uri session) url)
                (xwidget-webkit-reload)
              (xwidget-webkit-goto-uri session url)))
          (unless (get-buffer-window ide-markdown--preview-buffer t)
            (let ((display-buffer-overriding-action
                   '(display-buffer-in-side-window (side . right) (slot . 2) (window-width . 0.45))))
              (display-buffer ide-markdown--preview-buffer))))
      (save-selected-window
        ;; A dedicated session: never hijack the docs browser that may be open.
        (let ((xwidget-webkit-last-session-buffer nil))
          (ide-lang-browse url)
          (setq ide-markdown--preview-buffer
                (and (bound-and-true-p xwidget-webkit-last-session-buffer)
                     xwidget-webkit-last-session-buffer)))))))

(defun ide-markdown--sync-later ()
  "Sync the preview once the reloaded page has had time to load."
  (let ((buf (current-buffer)))
    (run-with-timer 1 nil (lambda () (when (buffer-live-p buf)
                                       (with-current-buffer buf (ide-markdown-preview-sync)))))))

(defun ide-markdown--refresh ()
  "Re-render and reload the preview of this buffer when one is open."
  (when (and (bound-and-true-p ide-markdown-preview-mode) (ide-markdown--preview-live-p))
    (ide-markdown--render
     (lambda (file)
       (ide-markdown--show-preview file)
       (when ide-markdown-preview-follow (ide-markdown--sync-later))))))

(defun ide-markdown--on-save ()
  (when (eq ide-markdown-preview-auto 'save) (ide-markdown--refresh)))

(defun ide-markdown--on-change (&rest _)
  (when (eq ide-markdown-preview-auto 'idle)
    (when (timerp ide-markdown--idle-timer) (cancel-timer ide-markdown--idle-timer))
    (let ((buf (current-buffer)))
      (setq ide-markdown--idle-timer
            (run-with-idle-timer 1 nil (lambda () (when (buffer-live-p buf)
                                                    (with-current-buffer buf (ide-markdown--refresh)))))))))

(defun ide-markdown-preview-sync ()
  "Scroll the preview to the source line at point."
  (interactive)
  (when (and (buffer-live-p ide-markdown--preview-buffer) (display-graphic-p))
    (let ((line (line-number-at-pos nil t)))
      (with-current-buffer ide-markdown--preview-buffer
        (when-let* ((session (ignore-errors (xwidget-webkit-current-session))))
          (xwidget-webkit-execute-script session (format ide-markdown--scroll-js line)))))))

(defun ide-markdown--follow ()
  "post-command-hook: scroll the preview after the cursor moved to another line."
  (when (and ide-markdown-preview-follow (bound-and-true-p ide-markdown-preview-mode)
             (not (eq (line-number-at-pos) ide-markdown--last-line)))
    (setq ide-markdown--last-line (line-number-at-pos))
    (when (timerp ide-markdown--follow-timer) (cancel-timer ide-markdown--follow-timer))
    (let ((buf (current-buffer)))
      (setq ide-markdown--follow-timer
            (run-with-idle-timer 0.15 nil (lambda () (when (buffer-live-p buf)
                                                       (with-current-buffer buf (ide-markdown-preview-sync)))))))))

;;;###autoload
(define-minor-mode ide-markdown-preview-mode
  "Show a pandoc-rendered, theme-coloured preview beside the editor and keep it current.
`ide-markdown-preview-auto' decides between refreshing on save or while idle."
  :lighter " Prev"
  (if ide-markdown-preview-mode
      (progn
        (add-hook 'after-save-hook #'ide-markdown--on-save nil t)
        (add-hook 'after-change-functions #'ide-markdown--on-change nil t)
        (add-hook 'post-command-hook #'ide-markdown--follow nil t)
        (ide-markdown--render
         (lambda (file)
           (ide-markdown--show-preview file)
           (when ide-markdown-preview-follow (ide-markdown--sync-later)))))
    (remove-hook 'after-save-hook #'ide-markdown--on-save t)
    (remove-hook 'after-change-functions #'ide-markdown--on-change t)
    (remove-hook 'post-command-hook #'ide-markdown--follow t)
    (when (process-live-p ide-markdown--render-process) (kill-process ide-markdown--render-process))))

(defun ide-markdown-preview ()
  "Open the live preview beside the editor (or refresh it)."
  (interactive)
  (if ide-markdown-preview-mode (ide-markdown--refresh) (ide-markdown-preview-mode 1))
  (when (and ide-markdown-preview-mode (not (ide-markdown--preview-live-p)))
    (ide-markdown--render #'ide-markdown--show-preview)))

(defun ide-markdown-preview-cycle-auto ()
  "Cycle automatic refresh: on save, while idle, off."
  (interactive)
  (setq ide-markdown-preview-auto (pcase ide-markdown-preview-auto ('save 'idle) ('idle nil) (_ 'save)))
  (message "Preview refresh: %s" (or ide-markdown-preview-auto "manual")))

(defun ide-markdown-preview-toggle-follow ()
  "Toggle scrolling the preview along with point."
  (interactive)
  (setq ide-markdown-preview-follow (not ide-markdown-preview-follow))
  (message "Preview follows point: %s" (if ide-markdown-preview-follow "on" "off")))

(defun ide-markdown-open-rendered ()
  "Render this buffer and open the HTML in the system browser."
  (interactive)
  (ide-markdown--render #'browse-url-of-file))

;;;; Export -------------------------------------------------------------------------------------------------------

(defconst ide-markdown-export-formats
  '("html" "docx" "pdf" "odt" "epub" "latex" "rst" "asciidoc" "typst" "org" "pptx")
  "Output formats offered by `ide-markdown-export' (any pandoc writer works).")

(defun ide-markdown--pdf-engine ()
  "A PDF engine pandoc can use, or nil."
  (cl-find-if #'executable-find '("typst" "xelatex" "lualatex" "pdflatex" "tectonic" "weasyprint"
                                  "wkhtmltopdf" "prince")))

(defun ide-markdown-export (format)
  "Export the file with pandoc to FORMAT next to it; progress and errors show in the drawer."
  (interactive (list (completing-read "Export to: " ide-markdown-export-formats nil nil)))
  (ide-lang-require-file t)
  (let* ((ext (pcase format ("latex" "tex") ("asciidoc" "adoc") (f f)))
         (out (concat (file-name-sans-extension buffer-file-name) "." ext))
         (engine (when (equal format "pdf")
                   (or (ide-markdown--pdf-engine)
                       (user-error "No PDF engine (typst, xelatex, weasyprint ...) is installed; export to html or docx instead"))))
         (args (if (equal format "html")
                   (ide-markdown--html-args out (file-name-base buffer-file-name))
                 (append (list "-f" ide-markdown-pandoc-reader "--standalone" "--resource-path=."
                               "-o" out)
                         (when engine (list (concat "--pdf-engine=" engine)))
                         (unless (member format '("pdf" "docx" "odt" "epub" "pptx")) (list "-t" format)))))
         (default-directory (file-name-directory buffer-file-name)))
    (ide-lang-run-then (format "pandoc %s" format)
                       (append (list "pandoc") args (list (file-name-nondirectory buffer-file-name)))
                       (lambda ()
                         (message "Exported %s" out)
                         (when (equal format "html") (ide-lang-browse (ide-markdown--file-url out))))
                       default-directory)))

;;;; Org interoperability ---------------------------------------------------------------------------------------------

(defun ide-markdown--md-to-org (text)
  "TEXT (GFM) as Org.  Lossy: raw HTML, attributes and heading ids have no Org form."
  (ide-markdown--pandoc text (list "-f" (concat ide-markdown-pandoc-reader "-gfm_auto_identifiers") "-t" "org" "--wrap=preserve")))

(defun ide-markdown--org-to-md (text)
  "TEXT (Org) as GFM.  Lossy: TODO keywords, tags, properties, timestamps, babel headers and
#+ keywords other than the title have no Markdown form."
  (ide-markdown--pandoc text (list "-f" "org" "-t" "gfm-raw_html" "--wrap=preserve")))

(defun ide-markdown--html-to (text target)
  "HTML TEXT as TARGET (\"org\" or \"gfm\")."
  (ide-markdown--pandoc text (list "-f" "html-auto_identifiers" "-t" (if (equal target "gfm") "gfm-raw_html" target) "--wrap=preserve")))

(defun ide-markdown--show-converted (name text mode)
  "Show TEXT in a new buffer called NAME in MODE."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf (insert text) (goto-char (point-min)) (funcall mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun ide-markdown-to-org ()
  "Convert the region (or buffer) from Markdown to Org in a new buffer."
  (interactive)
  (let ((src (ide-markdown--source-text)))
    (ide-markdown--show-converted
     (format "%s.org" (if buffer-file-name (file-name-base buffer-file-name) "converted"))
     (ide-markdown--md-to-org (car src)) #'org-mode)))

;;;###autoload
(defun ide-org-to-markdown ()
  "Convert the region (or buffer) from Org to GFM Markdown in a new buffer."
  (interactive)
  (let ((src (ide-markdown--source-text)))
    (ide-markdown--show-converted
     (format "%s.md" (if buffer-file-name (file-name-base buffer-file-name) "converted"))
     (ide-markdown--org-to-md (car src)) #'markdown-mode)))

(defun ide-markdown-region-to-org (beg end)
  "Replace the region with its conversion from Markdown to Org (in any buffer)."
  (interactive "r")
  (let ((new (ide-markdown--md-to-org (buffer-substring-no-properties beg end))))
    (delete-region beg end) (insert (string-trim-right new) "\n")))

(defun ide-org-region-to-markdown (beg end)
  "Replace the region with its conversion from Org to Markdown (in any buffer)."
  (interactive "r")
  (let ((new (ide-markdown--org-to-md (buffer-substring-no-properties beg end))))
    (delete-region beg end) (insert (string-trim-right new) "\n")))

(defun ide-markdown--convert-file (file ext converter mode)
  "Convert FILE with CONVERTER (text to text) into a sibling file with extension EXT and open it."
  (let ((out (concat (file-name-sans-extension file) "." ext)))
    (when (and (file-exists-p out) (not (y-or-n-p (format "%s exists; overwrite? " (abbreviate-file-name out)))))
      (user-error "Cancelled"))
    (let ((text (funcall converter (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (with-temp-file out (insert text)))
    (find-file out)
    (when (and mode (not (derived-mode-p mode))) (funcall mode))
    (message "Wrote %s" out)))

(defun ide-markdown-file-to-org (file)
  "Convert the Markdown FILE (default: this buffer's) to a sibling .org file and open it."
  (interactive (list (read-file-name "Markdown file: " nil buffer-file-name t
                                     (and buffer-file-name (file-name-nondirectory buffer-file-name)))))
  (ide-markdown--convert-file (expand-file-name file) "org" #'ide-markdown--md-to-org nil))

(defun ide-org-file-to-markdown (file)
  "Convert the Org FILE (default: this buffer's) to a sibling .md file (GFM) and open it."
  (interactive (list (read-file-name "Org file: " nil buffer-file-name t
                                     (and buffer-file-name (file-name-nondirectory buffer-file-name)))))
  (ide-markdown--convert-file (expand-file-name file) "md" #'ide-markdown--org-to-md nil))

(defun ide-markdown--clipboard-text (&optional type)
  "Clipboard text of MIME TYPE (default text/plain) through wl-paste, else the kill ring."
  (or (when (and (executable-find "wl-paste")
                 (member (or type "text/plain") (ide-markdown--clipboard-types)))
        (with-temp-buffer
          (when (zerop (call-process "wl-paste" nil '(t nil) nil "--no-newline" "--type" (or type "text/plain")))
            (buffer-string))))
      (and (null type) (current-kill 0 t))
      (user-error "Nothing of type %s on the clipboard" (or type "text/plain"))))

(defun ide-org-paste-markdown ()
  "Paste the clipboard (Markdown source) at point as Org text."
  (interactive "*")
  (insert (string-trim-right (ide-markdown--md-to-org (ide-markdown--clipboard-text))) "\n"))

(defun ide-markdown-paste-org ()
  "Paste the clipboard (Org source) at point as GFM Markdown."
  (interactive "*")
  (insert (string-trim-right (ide-markdown--org-to-md (ide-markdown--clipboard-text))) "\n"))

(defun ide-markdown-paste-rich ()
  "Paste the clipboard's HTML (copied from a web page) as Org in Org buffers, else as Markdown."
  (interactive "*")
  (let ((html (ide-markdown--clipboard-text "text/html")))
    (insert (string-trim-right
             (ide-markdown--html-to html (if (derived-mode-p 'org-mode) "org" "gfm")))
            "\n")))

(defun ide-markdown-copy-as-org (beg end)
  "Copy the region (Markdown) to the clipboard as Org."
  (interactive "r")
  (kill-new (ide-markdown--md-to-org (buffer-substring-no-properties beg end)))
  (message "Copied as Org"))

(defun ide-org-copy-as-markdown (beg end)
  "Copy the region (Org) to the clipboard as GFM Markdown."
  (interactive "r")
  (kill-new (ide-markdown--org-to-md (buffer-substring-no-properties beg end)))
  (message "Copied as Markdown"))

(use-package ox-gfm :after ox :defer t)

(defun ide-org-export-markdown ()
  "Export this Org buffer to a sibling .md file.
ox-gfm (Org's own exporter, so babel results, includes and macros are honoured) is used
when installed, else pandoc.  Exporting evaluates source blocks, like any Org export."
  (interactive)
  (ide-lang-require-file)
  (let ((out (concat (file-name-sans-extension buffer-file-name) ".md")))
    (if (require 'ox-gfm nil t)
        (org-export-to-file 'gfm out)
      (with-temp-file out
        (insert (ide-markdown--org-to-md (buffer-substring-no-properties (point-min) (point-max))))))
    (message "Exported %s%s" out (if (featurep 'ox-gfm) " (ox-gfm)" " (pandoc)"))
    (find-file-other-window out)))

(defvar-local ide-markdown--orgtbl-fixing nil)

(defun ide-markdown--orgtbl-fix-hlines (&rest _)
  "After Org aligned a table in a Markdown buffer, turn |---+---| back into GFM's |---|---|."
  (when (and (derived-mode-p 'markdown-mode) (not ide-markdown--orgtbl-fixing))
    (save-excursion
      (let ((end (save-excursion (while (and (not (eobp)) (looking-at "[ \t]*|")) (forward-line 1)) (point))))
        (goto-char (line-beginning-position))
        (while (and (> (point) (point-min)) (save-excursion (forward-line -1) (looking-at "[ \t]*|")))
          (forward-line -1))
        (while (< (point) end)
          (when (looking-at "[ \t]*|[-+:]+|?[-+:|]*$")
            (let ((bound (line-end-position)))
              (while (re-search-forward "+" bound t) (replace-match "|"))))
          (forward-line 1))))))

(define-minor-mode ide-markdown-orgtbl-mode
  "Edit pipe tables with Org's table commands (the classic orgtbl trick).
TAB and S-TAB move between cells, M-arrows move rows and columns, C-c ^ sorts.  Org's
`|---+---|' rule lines are rewritten to GFM's `|---|---|' after each alignment.  Formulas
(#+TBLFM) are an Org-only concept and would not render, so do not use them in Markdown."
  :lighter " OrgTbl"
  (if ide-markdown-orgtbl-mode
      (progn (require 'org-table)
             (orgtbl-mode 1)
             (advice-add 'org-table-align :after #'ide-markdown--orgtbl-fix-hlines))
    (orgtbl-mode -1)))

;;;; Running fenced blocks -------------------------------------------------------------------------------------------------

(defconst ide-markdown-run-languages
  '(("sh" "bash" "sh") ("bash" "bash" "sh") ("shell" "bash" "sh") ("zsh" "zsh" "sh")
    ("python" "python3" "py") ("py" "python3" "py") ("python3" "python3" "py")
    ("js" "node" "js") ("javascript" "node" "js") ("node" "node" "js")
    ("ts" "tsx" "ts") ("typescript" "tsx" "ts")
    ("lua" "lua" "lua") ("ruby" "ruby" "rb") ("perl" "perl" "pl")
    ("go" "go run" "go") ("sql" "sqlite3" "sql") ("sqlite" "sqlite3" "sql"))
  "Fence language to (INTERPRETER FILE-EXTENSION) of the program that runs the block.
sql runs against an empty in-memory SQLite database.")

(defun ide-markdown--run-spec (lang)
  "Command (a list) and extension that run a LANG block, or nil when no interpreter is installed."
  (when-let* ((e (assoc lang ide-markdown-run-languages)))
    (let ((prog (split-string (nth 1 e))))
      (when (executable-find (car prog))
        (cons (if (equal (car prog) "sqlite3")
                  (list "sqlite3" "-header" "-column" ":memory:" ".read")
                prog)
              (nth 2 e))))))

(defun ide-markdown--block-command (block)
  "Write BLOCK's body to a file and return (COMMAND-STRING . FILE), after asking to run it."
  (let* ((lang (or (plist-get block :lang) (user-error "The fence has no language")))
         (spec (or (ide-markdown--run-spec lang)
                   (user-error "No interpreter for `%s' blocks on PATH" lang)))
         (body (buffer-substring-no-properties (plist-get block :body-start) (plist-get block :body-end)))
         (file (expand-file-name (format "block-%d.%s" (emacs-pid) (cdr spec))
                                 (let ((d (expand-file-name "markdown-run/" ide-cache-dir)))
                                   (make-directory d t) d))))
    ;; The same safety as the org-babel module: code is never run without asking.
    (unless (yes-or-no-p (format "Run this %s block (%d lines) with %s? "
                                 lang (with-temp-buffer (insert body) (count-lines (point-min) (point-max)))
                                 (car (car spec))))
      (user-error "Cancelled"))
    (with-temp-file file (insert body))
    (cons (ide-lang-command-string (append (car spec) (list file)))
          file)))

(defun ide-markdown--insert-output (block text)
  "Put TEXT in an `output' fence directly below BLOCK, replacing a previous one."
  (save-excursion
    (goto-char (plist-get block :end))
    (let ((next (cl-find-if (lambda (b) (and (equal (plist-get b :lang) "output")
                                             (<= (- (plist-get b :start) (plist-get block :end)) 2)
                                             (> (plist-get b :start) (plist-get block :end))))
                            (ide-markdown--fences))))
      (when next (delete-region (1- (plist-get next :start)) (plist-get next :end)))
      (goto-char (plist-get block :end))
      (insert "\n\n```output\n" (string-trim-right (ansi-color-filter-apply text)) "\n```"))))

(defun ide-markdown-run-block (&optional insert)
  "Run the fenced code block at point after confirmation.
The output goes to the drawer; with prefix INSERT it is put in an `output' fence below the block."
  (interactive "P")
  (let* ((block (or (ide-markdown--fence-at) (user-error "Point is not in a fenced code block")))
         (cmd (ide-markdown--block-command block))
         (dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory)))
    (if (not insert)
        (ide-lang-run (format "markdown %s" (plist-get block :lang)) (car cmd) dir)
      (let* ((src (current-buffer)) (marker (copy-marker (plist-get block :end)))
             (out (generate-new-buffer " *md-run*")) (default-directory dir))
        (set-process-sentinel
         (start-process-shell-command "markdown-run" out (car cmd))
         (lambda (p _e)
           (when (memq (process-status p) '(exit signal))
             (unwind-protect
                 (when (buffer-live-p src)
                   (with-current-buffer src
                     (save-excursion
                       (goto-char marker)
                       (when-let* ((b (ide-markdown--fence-at (max (point-min) (1- (marker-position marker))))))
                         (ide-markdown--insert-output b (with-current-buffer out (buffer-string)))))))
               (kill-buffer out)))))))))

(defun ide-markdown--run-command ()
  "`ide-run-command-functions' entry: the block at point, else a lint of the file."
  (when (derived-mode-p 'markdown-mode)
    (if-let* ((block (ide-markdown--fence-at)))
        (let ((lang (plist-get block :lang)))
          (when (ide-markdown--run-spec lang)
            (car (ide-markdown--block-command block))))
      (when (and buffer-file-name (executable-find "markdownlint-cli2"))
        (ide-lang-command-string
         (cons "markdownlint-cli2" (ide-markdown--lint-args (ide-lang-relative-file))))))))


;;;; Server -----------------------------------------------------------------------------------------------------------

(defun ide-markdown-lsp-toc ()
  "Ask marksman for its table-of-contents code action at point."
  (interactive)
  (ide-lang-code-action "source"))

(defun ide-markdown-start-server ()
  "Start marksman for this buffer whatever `ide-markdown-lsp' says."
  (interactive)
  (ide-lang-exe-or-error "marksman")
  (call-interactively #'eglot))

;;;; Org-style keys, menu -------------------------------------------------------------------------------------------------

(transient-define-prefix ide-markdown-org-menu ()
  "Markdown and Org interoperability (pandoc)."
  ["Markdown to Org"
   ("o" "region/buffer to new Org buffer" ide-markdown-to-org)
   ("r" "replace region with Org" ide-markdown-region-to-org)
   ("f" "file to .org" ide-markdown-file-to-org)
   ("c" "copy region as Org" ide-markdown-copy-as-org)
   ("p" "paste clipboard Markdown as Org" ide-org-paste-markdown)]
  ["Org to Markdown"
   ("m" "region/buffer to new Markdown buffer" ide-org-to-markdown)
   ("R" "replace region with Markdown" ide-org-region-to-markdown)
   ("F" "file to .md" ide-org-file-to-markdown)
   ("C" "copy region as Markdown" ide-org-copy-as-markdown)
   ("P" "paste clipboard Org as Markdown" ide-markdown-paste-org)
   ("e" "export Org buffer to GFM" ide-org-export-markdown)]
  ["Clipboard HTML"
   ("h" "paste web page HTML (Org or Markdown)" ide-markdown-paste-rich)])

(transient-define-prefix ide-markdown-menu ()
  "Markdown: preview, export, structure, links, lint and format, run, server.
Linting runs markdownlint-cli2, which executes project JavaScript config: trust the project
first with M-x ide-lang-trust-project."
  [["Preview"
    ("p" "live preview" ide-markdown-preview)
    ("P" "preview mode off/on" ide-markdown-preview-mode)
    ("a" "auto refresh: save/idle/off" ide-markdown-preview-cycle-auto)
    ("F" "follow point" ide-markdown-preview-toggle-follow)
    ("s" "sync scroll now" ide-markdown-preview-sync)
    ("b" "open in system browser" ide-markdown-open-rendered)]
   ["Export / convert"
    ("e" "export with pandoc" ide-markdown-export)
    ("o" "Org interop..." ide-markdown-org-menu)
    ("O" "orgtbl for tables" ide-markdown-orgtbl-mode)]
   ["Structure"
    ("t" "TOC insert/update" ide-markdown-toc)
    ("h" "promote subtree" markdown-promote-subtree)
    ("H" "demote subtree" markdown-demote-subtree)
    ("g" "go to heading" consult-imenu)
    ("w" "word count" ide-markdown-word-count)
    ("x" "toggle task [ ]/[x]" ide-markdown-toggle-task)
    ("n" "renumber list" markdown-cleanup-list-numbers)]]
  [["Links / images"
    ("l" "insert link" markdown-insert-link)
    ("i" "insert image" markdown-insert-image)
    ("I" "paste clipboard image" ide-markdown-paste-image)
    ("f" "insert footnote" markdown-insert-footnote)
    ("T" "insert table" markdown-insert-table)
    ("c" "insert code block" ide-markdown-insert-code-block)
    ("k" "check relative links" ide-markdown-check-links)]
   ["Lint / format"
    ("L" "fix lint issues" ide-markdown-lint-fix)
    ("m" "lint project" ide-markdown-lint-project)
    ("M" "fix whole project" ide-markdown-lint-fix-project)
    ("=" "format (prettier)" ide-reformat-buffer)
    ("A" "align tables" ide-markdown-align-tables)
    ("S" "toggle format on save" ide-markdown-toggle-format-on-save)
    ("z" "spell check" ide-markdown-spell)]
   ["Run / server"
    ("r" "run block at point" ide-markdown-run-block)
    ("R" "run block, result below" (lambda () (interactive) (ide-markdown-run-block t)))
    ("E" "start marksman" ide-markdown-start-server)
    ("N" "rename (LSP)" eglot-rename)
    ("D" "references" xref-find-references)
    ("C" "TOC code action (LSP)" ide-markdown-lsp-toc)
    ("X" "restart server" eglot-reconnect)
    ("U" "trust project (enables lint)" ide-lang-trust-project)]])

(ide-lang-register '(markdown-mode gfm-mode markdown-ts-mode)
  :run #'ide-markdown--run-command
  :menu #'ide-markdown-menu)

(with-eval-after-load 'markdown-mode
  ;; Org-flavoured keys: C-c C-c is the markdown-mode command prefix.
  (define-key markdown-mode-command-map "p" #'ide-markdown-preview)
  (define-key markdown-mode-command-map "l" #'ide-markdown-preview-mode)
  (define-key markdown-mode-command-map "v" #'ide-markdown-preview)
  (define-key markdown-mode-command-map "e" #'ide-markdown-export)
  (define-key markdown-mode-command-map "r" #'ide-markdown-run-block)
  (define-key markdown-mode-command-map "k" #'ide-markdown-check-links)
  (define-key markdown-mode-command-map "L" #'ide-markdown-lint-fix)
  (define-key markdown-mode-command-map "a" #'ide-markdown-align-tables)
  (define-key markdown-mode-map (kbd "C-c C-x t") #'ide-markdown-toggle-task)
  (define-key markdown-mode-map (kbd "C-c C-x T") #'ide-markdown-toc)
  (define-key markdown-mode-map (kbd "C-c C-x w") #'ide-markdown-word-count)
  (define-key markdown-mode-map (kbd "C-c C-x o") #'ide-markdown-orgtbl-mode)
  (define-key markdown-mode-map (kbd "C-c C-a p") #'ide-markdown-paste-image)
  (define-key markdown-mode-map (kbd "C-c C-s k") #'ide-markdown-insert-code-block)
  (define-key markdown-mode-map (kbd "C-c o m") #'ide-markdown-org-menu)
  (define-key markdown-mode-map (kbd "M-S-<left>") #'markdown-promote-subtree)
  (define-key markdown-mode-map (kbd "M-S-<right>") #'markdown-demote-subtree))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o m") #'ide-markdown-org-menu))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
