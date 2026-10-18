;;; docs-update.el --- Documentation generator and updater -*- lexical-binding: t; -*-

;;; Commentary:
;; Keeps docs/index.html in sync with the live configuration.
;;
;; Updating is deterministic:
;;   1. FACTS    extracted from the running Emacs: keymaps, transient layouts,
;;               module headers, the Makefile, the mise manifest, test counts.
;;   2. MERGE    facts are merged into the page's embedded JSON data
;;               deterministically.  Removals, renames and counts are exact.
;;               Descriptions are extracted directly from symbol docstrings
;;               (ide-docs--doc1).  Existing prose is preserved.
;;   3. VERIFY   the candidate page is checked against a fresh Emacs
;;               (tests/docs-check.el).  Only a passing page replaces the old
;;               one, and the old one is backed up first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'json)
(require 'transient)
(require '15-docs)
(require '00-core)

(declare-function ide-run-with-timeout "00-core")
(declare-function ide-slurp "00-core")
(declare-function ide-user--example-names "16-user")
(defvar ide-keys-mode-map)
(defvar ide-mise-required-executables)
(defvar ide-mise-manifest)
(defvar ide-lang-menus)

(defcustom ide-docs-backup-dir (expand-file-name ".cache/docs-backups/" user-emacs-directory)
  "Where previous versions of the page are kept." :type 'directory :group 'ide-docs)
(defcustom ide-docs-candidate-file (expand-file-name ".cache/docs-candidate/index.html" user-emacs-directory)
  "Where a candidate page is written for validation." :type 'file :group 'ide-docs)
(defcustom ide-docs-keep-backups 10 "Number of page backups to keep." :type 'integer :group 'ide-docs)
(defcustom ide-docs-validate-function #'ide-docs--validate-subprocess
  "Function (CANDIDATE-FILE CALLBACK) checking the page and calling CALLBACK with (OK . OUTPUT)."
  :type 'function :group 'ide-docs)

(defconst ide-docs-prefix-re "\\`\\(ide\\|azdo\\|azure\\|iac\\|confluence\\)")
(defvar ide-docs-command-regexp (concat ide-docs-prefix-re "-")
  "Commands matching this are expected to be documented somewhere on the page.")

(defvar ide-docs-contextual-maps
  '((minuet-active-mode-map minuet ".")
    (corfu-map corfu "\\`corfu-insert\\'")
    (prog-mode-map prog-mode "\\`eglot-code-actions\\'")
    (magit-status-mode-map devops-azdo-magit "\\`azdo-")
    (magit-azdo-pr-section-map devops-azdo-magit ".")
    (magit-azdo-wi-section-map devops-azdo-magit ".")
    (azdo-review-mode-map devops-azdo ".")
    (azdo-threads-mode-map devops-azdo ".")
    (azdo-compose-mode-map devops-azdo "."))
  "Contextual keymaps to document: (MAP FEATURE COMMAND-REGEXP).")

(defvar ide-docs-extra-global-keys
  '(("C-." embark) ("C-;" embark) ("M-." xref) ("M-?" xref) ("[left-fringe mouse-1]" dap-mode)
    ("C-c o a" org) ("C-c o n" org) ("C-c o c" org-confluence))
  "Global keys bound outside `ide-keys-mode-map': (KEYS FEATURE).")

(defvar ide-docs-menus
  '(("services" ide-services-dispatch nil) ("azdo" azdo-dispatch nil)
    ("azdo-pr" azdo-pr-dispatch nil) ("azdo-boards" azdo-boards-dispatch nil)
    ("azure" azure-dispatch nil) ("iac" iac-dispatch nil)
    ("push" magit-push "\\`azdo-"))
  "Transient menus to document: (ID PREFIX-SYMBOL COMMAND-REGEXP-OR-NIL).")

(defvar ide-docs-ignored-make-targets '("all" "clean" "run"))
(defvar ide-docs-ignored-commands '(ide-focus-code-window ide-mise-install)
  "Interactive commands that are implementation details, not user features.")
(defvar ide-docs-category-hints
  '(("\\`ide-docs-" . "Help & docs")
    ("\\`ide-user-" . "Customize"))
  "Alist of (COMMAND-REGEXP . CATEGORY) for new keybindings.")

;;;; JSON data ------------------------------------------------------------------

(defconst ide-docs--data-open "<script type=\"application/json\" id=\"ide-data\">")

(defun ide-docs--data-bounds (html)
  "Return (START . END) of the JSON text inside the embedded data block of HTML."
  (let* ((open (string-search ide-docs--data-open html))
         (start (and open (+ open (length ide-docs--data-open))))
         (end (and start (string-search "</script>" html start))))
    (unless (and start end) (error "No embedded data block in the documentation page"))
    (cons start end)))

(defun ide-docs--parse-data (html)
  "Parse the embedded data of HTML into alists and vectors."
  (let ((b (ide-docs--data-bounds html)))
    (json-parse-string (substring html (car b) (cdr b))
                       :object-type 'alist :array-type 'array :null-object :null :false-object :false)))

(defun ide-docs--json-string (s)
  "Return S as a quoted JSON string, as proper multibyte text.
`json-serialize' yields raw UTF-8 bytes; decode them so callers can safely
concatenate, search and parse the result."
  (decode-coding-string (substring (json-serialize (vector s)) 1 -1) 'utf-8))

(defun ide-docs--alist-p (x) (and (consp x) (consp (car x)) (symbolp (car (car x)))))

(defun ide-docs--pp (x depth)
  "Pretty-print X (alists, vectors, scalars) as JSON at indentation DEPTH."
  (let ((pad (make-string (* 1 (1+ depth)) ?\s)) (pad0 (make-string depth ?\s)))
    (cond
     ((eq x t) "true") ((eq x :false) "false") ((eq x :null) "null") ((null x) "null")
     ((numberp x) (number-to-string x))
     ((stringp x) (ide-docs--json-string x))
     ((vectorp x)
      (cond ((zerop (length x)) "[]")
            ((and (seq-every-p #'stringp x) (< (apply #'+ (mapcar #'length x)) 90))
             (concat "[" (mapconcat (lambda (e) (ide-docs--pp e 0)) x ", ") "]"))
            (t (concat "[\n" (mapconcat (lambda (e) (concat pad (ide-docs--pp e (1+ depth)))) x ",\n")
                       "\n" pad0 "]"))))
     ((ide-docs--alist-p x)
      (concat "{\n" (mapconcat (lambda (kv) (concat pad (ide-docs--json-string (symbol-name (car kv)))
                                                    ": " (ide-docs--pp (cdr kv) (1+ depth))))
                               x ",\n")
              "\n" pad0 "}"))
     (t (error "Cannot serialize %S" x)))))

(defun ide-docs--serialize (data)
  "Serialize DATA to JSON that is safe inside a script element."
  (replace-regexp-in-string "</" "<\\/" (ide-docs--pp data 0) t t))

(defun ide-docs--render (html data)
  "Return HTML with its embedded data block replaced by DATA."
  (let ((b (ide-docs--data-bounds html)))
    (concat (substring html 0 (car b)) "\n" (ide-docs--serialize data) "\n" (substring html (cdr b)))))

(defalias 'ide-docs--slurp #'ide-slurp)

;;;; Small data helpers ---------------------------------------------------------

(defun ide-docs--list (vec) (append vec nil))
(defun ide-docs--vec (list) (vconcat list))
(defun ide-docs--put (obj key val)
  "Return copy of alist OBJ with KEY set to VAL, keeping key order."
  (let ((copy (copy-alist obj)))
    (if-let* ((cell (assq key copy))) (progn (setcdr cell val) copy)
      (append copy (list (cons key val))))))
(defun ide-docs--mk (&rest kvs)
  (cl-loop for (k v) on kvs by #'cddr when v collect (cons k v)))
(defun ide-docs--clean (s max)
  "Return S stripped of control characters and truncated to MAX, or nil."
  (when (and (stringp s) (not (string-empty-p (string-trim s))))
    (let ((c (string-trim (replace-regexp-in-string "[[:cntrl:]]+" " " s))))
      (if (> (length c) max) (concat (substring c 0 (- max 1)) "…") c))))

(defun ide-docs--doc1 (sym)
  "First line of SYM's docstring, or nil."
  (when-let* ((d (and (symbolp sym) (fboundp sym) (ignore-errors (documentation sym t)))))
    (let ((line (car (split-string d "\n" t))))
      (and line (string-trim (string-remove-suffix "." line))))))

(defun ide-docs--humanize (sym)
  (let ((s (replace-regexp-in-string "\\`\\(ide\\|azdo\\|azure\\|iac\\)-" "" (symbol-name sym))))
    (capitalize (replace-regexp-in-string "-" " " s))))

(defun ide-docs--norm-keys (keys)
  "Canonical key description for KEYS (\"[...]\" vector strings are left alone)."
  (if (string-prefix-p "[" keys) keys (key-description (kbd keys))))

;;;; Facts ---------------------------------------------------------------------------

(defun ide-docs--keymap-bindings (map &optional prefix)
  "Return ((KEY-DESCRIPTION . COMMAND) ...) for MAP, descending into prefix maps."
  (let (bindings)
    (cl-labels
        ((descend (m pref)
           (map-keymap
            (lambda (ev binding)
              (let* ((name (if (integerp ev) (single-key-description ev) (format "<%s>" ev)))
                     (full (if pref (concat pref " " name) name)))
                (cond
                 ((keymapp binding) (descend binding full))
                 ((symbolp binding)
                  (let ((fn (indirect-function binding)))
                    (if (keymapp fn) (descend fn full)
                      (push (cons full binding) bindings))))
                 ((and (consp binding) (eq (car binding) 'lambda))
                  (push (cons full binding) bindings)))))
            m)))
      (descend map prefix))
    (nreverse bindings)))

(defun ide-docs--dedupe-aliases (bindings)
  "Drop <tab>/<return> duplicates of TAB/RET with the same command."
  (cl-remove-if (lambda (b)
                  (let ((alt (pcase (car b) ("<tab>" "TAB") ("<return>" "RET"))))
                    (and alt (eq (cdr (assoc alt bindings)) (cdr b)))))
                bindings))

(defun ide-docs--facts-keybinds ()
  "Return list of (KEYS CMD CTX) for every documented binding."
  (let (out)
    (dolist (b (ide-docs--dedupe-aliases (ide-docs--keymap-bindings ide-keys-mode-map)))
      (when (commandp (cdr b))
        (push (list (car b) (cdr b) "global") out)))
    (dolist (e ide-docs-extra-global-keys)
      (require (cadr e) nil t)
      (let* ((keys (car e))
             (spec (if (string-prefix-p "[" keys) (car (read-from-string keys)) (kbd keys)))
             (cmd (key-binding spec)))
        (when (and cmd (symbolp cmd) (commandp cmd))
          (push (list keys cmd "global") out))))
    (dolist (m ide-docs-contextual-maps)
      (require (nth 1 m) nil t)
      (when (and (boundp (car m)) (keymapp (symbol-value (car m))))
        (dolist (b (ide-docs--dedupe-aliases (ide-docs--keymap-bindings (symbol-value (car m)))))
          (when (and (commandp (cdr b))
                     (string-match-p (nth 2 m) (symbol-name (cdr b))))
            (push (list (car b) (cdr b) (symbol-name (car m))) out)))))
    (nreverse out)))

(defun ide-docs--layout-groups (node)
  "Flatten transient layout NODE into ((TITLE . ITEMS)...); items are (KEY LABEL CMD)."
  (let (groups)
    (cl-labels
        ((suffix-p (x) (and (consp x) (symbolp (car x)) (plist-get (cdr x) :key)))
         (items-of (children)
           (cl-loop for c in children when (suffix-p c)
                    collect (list (plist-get (cdr c) :key)
                                  (let ((d (plist-get (cdr c) :description))) (and (stringp d) d))
                                  (let ((cmd (plist-get (cdr c) :command)))
                                    (and (symbolp cmd) (not (string-prefix-p "transient:" (symbol-name cmd))) cmd)))))
         (walk (n)
           (cond
            ((vectorp n)
             (let* ((props (cl-find-if (lambda (e) (and (consp e) (keywordp (car e)))) (append n nil)))
                    (children (cl-find-if (lambda (e) (and (consp e) (not (keywordp (car e))))) (append n nil)))
                    (items (items-of children)))
               (when items (push (cons (let ((d (plist-get props :description))) (and (stringp d) d)) items) groups))
               (dolist (c children) (when (vectorp c) (walk c)))))
            ((consp n) (dolist (c n) (walk c))))))
      (walk node))
    (nreverse groups)))

(defun ide-docs--facts-menus ()
  "Return list of (ID PREFIX GROUPS) for the documented transient menus."
  (require 'magit nil t)
  (cl-loop for (id prefix filter) in ide-docs-menus
           for layout = (get prefix 'transient--layout)
           when layout
           collect (list id prefix
                         (cl-loop for (title . items) in (ide-docs--layout-groups layout)
                                  for kept = (cl-remove-if-not
                                              (lambda (i) (or (null filter)
                                                              (and (nth 2 i) (string-match-p filter (symbol-name (nth 2 i))))))
                                              items)
                                  when kept collect (cons title kept)))))

(defun ide-docs--module-files ()
  "Relative paths of the Emacs Lisp files that make up the config."
  (let ((root (file-name-as-directory (expand-file-name user-emacs-directory))))
    (append '("early-init.el" "init.el")
            (sort (mapcar (lambda (f) (file-relative-name f root))
                          (directory-files-recursively (expand-file-name "modules" root) "\\.el\\'"))
                  #'string<))))

(defun ide-docs--file-header (rel)
  "The description from the first line of config file REL."
  (with-temp-buffer
    (insert-file-contents (expand-file-name rel user-emacs-directory) nil 0 400)
    (when (looking-at ";;; [^ ]+ --- \\(.*?\\)\\(?: -\\*-.*\\)?$") (string-trim (match-string 1)))))

(defun ide-docs--facts-make ()
  "Return ((TARGET . COMMENT-OR-NIL)...) from the Makefile."
  (let ((mk (expand-file-name "Makefile" user-emacs-directory)) out prev)
    (when (file-exists-p mk)
      (with-temp-buffer
        (insert-file-contents mk)
        (dolist (line (split-string (buffer-string) "\n"))
          (cond ((string-match "\\`# \\(.*\\)" line) (setq prev (match-string 1 line)))
                ((string-match "\\`\\([a-z][a-z-]*\\):" line)
                 (let ((tg (match-string 1 line)))
                   (unless (member tg ide-docs-ignored-make-targets) (push (cons tg prev) out)))
                 (setq prev nil))
                (t (setq prev nil))))))
    (nreverse out)))

(defun ide-docs--count-tests ()
  (let ((n 0))
    (dolist (f (directory-files (expand-file-name "tests" user-emacs-directory) t "\\.el\\'"))
      (with-temp-buffer (insert-file-contents f)
                        (while (re-search-forward "^(ert-deftest " nil t) (cl-incf n))))
    n))

(defun ide-docs--user-commands ()
  "Interactive commands matching `ide-docs-command-regexp' (no -- internals)."
  (let (r)
    (mapatoms (lambda (s) (when (and (fboundp s) (commandp s)
                                     (string-match-p ide-docs-command-regexp (symbol-name s))
                                     (not (string-match-p "--\\|-mode\\'" (symbol-name s)))
                                     (not (memq s ide-docs-ignored-commands)))
                            (push s r))))
    (sort r (lambda (a b) (string< (symbol-name a) (symbol-name b))))))

(defvar ide-docs-example-order '("settings" "elfeed" "rust" "org-extras" "github-package" "shell-tools")
  "Display order of the shipped user-config examples; others follow alphabetically.")

(defun ide-docs--example-meta (text kind)
  "Return (TITLE . BLURB) from the leading comments of an example's TEXT (KIND `el' or `mise')."
  (if (eq kind 'el)
      (cons (and (string-match "\\`;;; [^ ]+ --- \\(.*?\\) -\\*-" text) (match-string 1 text))
            (and (string-match ";;; Commentary:\n\\(\\(?:;; .*\n\\)+\\)" text)
                 (string-join (mapcar (lambda (l) (string-trim (string-remove-prefix ";;" l)))
                                      (split-string (match-string 1 text) "\n" t))
                              " ")))
    (let ((lines (cl-loop for l in (split-string text "\n") while (string-prefix-p "#" l)
                          collect (string-trim (string-remove-prefix "#" l)))))
      (cons (car lines) (string-join (cdr lines) " ")))))

(defun ide-docs--facts-examples ()
  "Return the shipped user-config examples as data objects, read from examples/."
  (let* ((dir (expand-file-name "examples/" user-emacs-directory))
         (ids (if (fboundp 'ide-user--example-names)
                  (ide-user--example-names)
                (delete-dups (mapcar (lambda (f) (car (split-string (file-name-nondirectory f) "\\.")))
                                     (and (file-directory-p dir) (directory-files dir t "\\.example\\'"))))))
         (ids (append (cl-remove-if-not (lambda (i) (member i ids)) ide-docs-example-order)
                      (sort (cl-remove-if (lambda (i) (member i ide-docs-example-order)) ids) #'string<))))
    (mapcar
     (lambda (id)
       (let* ((el (expand-file-name (concat id ".el.example") dir))
              (mise (expand-file-name (concat id ".mise.example") dir))
              (entries (append (and (file-exists-p el) (list (list "user/%s.el" el 'el)))
                               (and (file-exists-p mise) (list (list "user/mise.toml" mise 'mise)))))
              (main (car entries))
              (meta (ide-docs--example-meta (ide-slurp (nth 1 main)) (nth 2 main))))
         (ide-docs--mk 'id id 'title (or (car meta) id) 'blurb (or (cdr meta) "")
                       'files (ide-docs--vec
                               (mapcar (lambda (e) (ide-docs--mk 'path (if (eq (nth 2 e) 'el) (format "user/%s.el" id) (nth 0 e))
                                                                 'code (ide-slurp (nth 1 e))))
                                       entries)))))
     ids)))

(defconst ide-docs-language-table
  '((ide-rust-dispatch :id "rust" :name "Rust"
     :summary "Cargo build, run and test, rust-analyzer, clippy, nextest, debugging and crate management."
     :tools (("Language server" "rust-analyzer") ("Build" "cargo") ("Test runner" "cargo-nextest"))
     :run (ide-rust--run-command) :debug (ide-rust-debug))
    (ide-web-dispatch :id "web" :name "TypeScript, JavaScript, HTML and CSS"
     :summary "npm scripts, tests, Node debugging, live preview and package management for web projects."
     :tools (("Language server" "vscode-html-language-server" "vscode-css-language-server") ("Compiler" "tsc")
             ("Formatter" "prettier") ("Linter" "eslint") ("Runtime" "node" "tsx"))
     :run (ide-web--run-command) :debug (ide-web--debug-hook))
    (ide-lua-dispatch :id "lua" :name "Lua"
     :summary "Run files and snippets, REPL, busted tests and project configuration."
     :tools (("Language server" "lua-language-server") ("Formatter" "stylua"))
     :run (ide-lua--run-command))
    (ide-powershell-dispatch :id "powershell" :name "PowerShell"
     :summary "Run scripts, Pester tests and debugging through PowerShell Editor Services."
     :tools (("Runtime" "pwsh"))
     :run (ide-powershell--run-command) :debug (ide-powershell-debug))
    (ide-k8s-dispatch :id "k8s" :name "Helm, Kubernetes and CI YAML"
     :summary "Render and validate Helm charts, kubectl actions and CI workflow checks."
     :tools (("Language server" "helm_ls" "yaml-language-server") ("Helm" "helm") ("Cluster" "kubectl" "kind")
             ("Linter" "kubeconform" "kube-linter" "yamllint" "actionlint"))
     :run (ide-k8s--run-command) :debug (ide-k8s--debug))
    (ide-toml-dispatch :id "toml" :name "TOML"
     :summary "Format, sort keys, validate and use schemas."
     :tools (("Language server" "taplo") ("Formatter" "taplo")))
    (ide-markdown-menu :id "markdown" :name "Markdown"
     :summary "Preview, export, outline, links, lint and format."
     :tools (("Language server" "marksman") ("Linter" "markdownlint-cli2") ("Converter" "pandoc"))
     :run (ide-markdown--run-command))
    (ide-bash-dispatch :id "bash" :name "Bash"
     :summary "Run, check, format and explain shell scripts."
     :tools (("Language server" "bash-language-server") ("Linter" "shellcheck") ("Formatter" "shfmt")))
    (ide-dockerfile-dispatch :id "dockerfile" :name "Dockerfile and Compose"
     :summary "Build, run, scan and lint images; Compose actions."
     :tools (("Language server" "docker-langserver") ("Linter" "hadolint") ("Scanner" "trivy"))
     :run (ide-dockerfile--run-command))
    (ide-terraform-dispatch :id "terraform" :name "Terraform"
     :summary "Lint, validate, scan and explore; plan and apply live in the IaC menu."
     :tools (("Language server" "terraform-ls") ("CLI" "terraform") ("Linter" "tflint") ("Scanner" "trivy")))
    (go :id "go" :name "Go" :modes (go-ts-mode go-mod-ts-mode) :files (".go" "go.mod")
     :summary "gopls, goimports on save, go-test in the drawer and Delve debugging (no language menu)."
     :tools (("Language server" "gopls") ("Debugger" "dlv")))
    (python :id "python" :name "Python" :modes (python-ts-mode python-mode) :files (".py")
     :summary "Pyright and Ruff, pytest in the drawer, debugpy, automatic virtualenvs (no language menu)."
     :tools (("Language server" "pyright-langserver") ("Formatter" "ruff") ("Linter" "ruff") ("Runtime" "python" "uv")))
    (ansible :id "ansible" :name "Ansible" :modes (yaml-ts-mode) :files ("playbooks/, roles/, group_vars/")
     :summary "Ansible minor mode on YAML, ansible-doc and ansible-language-server (no language menu)."
     :tools (("Language server" "ansible-language-server") ("CLI" "ansible"))))
  "Hand-written facts of the Languages cards: (KEY . PLIST).")

(defun ide-docs--hooked-p (hook syms)
  "Non-nil when one of SYMS is (or is called by a lambda) in the value of HOOK."
  (let ((print-level 10) (print-length 60))
    (cl-some (lambda (s)
               (cl-some (lambda (f)
                          (if (symbolp f) (eq f s)
                            (string-match-p (regexp-quote (symbol-name s)) (prin1-to-string f))))
                        (and (boundp hook) (symbol-value hook))))
             syms)))

(defun ide-docs--lang-files (modes)
  (let (out)
    (dolist (m modes)
      (dolist (cell auto-mode-alist)
        (when (eq (cdr cell) m)
          (let ((re (car cell)))
            (when (string-match "\\\\\\.\\([a-z0-9]+\\)\\\\?\\'" re)
              (push (concat "." (match-string 1 re)) out))))))
    out))

(defun ide-docs--facts-languages ()
  "Return a list of language fact plists from the live config and `ide-docs-language-table'."
  (require 'transient)
  (let* ((registered (mapcar #'cdr ide-lang-menus))
         (keys (append (mapcar #'car ide-docs-language-table)
                       (sort (cl-remove-if (lambda (c) (assq c ide-docs-language-table)) (copy-sequence registered))
                             (lambda (a b) (string< (symbol-name a) (symbol-name b))))))
         out)
    (dolist (key keys)
      (let* ((spec (cdr (assq key ide-docs-language-table)))
             (menu (and (memq key registered) key))
             (modes (if menu (car (rassq key ide-lang-menus)) (plist-get spec :modes)))
             (name (or (plist-get spec :name) (ide-docs--humanize key))))
        (when (or menu modes)
          (push (list :id (or (plist-get spec :id) (replace-regexp-in-string "-\\(dispatch\\|menu\\)\\'" "" (replace-regexp-in-string "\\`ide-" "" (symbol-name key))))
                      :name name
                      :summary (or (plist-get spec :summary) (and menu (ide-docs--doc1 menu)) name)
                      :modes (mapcar #'symbol-name modes)
                      :files (sort (delete-dups (append (ide-docs--lang-files modes) (plist-get spec :files))) #'string<)
                      :menu menu
                      :tools (cl-loop for (role . exes) in (plist-get spec :tools)
                                      append (mapcar (lambda (e) (list role e)) exes))
                      :run (ide-docs--hooked-p 'ide-run-command-functions (plist-get spec :run))
                      :debug (ide-docs--hooked-p 'ide-debug-functions (plist-get spec :debug))
                      :groups (and menu (ide-docs--layout-groups (get menu 'transient--layout))))
                out))))
    (nreverse out)))

(defun ide-docs--facts ()
  "Collect ground-truth facts from the running configuration."
  (list :examples (ide-docs--facts-examples)
        :keybinds (ide-docs--facts-keybinds)
        :menus (ide-docs--facts-menus)
        :languages (ide-docs--facts-languages)
        :tools (mapcar #'car ide-mise-required-executables)
        :modules (ide-docs--module-files)
        :make (ide-docs--facts-make)
        :commands (ide-docs--user-commands)
        :tests (ide-docs--count-tests)))

;;;; Merge ----------------------------------------------------------------------

(defun ide-docs--merge-rows (old new-keys key-fn make-row)
  "Merge NEW-KEYS into OLD rows using KEY-FN and MAKE-ROW.
Return (VECTOR ADDED REMOVED)."
  (let* ((old-list (ide-docs--list old))
         (kept (cl-remove-if-not (lambda (r) (member (funcall key-fn r) new-keys)) old-list))
         (removed (cl-remove-if (lambda (k) (member k new-keys)) (mapcar key-fn old-list)))
         (added (cl-remove-if (lambda (k) (cl-find k old-list :key key-fn :test #'equal)) new-keys)))
    (list (ide-docs--vec (append kept (mapcar make-row added))) added removed)))

(defun ide-docs--guess-tool-spec (exe)
  "Guess the mise.toml spec providing EXE."
  (let* ((aliases '(("pyright-langserver" . "pyright") ("docker-langserver" . "dockerfile-language-server")
                    ("dlv" . "delve") ("rg" . "ripgrep") ("az" . "azure-cli") ("ansible" . "ansible-core")
                    ("cargo" . "rust") ("rust-analyzer" . "rust") ("tsc" . "typescript")
                    ("vscode-html-language-server" . "vscode-langservers-extracted")
                    ("vscode-css-language-server" . "vscode-langservers-extracted")
                    ("pwsh" . "powershell") ("helm_ls" . "helm-ls")))
         (needle (or (cdr (assoc exe aliases)) exe))
         (toml (and (boundp 'ide-mise-manifest) (file-exists-p ide-mise-manifest) (ide-slurp ide-mise-manifest)))
         spec)
    (when toml
      (dolist (line (split-string toml "\n"))
        (when (and (not spec) (string-match "\\`\"?\\([^\"= ]+\\)\"? *=" line)
                   (string-match-p (regexp-quote needle) (match-string 1 line)))
          (setq spec (match-string 1 line)))))
    (or spec exe)))

(defun ide-docs--merge-tools (old exes)
  "Return (VECTOR ADDED REMOVED) for the tool table."
  (ide-docs--merge-rows old exes (lambda (r) (alist-get 'exe r))
                        (lambda (e) (ide-docs--mk 'exe e 'desc e 'cat "Other" 'spec (ide-docs--guess-tool-spec e)))))

(defun ide-docs--merge-modules (old files)
  "Return (VECTOR ADDED REMOVED) for the module map."
  (let* ((res (ide-docs--merge-rows old files (lambda (r) (alist-get 'file r))
                                    (lambda (f) (ide-docs--mk 'file f 'desc (or (ide-docs--file-header f) f)))))
         (sorted (sort (ide-docs--list (car res)) (lambda (a b) (string< (alist-get 'file a) (alist-get 'file b))))))
    (list (ide-docs--vec sorted) (nth 1 res) (nth 2 res))))

(defun ide-docs--merge-make (old targets)
  "Return (VECTOR ADDED REMOVED) for the make-target table; TARGETS is ((NAME . COMMENT))."
  (ide-docs--merge-rows old (mapcar (lambda (tg) (concat "make " (car tg))) targets)
                        (lambda (r) (alist-get 'cmd r))
                        (lambda (cmd)
                          (let ((tg (string-remove-prefix "make " cmd)))
                            (ide-docs--mk 'cmd cmd 'desc (or (cdr (assoc tg targets)) tg))))))

(defun ide-docs--merge-language (old f)
  "Build the data card for fact F, keeping hand-written name, summary and `does' text of OLD."
  (let* ((old-items (and old (cl-loop for g across (alist-get 'groups old) append (ide-docs--list (alist-get 'items g)))))
         (groups
          (cl-loop for (title . items) in (plist-get f :groups)
                   collect (ide-docs--mk
                            'title (or title "Commands")
                            'items (ide-docs--vec
                                    (cl-loop for i in items
                                             for cmd = (and (nth 2 i) (symbol-name (nth 2 i)))
                                             for label = (or (nth 1 i) (and (nth 2 i) (ide-docs--humanize (nth 2 i))) (nth 0 i))
                                             for o = (cl-find (nth 0 i) old-items :key (lambda (x) (alist-get 'key x)) :test #'equal)
                                             collect (ide-docs--mk 'key (nth 0 i) 'label label 'cmd cmd
                                                                   'does (or (and o (equal (alist-get 'cmd o) cmd) (alist-get 'does o))
                                                                             (and (nth 2 i) (ide-docs--doc1 (nth 2 i))) label)))))))
         (tools (mapcar (lambda (r) (ide-docs--mk 'role (car r) 'exe (cadr r))) (plist-get f :tools))))
    (ide-docs--mk 'id (plist-get f :id)
                  'name (or (and old (alist-get 'name old)) (plist-get f :name))
                  'summary (or (and old (alist-get 'summary old)) (plist-get f :summary))
                  'modes (ide-docs--vec (plist-get f :modes))
                  'files (ide-docs--vec (plist-get f :files))
                  'menu (and (plist-get f :menu) (symbol-name (plist-get f :menu)))
                  'open (and (plist-get f :menu) "C-c m")
                  'tools (ide-docs--vec tools)
                  'run (and (plist-get f :run) "S-<f10>")
                  'debug (and (plist-get f :debug) "S-<f9>")
                  'groups (ide-docs--vec groups))))

(defun ide-docs--merge-languages (old facts)
  "Return (VECTOR ADDED REMOVED) language ids merging language FACTS into OLD cards."
  (let* ((old-list (ide-docs--list old))
         (ids (mapcar (lambda (f) (plist-get f :id)) facts))
         (cards (mapcar (lambda (f) (ide-docs--merge-language
                                     (cl-find (plist-get f :id) old-list :key (lambda (c) (alist-get 'id c)) :test #'equal) f))
                        facts)))
    (list (ide-docs--vec cards)
          (cl-remove-if (lambda (i) (cl-find i old-list :key (lambda (c) (alist-get 'id c)) :test #'equal)) ids)
          (cl-remove-if (lambda (i) (member i ids)) (mapcar (lambda (c) (alist-get 'id c)) old-list)))))

(defun ide-docs--fact-id (f) (cons (ide-docs--norm-keys (nth 0 f)) (nth 2 f)))
(defun ide-docs--kb-id (k) (cons (ide-docs--norm-keys (alist-get 'keys k)) (alist-get 'ctx k)))

(defun ide-docs--infer-category (keys cmd existing)
  "Category for a new global binding KEYS of CMD, from hints and sibling bindings."
  (or (cl-loop for (re . cat) in ide-docs-category-hints
               when (string-match-p re (symbol-name cmd)) return cat)
      (let* ((chords (split-string keys " "))
             (prefix (and (> (length chords) 1) (concat (string-join (butlast chords) " ") " "))))
        (when prefix
          (cl-loop for e in existing
                   when (and (equal (alist-get 'ctx e) "global")
                             (string-prefix-p prefix (ide-docs--norm-keys (alist-get 'keys e))))
                   return (alist-get 'cat e))))
      "Other"))

(defun ide-docs--insert-after-cat (list entry)
  "Insert ENTRY after the last element of LIST with the same `cat', else at the end."
  (let ((idx (cl-position (alist-get 'cat entry) list :key (lambda (e) (alist-get 'cat e)) :test #'equal :from-end t)))
    (if idx (append (cl-subseq list 0 (1+ idx)) (list entry) (nthcdr (1+ idx) list))
      (append list (list entry)))))

(defun ide-docs--merge-keybinds (old facts)
  "Merge FACTS into the OLD keybind vector.  Return (VECTOR ADDED REMOVED CHANGED) ids."
  (let* ((old-list (ide-docs--list old)) added removed changed result)
    (dolist (e old-list)
      (let* ((id (ide-docs--kb-id e))
             (f (cl-find id facts :key #'ide-docs--fact-id :test #'equal)))
        (cond ((null f) (push id removed))
              ((equal (symbol-name (nth 1 f)) (alist-get 'cmd e)) (push e result))
              (t (push id changed) (push (ide-docs--put e 'cmd (symbol-name (nth 1 f))) result)))))
    (setq result (nreverse result))
    (dolist (f facts)
      (let ((id (ide-docs--fact-id f)))
        (unless (cl-find id old-list :key #'ide-docs--kb-id :test #'equal)
          (push id added)
          (let* ((cmd (nth 1 f))
                 (title (ide-docs--humanize cmd))
                 (entry (ide-docs--mk 'keys (nth 0 f) 'cmd (symbol-name cmd) 'title title
                                      'desc (or (ide-docs--doc1 cmd) title)
                                      'cat (if (equal (nth 2 f) "global")
                                               (ide-docs--infer-category (nth 0 f) cmd (append old-list result))
                                             "Contextual")
                                      'ctx (nth 2 f))))
            (setq result (ide-docs--insert-after-cat result entry))))))
    (list (ide-docs--vec result) (nreverse added) (nreverse removed) (nreverse changed))))

(defun ide-docs--merge-menu (existing fact)
  "Return (MENU ADDED CHANGED) merging FACT (ID PREFIX GROUPS) into EXISTING (or nil)."
  (let* ((old-items (and existing (cl-loop for g across (alist-get 'groups existing)
                                           append (ide-docs--list (alist-get 'items g)))))
         (old-groups (and existing (ide-docs--list (alist-get 'groups existing))))
         added changed groups)
    (dolist (fg (nth 2 fact))
      (let* ((items
              (mapcar
               (lambda (fi)
                 (let* ((key (nth 0 fi)) (label (or (nth 1 fi) (and (nth 2 fi) (ide-docs--humanize (nth 2 fi))) key))
                        (cmd (and (nth 2 fi) (symbol-name (nth 2 fi))))
                        (old (cl-find key old-items :key (lambda (i) (alist-get 'key i)) :test #'equal)))
                   (if old
                       (let ((o (ide-docs--put old 'label label)))
                         (cond ((and cmd (not (equal cmd (alist-get 'cmd old))))
                                (push (cons (car fact) key) changed) (ide-docs--put o 'cmd cmd))
                               ((and (null cmd) (alist-get 'cmd o)) (assq-delete-all 'cmd (copy-alist o)))
                               (t o)))
                     (push (cons (car fact) key) added)
                     (ide-docs--mk 'key key 'label label
                                   'does (or (and (nth 2 fi) (ide-docs--doc1 (nth 2 fi))) label)
                                   'cmd cmd))))
               (cdr fg)))
             (title (or (car fg)
                        (let ((g (cl-find-if (lambda (g) (seq-some (lambda (i) (equal (alist-get 'key i) (nth 0 (cadr fg))))
                                                                    (alist-get 'items g)))
                                             old-groups)))
                          (and g (alist-get 'title g)))
                        "Commands")))
        (push (ide-docs--mk 'title title 'items (ide-docs--vec items)) groups)))
    (let ((menu (if existing
                    (ide-docs--put existing 'groups (ide-docs--vec (nreverse groups)))
                  (ide-docs--mk 'id (car fact) 'name (or (ide-docs--doc1 (nth 1 fact)) (car fact))
                                'prefix (symbol-name (nth 1 fact)) 'open ""
                                'blurb (or (ide-docs--doc1 (nth 1 fact)) "")
                                'groups (ide-docs--vec (nreverse groups))))))
      (list menu (nreverse added) (nreverse changed)))))

(defun ide-docs--merge-menus (old facts)
  "Return (VECTOR ADDED CHANGED) merging menu FACTS into OLD."
  (let ((menus (ide-docs--list old)) added changed)
    (dolist (f facts)
      (let* ((existing (cl-find (car f) menus :key (lambda (m) (alist-get 'id m)) :test #'equal))
             (r (ide-docs--merge-menu existing f)))
        (setq added (append added (nth 1 r)) changed (append changed (nth 2 r)))
        (setq menus (if existing
                        (mapcar (lambda (m) (if (eq m existing) (car r) m)) menus)
                      (append menus (list (car r)))))))
    (list (ide-docs--vec menus) added changed)))

(defun ide-docs--layout-cmds (groups)
  "Return all command names from transient GROUPS."
  (cl-loop for g across (if (vectorp groups) groups (vconcat groups))
           append (cl-loop for i across (alist-get 'items g)
                           when (alist-get 'cmd i) collect (alist-get 'cmd i))))

(defun ide-docs--data-commands (data)
  "All command names mentioned in keybinds, menus and feature cards of DATA."
  (append (mapcar (lambda (k) (alist-get 'cmd k)) (ide-docs--list (alist-get 'keybinds data)))
          (cl-loop for m across (alist-get 'menus data) append (ide-docs--layout-cmds (alist-get 'groups m)))
          (cl-loop for f across (alist-get 'features data) append (ide-docs--list (alist-get 'cmds f)))
          (cl-loop for l across (alist-get 'languages data)
                   append (append (and (alist-get 'menu l) (list (alist-get 'menu l)))
                                  (ide-docs--layout-cmds (alist-get 'groups l))))))

(defun ide-docs--merge-features (old data commands)
  "Drop vanished commands from cards.  Return (VECTOR DROPPED UNCOVERED)."
  (let (dropped)
    (let ((cards (mapcar (lambda (f)
                           (let ((keep (cl-remove-if-not
                                        (lambda (c) (or (fboundp (intern c)) (progn (push c dropped) nil)))
                                        (ide-docs--list (alist-get 'cmds f)))))
                             (ide-docs--put f 'cmds (ide-docs--vec keep))))
                         (ide-docs--list old))))
      (let* ((covered (ide-docs--data-commands (ide-docs--put data 'features (ide-docs--vec cards))))
             (uncovered (cl-remove-if (lambda (c) (member (symbol-name c) covered)) commands)))
        (list (ide-docs--vec cards) (nreverse dropped) uncovered)))))

(defun ide-docs--workflow-problems (data keys)
  "Return ((WORKFLOW-ID . STEP-INDEX) ...) whose text names unbound keys or missing commands."
  (let (bad)
    (dolist (w (ide-docs--list (alist-get 'workflows data)))
      (cl-loop for s across (alist-get 'steps w) for i from 0
               for text = (alist-get 'd s)
               do (let ((pos 0) stale)
                    (while (string-match "{{\\(.+?\\)}}\\|\\[\\[\\(.+?\\)\\]\\]" text pos)
                      (let ((chord (match-string 1 text)) (cmd (match-string 2 text)))
                        (when (and chord (string-match-p "\\(\\`\\| \\)\\(C-\\|M-\\|S-<\\)" chord)
                                   (not (member chord keys)))
                          (setq stale t))
                        (when (and cmd (string-match-p (concat ide-docs-prefix-re "-[a-z-]+\\'") cmd)
                                   (not (fboundp (intern cmd))) (not (boundp (intern cmd))))
                          (setq stale t)))
                      (setq pos (match-end 0)))
                    (when stale (push (cons (alist-get 'id w) i) bad)))))
    (nreverse bad)))

(defun ide-docs--merge (data facts)
  "Merge FACTS into DATA.  Return (NEW-DATA . REPORT); REPORT is a plist of changes."
  (let* ((kb (ide-docs--merge-keybinds (alist-get 'keybinds data) (plist-get facts :keybinds)))
         (menus (ide-docs--merge-menus (alist-get 'menus data) (plist-get facts :menus)))
         (langs (ide-docs--merge-languages (alist-get 'languages data) (plist-get facts :languages)))
         (tools (ide-docs--merge-tools (alist-get 'tools data) (plist-get facts :tools)))
         (mods (ide-docs--merge-modules (alist-get 'modules data) (plist-get facts :modules)))
         (make (ide-docs--merge-make (alist-get 'make data) (plist-get facts :make)))
         (new (let ((d data))
                (dolist (kv `((keybinds . ,(nth 0 kb)) (menus . ,(nth 0 menus)) (languages . ,(nth 0 langs)) (tools . ,(nth 0 tools))
                              (modules . ,(nth 0 mods)) (make . ,(nth 0 make))))
                  (setq d (ide-docs--put d (car kv) (cdr kv))))
                d))
         (feat (ide-docs--merge-features (alist-get 'features new) new (plist-get facts :commands)))
         (new (ide-docs--put new 'features (nth 0 feat)))
         (stats (ide-docs--merge-stats (alist-get 'stats new) new facts))
         (new (ide-docs--put new 'stats stats))
         (examples-changed (not (equal (alist-get 'examples new) (ide-docs--vec (plist-get facts :examples)))))
         (new (ide-docs--put new 'examples (ide-docs--vec (plist-get facts :examples))))
         (keys (mapcar (lambda (f) (ide-docs--norm-keys (nth 0 f))) (plist-get facts :keybinds))))
    (cons new (list :kb-added (nth 1 kb) :kb-removed (nth 2 kb) :kb-changed (nth 3 kb)
                    :menu-added (nth 1 menus) :menu-changed (nth 2 menus)
                    :lang-added (nth 1 langs) :lang-removed (nth 2 langs)
                    :tools-added (nth 1 tools) :tools-removed (nth 2 tools)
                    :modules-added (nth 1 mods) :modules-removed (nth 2 mods)
                    :make-added (nth 1 make) :make-removed (nth 2 make)
                    :cmds-dropped (nth 1 feat) :uncovered (nth 2 feat)
                    :examples-changed (and examples-changed (list t))
                    :stale-steps (ide-docs--workflow-problems new keys)))))

(defun ide-docs--merge-stats (stats data facts)
  "Recompute the countable statistics; leave measured values (startup time) alone."
  (let ((counts `(("documented keybindings" . ,(length (alist-get 'keybinds data)))
                  ("CLIs and servers" . ,(length (alist-get 'tools data)))
                  ("Emacs Lisp modules" . ,(length (cl-remove-if (lambda (f) (member f '("early-init.el" "init.el")))
                                                                  (plist-get facts :modules))))
                  ("automated tests" . ,(plist-get facts :tests)))))
    (ide-docs--vec
     (mapcar (lambda (s)
               (if-let* ((c (cl-find-if (lambda (c) (string-match-p (car c) (alist-get 'l s))) counts)))
                   (ide-docs--put s 'v (number-to-string (cdr c)))
                 s))
             (ide-docs--list stats)))))

(defun ide-docs--fallback-cards (data uncovered)
  "Put still-uncovered commands in an `Other commands' card so none go undocumented."
  (let* ((covered (ide-docs--data-commands data))
         (left (cl-remove-if (lambda (c) (member (symbol-name c) covered)) uncovered)))
    (if (null left) data
      (let* ((cards (ide-docs--list (alist-get 'features data)))
             (card (cl-find "Other commands" cards :key (lambda (c) (alist-get 'title c)) :test #'equal))
             (names (mapcar #'symbol-name left)))
        (ide-docs--put
          data 'features
          (ide-docs--vec
           (if card
               (mapcar (lambda (c) (if (eq c card) (ide-docs--put c 'cmds (ide-docs--vec (append (alist-get 'cmds c) names))) c)) cards)
             (append cards (list (ide-docs--mk 'cat "Tooling" 'ico "…" 'title "Other commands"
                                               'body "Further commands available through M-x."
                                               'cmds (ide-docs--vec names)))))))))))

;;;; Validation ------------------------------------------------------------------------

(defun ide-docs--validate-subprocess (file callback)
  "Check FILE with tests/docs-check.el in a fresh Emacs; call CALLBACK with (OK . OUTPUT)."
  (let* ((root (file-name-as-directory (expand-file-name user-emacs-directory)))
         (buf (generate-new-buffer " *ide-docs-validate*"))
         (process-environment (append (list (concat "DOCS_FILE=" (expand-file-name file)) "IDE_NO_USER=1")
                                      process-environment))
         (done nil) timer proc)
    (cl-flet ((finish (ok)
                (unless done
                  (setq done t) (when timer (cancel-timer timer))
                  (let ((out (with-current-buffer buf (buffer-string))))
                    (kill-buffer buf)
                    (funcall callback (cons ok (substring out (max 0 (- (length out) 4000)))))))))
      (setq proc (make-process :name "ide-docs-validate" :buffer buf :noquery t :connection-type 'pipe
                               :command (list (expand-file-name invocation-name invocation-directory)
                                              "--init-directory" root "--batch"
                                              "-l" (expand-file-name "init.el" root)
                                              "-l" (expand-file-name "tests/docs-check.el" root))
                               :sentinel (lambda (p _e) (when (memq (process-status p) '(exit signal))
                                                          (finish (zerop (process-exit-status p)))))))
      (setq timer (run-at-time 300 nil (lambda () (when (process-live-p proc) (delete-process proc))
                                         (with-current-buffer buf (insert "\nvalidation timed out\n"))
                                         (finish nil)))))))

;;;; Pipeline ------------------------------------------------------------------------------

(defconst ide-docs--log-buffer "*ide-docs*")
(defun ide-docs--log (fmt &rest args)
  (let ((line (apply #'format fmt args)))
    (if noninteractive (message "%s" line)
      (with-current-buffer (get-buffer-create ide-docs--log-buffer)
        (let ((inhibit-read-only t)) (goto-char (point-max)) (insert line "\n"))))))

(defun ide-docs--update-static (html facts)
  "Update sentences in the static HTML that contain counts."
  (replace-regexp-in-string "declares [0-9]+ CLIs" (format "declares %d CLIs" (length (plist-get facts :tools))) html t t))

(defun ide-docs--install (candidate)
  "Back up the current page, then atomically replace it with CANDIDATE."
  (let* ((dir ide-docs-backup-dir)
         (bak (expand-file-name (format-time-string "index-%Y%m%d-%H%M%S.html") dir))
         (tmp (make-temp-file (expand-file-name "index-" (file-name-directory ide-docs-file)) nil ".tmp")))
    (make-directory dir t)
    (copy-file ide-docs-file bak t)
    (copy-file candidate tmp t)
    (rename-file tmp ide-docs-file t)
    (let ((old (nthcdr ide-docs-keep-backups (sort (directory-files dir t "\\`.*index-.*\\.html\\'") #'string>))))
      (mapc #'delete-file old))
    bak))

(defun ide-docs--summary (report)
  (string-join
   (delq nil (mapcar (lambda (k) (when-let* ((v (plist-get report (car k)))) (format "%s: %d" (cdr k) (length v))))
                     '((:kb-added . "new keys") (:kb-removed . "removed keys") (:kb-changed . "changed keys")
                       (:lang-added . "new languages") (:lang-removed . "removed languages") (:menu-added . "new menu entries") (:menu-changed . "changed menu entries")
                       (:tools-added . "new tools") (:tools-removed . "removed tools")
                       (:modules-added . "new modules") (:modules-removed . "removed modules")
                       (:make-added . "new make targets") (:cmds-dropped . "vanished commands")
                       (:uncovered . "undocumented commands") (:examples-changed . "examples refreshed") (:stale-steps . "stale workflow steps"))))
   ", "))

(cl-defun ide-docs--run (&key dry-run on-done &allow-other-keys)
  "Update the documentation deterministically.
ON-DONE receives (STATUS MESSAGE): ok, unchanged, dry-run, failed."
  (unless (file-exists-p ide-docs-file) (user-error "No documentation at %s" ide-docs-file))
  (when (fboundp 'elpaca-wait) (ignore-errors (elpaca-wait)))
  (let* ((html (ide-slurp ide-docs-file))
         (data (ide-docs--parse-data html))
         (facts (ide-docs--facts))
         (mr (ide-docs--merge data facts))
         (merged (let ((m (car mr))) (ide-docs--fallback-cards m (plist-get (cdr mr) :uncovered))))
         (report (cdr mr))
         (done (or on-done #'ignore)))
    (when (and (not noninteractive) (not dry-run) (get-buffer ide-docs--log-buffer))
      (with-current-buffer ide-docs--log-buffer (let ((inhibit-read-only t)) (erase-buffer))))
    (ide-docs--log "Documentation vs live configuration: %s"
                   (let ((s (ide-docs--summary report))) (if (string-empty-p s) "no differences" s)))
    (if dry-run
        (progn
          (ide-docs--log "Dry run: nothing written.")
          (funcall done 'dry-run "dry run"))
      (let* ((new-html (ide-docs--update-static (ide-docs--render html merged) facts))
             (cand ide-docs-candidate-file))
        (if (and (equal merged data) (equal new-html (ide-docs--update-static html facts)))
            (progn
              (ide-docs--log "Already up to date.")
              (funcall done 'unchanged "already up to date"))
          (make-directory (file-name-directory cand) t)
          (let ((coding-system-for-write 'utf-8)) (write-region new-html nil cand nil 'silent))
          (ide-docs--log "Validating the updated page against a fresh Emacs…")
          (funcall ide-docs-validate-function cand
                   (lambda (res)
                     (if (car res)
                         (let ((bak (ide-docs--install cand)))
                           (ide-docs--log "Updated %s (backup: %s)" ide-docs-file (file-name-nondirectory bak))
                           (funcall done 'ok "updated"))
                       (ide-docs--log "Validation FAILED; the page was not changed.\n%s" (cdr res))
                       (funcall done 'failed (cdr res))))))))))

;;;###autoload
(defun ide-docs-update (&optional dry-run)
  "Update the documentation page deterministically from the live config and docstrings.
With a prefix argument, only show what would change."
  (interactive "P")
  (unless noninteractive (display-buffer (get-buffer-create ide-docs--log-buffer)))
  (ide-docs--run :dry-run dry-run
                 :on-done (lambda (status msg)
                            (message "Documentation update: %s" (if (eq status 'failed) "FAILED (see *ide-docs*)" msg)))))

;;;###autoload
(defalias 'ide-docs-update-facts-only #'ide-docs-update)

(defun ide-docs-update-batch ()
  "Synchronous update for `emacs --batch -f ide-docs-update-batch' (make docs)."
  (let (result)
    (ide-docs--run :on-done (lambda (status msg) (setq result (cons status msg))))
    (while (null result) (accept-process-output nil 0.2))
    (kill-emacs (if (eq (car result) 'failed) 1 0))))

(provide 'docs-update)
;;; docs-update.el ends here
