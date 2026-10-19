;;; lang-devops.el --- TOML, shell, Dockerfile/Compose, Terraform linting -*- lexical-binding: t; -*-

;;; Commentary:
;; The file types a DevOps day is made of, each with the full loop inside Emacs:
;;
;;   TOML        toml-ts-mode + taplo (LSP, schemas, formatting); sort keys / tables, validate
;;   shell       bash-ts-mode + bash-language-server (which runs shellcheck, so there is no
;;               second shellcheck backend while it is connected), shfmt, bash -n, run with
;;               arguments, shellcheck wiki page for the code at point, help for the command
;;   Dockerfile  dockerfile-ts-mode + docker-langserver + hadolint (Flymake); build with ARGs
;;               read from the file, build and run, buildx bake, trivy scans
;;   Compose     `ide-compose-mode' in compose files: config check, up, down, logs, exec
;;   Terraform   tflint and `terraform validate' as Flymake backends, trivy config scan,
;;               Imenu outline; plan/apply stay in `iac-dispatch' (lang-iac.el)
;;
;; Every language has a menu on C-c m (compose files: C-c C-d).  Docker commands are built
;; without needing the daemon, so they can be inspected and tested anywhere.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'transient)
(require 'compile)
(require 'flymake)

(defvar ide-run-command-functions)
(defvar ide-cache-dir)
(defvar ide-eglot-format-on-save)
(defvar apheleia-formatters)
(defvar apheleia-mode-alist)
(defvar ghostel-shell)
(defvar ghostel-buffer-name)
(defvar ghostel-kill-buffer-on-exit)
(defvar imenu-create-index-function)
(declare-function ghostel "ghostel")
(declare-function docker "docker")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-format-buffer "eglot")
(declare-function ide-lang-run "00-lang-lib")
(declare-function ide-lang-exe-or-error "00-lang-lib")
(declare-function ide-lang-root "00-lang-lib")
(declare-function ide-lang-ancestor "00-lang-lib")
(declare-function ide-lang-browse "00-lang-lib")
(declare-function ide-lang-diag "00-lang-lib")
(declare-function ide-lang-tools-dir "00-lang-lib")
(declare-function ide-lang-define-linter "00-lang-lib")
(declare-function ide-lang-register-menu "00-lang-lib")
(declare-function iac-dispatch "lang-iac")
(eval-when-compile (require 'treesit nil t))

(defgroup ide-devops nil "TOML, shell, Docker and Terraform support." :group 'ide-lang)

;;;; Shared helpers ------------------------------------------------------------------------

(defun ide-devops--file ()
  "The current buffer's file, or a `user-error'."
  (or buffer-file-name (user-error "Save the buffer to a file first")))

(defun ide-devops--dir ()
  "Directory of the current file (or `default-directory')."
  (if buffer-file-name (file-name-directory buffer-file-name) default-directory))

(defun ide-devops--quote (arg)
  "ARG quoted for a shell command line, left bare when it is made of safe characters."
  (if (string-match-p "\\`[A-Za-z0-9_@%+=:,./-]+\\'" arg) arg (shell-quote-argument arg)))

(defun ide-devops--cmdline (argv)
  "The shell command line for the list of arguments ARGV."
  (mapconcat #'ide-devops--quote (delq nil argv) " "))

(defun ide-devops--run (name command &optional dir comint)
  "Run COMMAND (string or argv list) in the drawer as *run: NAME* in DIR."
  (ide-lang-run name command (or dir (ide-devops--dir)) comint))

;; BuildKit points at the failing instruction with a bare `Dockerfile:LINE' line.
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(ide-buildkit "^\\([^ \n:]*\\(?:Dockerfile\\|Containerfile\\)[^ \n:]*\\|[^ \n:]+\\.dockerfile\\):\\([0-9]+\\)$" 1 2)))

(defun ide-devops--run-docker (name command &optional dir)
  "Like `ide-devops--run', with BuildKit's `Dockerfile:LINE' errors made clickable."
  (let ((buf (ide-devops--run name command dir)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq-local compilation-error-regexp-alist
                    (cons 'ide-buildkit compilation-error-regexp-alist))))
    buf))

(defun ide-devops--terminal (name command dir)
  "Run the shell COMMAND string in an interactive terminal buffer called NAME in DIR.
Uses ghostel when installed, otherwise a comint compilation in the drawer."
  (let ((default-directory (file-name-as-directory dir)))
    (if (require 'ghostel nil t)
        (let ((ghostel-shell (list "sh" "-c" command))
              (ghostel-buffer-name (format "*ghostel: %s*" name)))
          (let ((buf (ghostel t)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (setq-local ghostel-kill-buffer-on-exit nil)))
            buf))
      (ide-lang-run name command dir t))))

(defun ide-devops--trivy-cache ()
  "Trivy's cache directory inside the sandbox."
  (expand-file-name "trivy/" ide-cache-dir))

(defun ide-devops--trivy (kind target &rest extra)
  "Run `trivy KIND TARGET EXTRA...' in the drawer, with the cache in the sandbox."
  (ide-lang-exe-or-error "trivy")
  (ide-devops--run (format "trivy %s" kind)
                   (append (list "trivy" kind "--cache-dir" (ide-devops--trivy-cache)) extra (list target))))

(defun ide-devops--local-config (names)
  "Nearest file called one of NAMES in the current file's directory or above, or nil."
  (when buffer-file-name
    (cl-loop for n in names
             for dir = (locate-dominating-file buffer-file-name n)
             when dir return (expand-file-name n dir))))

;;;; Linters next to Eglot ----------------------------------------------------------------

;; Eglot sets `flymake-diagnostic-functions' to its own backend alone when it manages a
;; buffer (eglot--setq-saving), which silently drops every `ide-lang-define-linter' backend.
;; Put ours back once the server is up, and rerun the checks.
;; shellcheck, hadolint and tflint only read text or static config, so they are `:safe'
;; (they run in untrusted files).  terraform validate starts the module's provider
;; binaries, so it is not: it pauses until `ide-lang-trust-project'.

(defconst ide-devops--linters
  '((dockerfile-ts-mode ide-lang-flymake-hadolint)
    (bash-ts-mode ide-lang-flymake-shellcheck)
    (terraform-mode ide-lang-flymake-tflint ide-lang-flymake-terraform-validate))
  "Alist (MODE . BACKENDS) of the Flymake backends this file defines.")

(defun ide-devops--restore-linters ()
  "Re-add this file's Flymake backends to a buffer Eglot has taken over."
  (dolist (entry ide-devops--linters)
    (when (derived-mode-p (car entry))
      (dolist (fn (cdr entry)) (add-hook 'flymake-diagnostic-functions fn nil t))
      (when (bound-and-true-p flymake-mode) (flymake-start)))))

(add-hook 'eglot-managed-mode-hook #'ide-devops--restore-linters)

;;;; TOML ----------------------------------------------------------------------------------

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(toml-ts-mode . ("taplo" "lsp" "stdio"))))

(add-hook 'toml-ts-mode-hook #'eglot-ensure)
(add-hook 'toml-ts-mode-hook
          (lambda ()
            (setq-local tab-width 2 indent-tabs-mode nil)
            ;; taplo (Apheleia) owns formatting, honouring .taplo.toml.
            (setq-local ide-eglot-format-on-save nil)))

(with-eval-after-load 'apheleia
  (setf (alist-get 'toml-ts-mode apheleia-mode-alist) 'taplo))

(defun ide-toml--entries (parent)
  "Entries of the direct `pair' children of PARENT (a node), as a list of (START END KEY).
An entry covers the comment lines right above a pair and a comment on its last line."
  (let (entries)
    (dolist (n (treesit-node-children parent))
      (when (equal (treesit-node-type n) "pair")
        (let ((start (treesit-node-start n)) (end (treesit-node-end n))
              (key (treesit-node-text (car (treesit-node-children n)) t))
              prev next)
          (setq prev (treesit-node-prev-sibling n))
          (while (and prev (equal (treesit-node-type prev) "comment")
                      (= (1+ (line-number-at-pos (treesit-node-end prev))) (line-number-at-pos start))
                      (ide-toml--bol-p (treesit-node-start prev)))
            (setq start (treesit-node-start prev)
                  prev (treesit-node-prev-sibling prev)))
          (setq next (treesit-node-next-sibling n))
          (when (and next (equal (treesit-node-type next) "comment")
                     (= (line-number-at-pos (treesit-node-start next)) (line-number-at-pos end)))
            (setq end (treesit-node-end next)))
          (push (list start end key) entries))))
    (nreverse entries)))

(defun ide-toml--bol-p (pos)
  "Non-nil when only whitespace precedes POS on its line."
  (save-excursion (goto-char pos) (skip-chars-backward " \t") (bolp)))

(defun ide-toml--blocks (entries)
  "Split ENTRIES into runs separated by blank lines."
  (let (blocks cur)
    (dolist (e entries)
      (if (and cur (string-match-p "\n[ \t]*\n" (buffer-substring-no-properties (nth 1 (car cur)) (nth 0 e))))
          (progn (push (nreverse cur) blocks) (setq cur (list e)))
        (push e cur)))
    (when cur (push (nreverse cur) blocks))
    (nreverse blocks)))

(defun ide-toml-sort-keys ()
  "Sort the keys of the table at point alphabetically (top level when outside tables).
Runs of keys separated by blank lines are sorted separately; comments stay with their key."
  (interactive)
  (unless (derived-mode-p 'toml-ts-mode) (user-error "Not in a TOML buffer"))
  (let* ((table (ide-lang-ancestor '("table" "table_array_element")))
         (parent (or table (treesit-buffer-root-node)))
         (blocks (ide-toml--blocks (ide-toml--entries parent)))
         (count 0))
    (dolist (block (reverse blocks))
      (when (cdr block)
        (let* ((beg (nth 0 (car block))) (end (nth 1 (car (last block))))
               (texts (mapcar (lambda (e) (cons (nth 2 e) (buffer-substring (nth 0 e) (nth 1 e)))) block))
               (sorted (sort (copy-sequence texts) (lambda (a b) (string< (car a) (car b))))))
          (unless (equal sorted texts)
            (goto-char beg)
            (delete-region beg end)
            (insert (mapconcat #'cdr sorted "\n"))
            (cl-incf count)))))
    (message "Sorted %d block(s) of keys" count)))

(defun ide-toml--table-key (node)
  "Dotted name of a table NODE, without brackets and spaces."
  (let ((header (car (split-string (treesit-node-text node t) "\n"))))
    (replace-regexp-in-string "[][ \t]" "" header)))

(defun ide-toml-sort-tables ()
  "Sort the [tables] of the file by name, keeping the comments above each table.
Array tables keep their relative order; keys before the first table stay where they are."
  (interactive)
  (unless (derived-mode-p 'toml-ts-mode) (user-error "Not in a TOML buffer"))
  (let* ((tables (seq-filter (lambda (n) (member (treesit-node-type n) '("table" "table_array_element")))
                             (treesit-node-children (treesit-buffer-root-node))))
         ;; A table owns the comment lines directly above its header (the grammar nests
         ;; them in the previous table, so look at the text).
         (starts (mapcar (lambda (n)
                           (let ((pos (save-excursion (goto-char (treesit-node-start n)) (line-beginning-position))))
                             (while (and (> pos (point-min))
                                         (save-excursion (goto-char (1- pos)) (beginning-of-line)
                                                         (looking-at "[ \t]*#")))
                               (setq pos (save-excursion (goto-char (1- pos)) (line-beginning-position))))
                             pos))
                         tables)))
    (when (cdr tables)
      (let* ((chunks (cl-loop for (n . rest) on tables
                              for s in starts
                              for e = (if rest (nth (1+ (cl-position n tables)) starts) (point-max))
                              collect (cons (ide-toml--table-key n)
                                            (string-trim-right (buffer-substring s e)))))
             (sorted (sort (copy-sequence chunks) (lambda (a b) (string< (car a) (car b))))))
        (unless (equal chunks sorted)
          (let ((beg (car starts)))
            (delete-region beg (point-max))
            (goto-char beg)
            (insert (mapconcat #'cdr sorted "\n\n") "\n")))))
    (message "%d tables" (length tables))))

(defun ide-toml-validate ()
  "Check this file's syntax and schema (when one applies) with `taplo check'."
  (interactive)
  (ide-lang-exe-or-error "taplo")
  (ide-devops--run "taplo check" (list "taplo" "check" (ide-devops--file))))

(defconst ide-toml-schemas
  '(("mise" . "https://mise.jdx.dev/schema/mise.json")
    ("Cargo" . "https://www.schemastore.org/cargo.json")
    ("pyproject" . "https://www.schemastore.org/pyproject.json")
    ("taplo" . "https://taplo.tamasfe.dev/schemas/taplo.json")
    ("ruff" . "https://www.schemastore.org/ruff.json")
    ("rustfmt" . "https://www.schemastore.org/rustfmt.json"))
  "Schemas `ide-toml-insert-schema' offers: a name and the JSON schema URL.")

(defun ide-toml--default-schema ()
  "The schema name suited to the current file name, or nil."
  (when buffer-file-name
    (let ((n (file-name-nondirectory buffer-file-name)))
      (cond ((string-match-p "\\`\\.?mise\\(?:\\..*\\)?\\.toml\\'" n) "mise")
            ((equal n "Cargo.toml") "Cargo")
            ((equal n "pyproject.toml") "pyproject")
            ((string-match-p "taplo\\.toml\\'" n) "taplo")
            ((member n '("ruff.toml" ".ruff.toml")) "ruff")
            ((member n '("rustfmt.toml" ".rustfmt.toml")) "rustfmt")))))

(defun ide-toml-insert-schema (name)
  "Insert a `#:schema' directive for schema NAME at the top of the file (taplo reads it).
Useful offline-first: taplo then validates and completes against that schema."
  (interactive (list (completing-read "Schema: " (mapcar #'car ide-toml-schemas) nil t
                                      nil nil (ide-toml--default-schema))))
  (let ((line (format "#:schema %s\n" (cdr (assoc name ide-toml-schemas)))))
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "#:schema .*\n") (replace-match line t t) (insert line)))))

;;;###autoload (autoload 'ide-toml-dispatch "lang-devops" nil t)
(transient-define-prefix ide-toml-dispatch ()
  "TOML: format, sort, validate, schemas."
  [["Edit"
    ("f" "format (taplo)" apheleia-format-buffer)
    ("k" "sort keys (table at point)" ide-toml-sort-keys)
    ("t" "sort tables" ide-toml-sort-tables)
    ("s" "insert schema directive" ide-toml-insert-schema)]
   ["Check"
    ("v" "validate (taplo check)" ide-toml-validate)
    ("!" "diagnostics list" flymake-show-buffer-diagnostics)]
   ["Server"
    ("r" "restart taplo" eglot-reconnect)
    ("i" "outline (imenu)" imenu)]])

(ide-lang-register-menu '(toml-ts-mode) #'ide-toml-dispatch)

;;;; Shell ---------------------------------------------------------------------------------

(add-hook 'bash-ts-mode-hook
          (lambda ()
            (setq-local tab-width 2 indent-tabs-mode nil)
            ;; shfmt (Apheleia) owns formatting; bash-language-server would also call shfmt.
            (setq-local ide-eglot-format-on-save nil)))

(defun ide-bash--shfmt-flags ()
  "Style flags for shfmt: `-i 2 -ci', unless an .editorconfig is in charge."
  (unless (ide-devops--local-config '(".editorconfig"))
    (list "-i" "2" "-ci")))

(with-eval-after-load 'apheleia
  (setf (alist-get 'ide-shfmt apheleia-formatters)
        '("shfmt" "-filename" filepath (ide-bash--shfmt-flags)))
  (setf (alist-get 'bash-ts-mode apheleia-mode-alist) 'ide-shfmt))

(defun ide-bash--parse-shellcheck (output buffer)
  "Flymake diagnostics from `shellcheck -f json' OUTPUT."
  (let ((items (json-parse-string output :object-type 'alist :array-type 'list)))
    (mapcar (lambda (c)
              (let ((same (equal (alist-get 'line c) (alist-get 'endLine c))))
                (ide-lang-diag buffer "shellcheck" (alist-get 'line c) (alist-get 'column c)
                               (alist-get 'level c)
                               (format "[SC%s]: %s" (alist-get 'code c) (alist-get 'message c))
                               (and same (alist-get 'endColumn c)))))
            items)))

;; bash-language-server already runs shellcheck and reports it through Eglot with the
;; same codes (SC2086 ...) and quick fixes.  This backend only fills in when no server is
;; connected, so a buffer never shows each finding twice.
(ide-lang-define-linter 'shellcheck :safe t
  :modes '(bash-ts-mode)
  :program (lambda () (unless (and (fboundp 'eglot-current-server) (eglot-current-server))
                        (executable-find "shellcheck")))
  :args '("--format" "json" "-x" "-")
  :parse #'ide-bash--parse-shellcheck)

(defun ide-bash-syntax-check ()
  "Check this script for syntax errors with `bash -n' (nothing is executed)."
  (interactive)
  (ide-devops--run "bash -n" (list "bash" "-n" (ide-devops--file))))

(defun ide-bash-shellcheck-file ()
  "Run shellcheck on the file, following `source'd files, with results in the drawer."
  (interactive)
  (ide-lang-exe-or-error "shellcheck")
  (ide-devops--run "shellcheck" (list "shellcheck" "-x" "-f" "gcc" (ide-devops--file))))

(defvar ide-bash--args-history nil)

(defun ide-bash-run-with-args (args)
  "Run this script with ARGS (a shell string) in the drawer; the buffer accepts input."
  (interactive (list (read-string "Arguments: " nil 'ide-bash--args-history)))
  (ide-devops--run "bash script"
                   (concat "bash " (shell-quote-argument (ide-devops--file)) " " args) nil t))

(defun ide-bash-trace ()
  "Run this script with `bash -x' to see every command as it executes."
  (interactive)
  (ide-devops--run "bash -x" (list "bash" "-x" (ide-devops--file)) nil t))

(defun ide-bash-make-executable ()
  "chmod +x the file."
  (interactive)
  (set-file-modes (ide-devops--file) (logior (file-modes buffer-file-name) #o111))
  (message "%s is executable" (file-name-nondirectory buffer-file-name)))

(defun ide-bash--shellcheck-code-at-point ()
  "The SCxxxx code of the diagnostic at point, or nil."
  (cl-loop for d in (flymake-diagnostics (point))
           when (string-match "\\bSC\\([0-9]\\{4\\}\\)\\b" (flymake-diagnostic-text d))
           return (concat "SC" (match-string 1 (flymake-diagnostic-text d)))))

(defun ide-bash-shellcheck-wiki (code)
  "Open the shellcheck wiki page for CODE (default: the diagnostic at point) in the browser."
  (interactive
   (list (or (ide-bash--shellcheck-code-at-point) (read-string "ShellCheck code (SC2086): "))))
  (unless (string-match-p "\\`SC[0-9]+\\'" code) (user-error "Not a ShellCheck code: %s" code))
  (ide-lang-browse (concat "https://www.shellcheck.net/wiki/" code)))

(defun ide-bash-disable-check ()
  "Insert `# shellcheck disable=SCxxxx' above the line of the diagnostic at point."
  (interactive)
  (let ((code (or (ide-bash--shellcheck-code-at-point) (user-error "No ShellCheck diagnostic at point"))))
    (save-excursion
      (beginning-of-line)
      (insert (format "# shellcheck disable=%s\n" code))
      (indent-according-to-mode))))

(defun ide-bash--command-at-point ()
  "The command name around point (tree-sitter), else the symbol at point."
  (or (when-let* ((cmd (ide-lang-ancestor '("command")))
                  (name (treesit-node-child-by-field-name cmd "name")))
        (treesit-node-text name t))
      (thing-at-point 'symbol t)))

(defun ide-bash--help-text (name)
  "Help text for command NAME: shell builtin help, tldr, or `NAME --help'.  Nil when unknown.
Man pages are handled by the caller."
  (let ((kind (string-trim (shell-command-to-string (format "type -t %s 2>/dev/null" (shell-quote-argument name))))))
    (cond
     ((member kind '("builtin" "keyword"))
      (shell-command-to-string (format "bash -c %s 2>&1" (shell-quote-argument (concat "help " name)))))
     ((executable-find "tldr")
      (shell-command-to-string (format "tldr %s 2>&1" (shell-quote-argument name))))
     ((equal kind "file")
      (shell-command-to-string (format "timeout 5 %s --help 2>&1 </dev/null" (shell-quote-argument name)))))))

(defun ide-bash-help-at-point (name)
  "Show the manual of the command at point: man page, builtin help, tldr or --help output."
  (interactive (list (or (ide-bash--command-at-point) (read-string "Command: "))))
  (if (and (executable-find "man") (zerop (call-process "man" nil nil nil "-w" name))
           (not (member (string-trim (shell-command-to-string
                                      (format "type -t %s" (shell-quote-argument name))))
                        '("builtin" "keyword"))))
      (man name)
    (let ((text (ide-bash--help-text name)))
      (unless (and text (not (string-empty-p (string-trim text))))
        (user-error "No help found for %s" name))
      (with-current-buffer (get-buffer-create (format "*help: %s*" name))
        (let ((inhibit-read-only t))
          (erase-buffer) (insert text) (goto-char (point-min)))
        (special-mode)
        (pop-to-buffer (current-buffer))))))

;;;###autoload (autoload 'ide-bash-dispatch "lang-devops" nil t)
(transient-define-prefix ide-bash-dispatch ()
  "Shell: run, check, format, explore."
  [["Run"
    ("r" "run (no arguments)" ide-run-current)
    ("a" "run with arguments..." ide-bash-run-with-args)
    ("x" "trace (bash -x)" ide-bash-trace)
    ("E" "make executable" ide-bash-make-executable)]
   ["Check"
    ("n" "syntax check (bash -n)" ide-bash-syntax-check)
    ("s" "shellcheck file (follow sources)" ide-bash-shellcheck-file)
    ("!" "diagnostics list" flymake-show-buffer-diagnostics)
    ("w" "shellcheck wiki for code" ide-bash-shellcheck-wiki)
    ("d" "disable this check" ide-bash-disable-check)]
   ["Edit / explore"
    ("f" "format (shfmt)" apheleia-format-buffer)
    ("h" "help for command at point" ide-bash-help-at-point)
    ("c" "code actions" eglot-code-actions)
    ("o" "outline (imenu)" imenu)]])

(ide-lang-register-menu '(bash-ts-mode sh-mode) #'ide-bash-dispatch)

;;;; Dockerfile ----------------------------------------------------------------------------

(defun ide-dockerfile--config ()
  "The nearest .hadolint.yaml (or .yml) above the file, for hadolint's --config."
  (ide-devops--local-config '(".hadolint.yaml" ".hadolint.yml")))

(defun ide-dockerfile--parse-hadolint (output buffer)
  "Flymake diagnostics from `hadolint --format json' OUTPUT."
  (mapcar (lambda (d)
            (ide-lang-diag buffer "hadolint" (alist-get 'line d) nil
                           (pcase (alist-get 'level d) ("error" :error) ("warning" :warning) (_ :note))
                           (format "%s: %s" (alist-get 'code d) (alist-get 'message d))))
          (json-parse-string output :object-type 'alist :array-type 'list)))

;; hadolint reads the buffer from stdin.  Without --config it only finds .hadolint.yaml in
;; the project root; passing the nearest one honours nested directories too.
(ide-lang-define-linter 'hadolint :safe t
  :modes '(dockerfile-ts-mode)
  :program "hadolint"
  :args (lambda (_file)
          (append (list "--format" "json" "--no-color")
                  (when-let* ((c (ide-dockerfile--config))) (list "--config" c))
                  (list "-")))
  :parse #'ide-dockerfile--parse-hadolint)

(defun ide-dockerfile--args ()
  "The ARG instructions of the buffer as a list of (NAME . DEFAULT-or-nil), via tree-sitter."
  (let (res)
    (dolist (n (treesit-node-children (treesit-buffer-root-node)))
      (when (equal (treesit-node-type n) "arg_instruction")
        (let ((name (treesit-node-child-by-field-name n "name"))
              (def (treesit-node-child-by-field-name n "default")))
          (when name
            (push (cons (treesit-node-text name t)
                        (and def (string-trim (treesit-node-text def t) "[\"']" "[\"']")))
                  res)))))
    (nreverse res)))

(defun ide-dockerfile--stages ()
  "Names of the build stages (FROM ... AS name) of the buffer."
  (let (res)
    (dolist (n (treesit-node-children (treesit-buffer-root-node)))
      (when (equal (treesit-node-type n) "from_instruction")
        (when-let* ((a (treesit-node-child-by-field-name n "as")))
          (push (treesit-node-text a t) res))))
    (nreverse res)))

(defun ide-dockerfile--default-tag (dir)
  "A valid image tag derived from the name of directory DIR."
  (let ((base (downcase (file-name-nondirectory (directory-file-name (expand-file-name dir))))))
    (concat (replace-regexp-in-string "[^a-z0-9._-]+" "-" (if (string-empty-p base) "image" base)) ":dev")))

(defun ide-dockerfile--build-argv (file tag &optional build-args target)
  "The `docker build' argv for Dockerfile FILE producing TAG.
BUILD-ARGS is an alist (NAME . VALUE); TARGET a stage name.  The context is FILE's directory."
  (append (list "docker" "build" "-f" file "-t" tag)
          (when target (list "--target" target))
          (cl-loop for (k . v) in build-args append (list "--build-arg" (format "%s=%s" k v)))
          (list (directory-file-name (file-name-directory file)))))

(defun ide-dockerfile--read-build-args ()
  "Ask for a value for each ARG of the buffer; blank keeps the default.  Returns an alist."
  (cl-loop for (name . def) in (ide-dockerfile--args)
           for v = (read-string (format "ARG %s%s: " name (if def (format " [%s]" def) "")))
           unless (string-empty-p v) collect (cons name v)))

(defun ide-dockerfile--build (&optional ask-args stage run)
  "Build this Dockerfile (asking for ARG values with ASK-ARGS, targeting STAGE).
With RUN, then run the image interactively in a terminal."
  (let* ((file (ide-devops--file))
         (tag (read-string "Image tag: " (ide-dockerfile--default-tag (file-name-directory file))))
         (argv (ide-dockerfile--build-argv file tag (when ask-args (ide-dockerfile--read-build-args)) stage))
         (cmd (ide-devops--cmdline argv)))
    (if run
        (ide-devops--terminal "docker run" (format "%s && docker run --rm -it %s" cmd (ide-devops--quote tag))
                              (ide-devops--dir))
      (ide-devops--run-docker "docker build" cmd))))

(defun ide-dockerfile-build () "docker build this Dockerfile." (interactive) (ide-dockerfile--build))
(defun ide-dockerfile-build-with-args ()
  "docker build, asking for a value of each ARG declared in the file."
  (interactive) (ide-dockerfile--build t))
(defun ide-dockerfile-build-stage ()
  "docker build --target a stage chosen from the FROM ... AS lines."
  (interactive)
  (let ((stages (or (ide-dockerfile--stages) (user-error "No named build stages"))))
    (ide-dockerfile--build nil (completing-read "Stage: " stages nil t))))
(defun ide-dockerfile-build-and-run ()
  "Build, then run the image interactively in a terminal."
  (interactive) (ide-dockerfile--build nil nil t))

(defun ide-dockerfile--images ()
  "Local image references (needs the daemon); nil when docker cannot answer."
  (ignore-errors (process-lines "docker" "images" "--format" "{{.Repository}}:{{.Tag}}")))

(defun ide-dockerfile--read-image (prompt)
  (let ((imgs (ide-dockerfile--images)))
    (if imgs (completing-read prompt imgs) (read-string prompt))))

(defun ide-dockerfile-run-image (image)
  "Run IMAGE interactively (docker run --rm -it) in a terminal."
  (interactive (list (ide-dockerfile--read-image "Run image: ")))
  (ide-devops--terminal "docker run" (format "docker run --rm -it %s" (ide-devops--quote image))
                        (ide-devops--dir)))

(defconst ide-dockerfile--bake-files
  '("docker-bake.hcl" "docker-bake.json" "docker-bake.override.hcl" "docker-bake.override.json"
    "compose.yaml" "compose.yml" "docker-compose.yaml" "docker-compose.yml")
  "Files `docker buildx bake' reads, in its own order of preference (compose is a fallback).")

(defun ide-dockerfile--bake-file ()
  "The nearest bake definition above the buffer's directory, or nil."
  (ide-devops--local-config ide-dockerfile--bake-files))

(defun ide-dockerfile--bake-targets (file)
  "Target names declared in bake FILE (HCL `target \"x\"' blocks, JSON keys, or compose services)."
  (with-temp-buffer
    (insert-file-contents file)
    (cond
     ((string-suffix-p ".json" file)
      (mapcar #'symbol-name (map-keys (alist-get 'target (json-parse-buffer :object-type 'alist) nil))))
     ((string-match-p "\\.ya?ml\\'" file)
      (goto-char (point-min))
      (when (re-search-forward "^[ \t]*services:[ \t]*$" nil t)
        (let ((end (save-excursion
                     (if (re-search-forward "^[A-Za-z0-9_-]+:[ \t]*$" nil t)
                         (match-beginning 0)
                       (point-max))))
              res)
          (while (re-search-forward "^  \\([A-Za-z0-9_.-]+\\):[ \t]*$" end t)
            (push (match-string 1) res))
          (nreverse res))))
     (t (let (res)
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*target[ \t]+\"\\([^\"]+\\)\"" nil t) (push (match-string 1) res))
          (nreverse res))))))

(defun ide-dockerfile-bake ()
  "docker buildx bake a target of the nearest docker-bake.hcl (or the compose build section)."
  (interactive)
  (let* ((file (or (ide-dockerfile--bake-file) (user-error "No docker-bake.hcl or compose file found")))
         (targets (ide-dockerfile--bake-targets file))
         (target (and targets (completing-read "Bake target (empty = default): " (cons "" targets) nil t))))
    (ide-devops--run-docker "docker bake"
                     (append (list "docker" "buildx" "bake" "-f" file)
                             (and target (not (string-empty-p target)) (list target)))
                     (file-name-directory file))))

(defun ide-dockerfile-bake-print ()
  "Show the resolved bake configuration (docker buildx bake --print) without building."
  (interactive)
  (let ((file (or (ide-dockerfile--bake-file) (user-error "No docker-bake.hcl or compose file found"))))
    (ide-devops--run "docker bake print" (list "docker" "buildx" "bake" "-f" file "--print")
                     (file-name-directory file))))

(defun ide-dockerfile-trivy-config ()
  "Scan this directory's Dockerfile and IaC for misconfigurations (trivy config)."
  (interactive)
  (ide-devops--trivy "config" (directory-file-name (ide-devops--dir))))

(defun ide-dockerfile-trivy-image (image)
  "Scan IMAGE for vulnerabilities and secrets (trivy image; needs the vulnerability database)."
  (interactive (list (ide-dockerfile--read-image "Scan image: ")))
  (ide-devops--trivy "image" image))

(defun ide-dockerfile-hadolint-wiki ()
  "Open the wiki page of the hadolint rule at point in the in-Emacs browser."
  (interactive)
  (let ((code (cl-loop for d in (or (flymake-diagnostics (point))
                                    (flymake-diagnostics (line-beginning-position) (line-end-position)))
                       when (string-match "\\b\\(DL[0-9]+\\|SC[0-9]+\\)\\b" (flymake-diagnostic-text d))
                       return (match-string 1 (flymake-diagnostic-text d)))))
    (ide-lang-browse (cond ((null code) "https://github.com/hadolint/hadolint/wiki")
                           ((string-prefix-p "SC" code) (concat "https://www.shellcheck.net/wiki/" code))
                           (t (concat "https://github.com/hadolint/hadolint/wiki/" code))))))

(defun ide-dockerfile--run-command ()
  "S-<f10> in a Dockerfile builds it with the default tag."
  (when (derived-mode-p 'dockerfile-ts-mode 'dockerfile-mode)
    (ide-devops--cmdline (ide-dockerfile--build-argv (ide-devops--file)
                                                    (ide-dockerfile--default-tag (ide-devops--dir))))))

;;;###autoload (autoload 'ide-dockerfile-dispatch "lang-devops" nil t)
(transient-define-prefix ide-dockerfile-dispatch ()
  "Dockerfile: build, run, scan, compose."
  [["Build"
    ("b" "build" ide-dockerfile-build)
    ("B" "build with ARGs..." ide-dockerfile-build-with-args)
    ("s" "build a stage..." ide-dockerfile-build-stage)
    ("k" "buildx bake..." ide-dockerfile-bake)
    ("K" "bake --print" ide-dockerfile-bake-print)]
   ["Run"
    ("r" "build and run" ide-dockerfile-build-and-run)
    ("i" "run an image..." ide-dockerfile-run-image)
    ("d" "docker (containers, images)" docker)]
   ["Scan / lint"
    ("t" "trivy config scan" ide-dockerfile-trivy-config)
    ("T" "trivy image scan..." ide-dockerfile-trivy-image)
    ("!" "diagnostics list" flymake-show-buffer-diagnostics)
    ("w" "hadolint rule docs" ide-dockerfile-hadolint-wiki)]
   ["Compose / server"
    ("c" "compose menu" ide-compose-dispatch)
    ("o" "outline (imenu)" imenu)
    ("R" "restart language server" eglot-reconnect)]])

(ide-lang-register-menu '(dockerfile-ts-mode dockerfile-mode) #'ide-dockerfile-dispatch)
(add-hook 'ide-run-command-functions #'ide-dockerfile--run-command)

;;;; Compose -------------------------------------------------------------------------------

(defconst ide-compose-file-regexp "\\(?:\\`\\|/\\)\\(?:docker-\\)?compose\\(?:\\.[^/]+\\)?\\.ya?ml\\'"
  "File names that are Docker Compose files.")

(defun ide-compose--file ()
  "The compose file this buffer is, or the nearest one above it."
  (if (and buffer-file-name (string-match-p ide-compose-file-regexp buffer-file-name))
      buffer-file-name
    (or (ide-devops--local-config '("compose.yaml" "compose.yml" "docker-compose.yaml" "docker-compose.yml"))
        (user-error "No compose file found"))))

(defun ide-compose--argv (file &rest args)
  "Argv of `docker compose -f FILE ARGS...'."
  (append (list "docker" "compose" "-f" file) args))

(defun ide-compose--run (name &rest args)
  "Run a compose command with ARGS in the drawer, from the compose file's directory."
  (let ((file (ide-compose--file)))
    (ide-devops--run (format "compose %s" name) (apply #'ide-compose--argv file args)
                     (file-name-directory file))))

(defun ide-compose--services ()
  "Service names of the compose file (needs no daemon)."
  (let ((file (ide-compose--file)))
    (ignore-errors
      (let ((default-directory (file-name-directory file)))
        (process-lines "docker" "compose" "-f" file "config" "--services")))))

(defmacro ide-compose--def (name &rest args)
  `(defun ,name () (interactive) (ide-compose--run ,@args)))

(ide-compose--def ide-compose-config "config" "config")
(ide-compose--def ide-compose-validate "validate" "config" "-q")
(ide-compose--def ide-compose-up "up" "up" "--build" "-d")
(ide-compose--def ide-compose-up-foreground "up" "up" "--build")
(ide-compose--def ide-compose-down "down" "down")
(defun ide-compose-down-volumes ()
  "docker compose down -v: also remove the stack's named volumes."
  (interactive)
  (when (yes-or-no-p "Remove the stack's volumes too? ") (ide-compose--run "down" "down" "-v")))
(ide-compose--def ide-compose-ps "ps" "ps")
(ide-compose--def ide-compose-pull "pull" "pull")
(ide-compose--def ide-compose-build "build" "build")
(ide-compose--def ide-compose-restart "restart" "restart")

(defun ide-compose-logs (service)
  "Follow the logs of SERVICE (empty: all services)."
  (interactive (list (completing-read "Service (empty = all): " (cons "" (ide-compose--services)))))
  (apply #'ide-compose--run "logs" "logs" "-f" "--tail" "200" (unless (string-empty-p service) (list service))))

(defun ide-compose-exec (service)
  "Open a shell in SERVICE's running container, in a terminal."
  (interactive (list (completing-read "Service: " (ide-compose--services) nil t)))
  (let ((file (ide-compose--file)))
    (ide-devops--terminal (format "compose exec %s" service)
                          (ide-devops--cmdline
                           (ide-compose--argv file "exec" service "sh" "-c"
                                              "command -v bash >/dev/null && exec bash || exec sh"))
                          (file-name-directory file))))

;;;###autoload (autoload 'ide-compose-dispatch "lang-devops" nil t)
(transient-define-prefix ide-compose-dispatch ()
  "Docker Compose for the compose file of this buffer."
  [["Lifecycle"
    ("u" "up -d (build)" ide-compose-up)
    ("U" "up (attached)" ide-compose-up-foreground)
    ("d" "down" ide-compose-down)
    ("V" "down + volumes" ide-compose-down-volumes)
    ("R" "restart" ide-compose-restart)
    ("b" "build" ide-compose-build)
    ("p" "pull" ide-compose-pull)]
   ["Inspect"
    ("v" "validate" ide-compose-validate)
    ("c" "config (resolved)" ide-compose-config)
    ("s" "ps" ide-compose-ps)
    ("l" "logs..." ide-compose-logs)
    ("e" "exec shell..." ide-compose-exec)]
   ["Docker"
    ("D" "docker (containers, images)" docker)]])

(defvar ide-compose-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-d") #'ide-compose-dispatch)
    m))

(define-minor-mode ide-compose-mode
  "Docker Compose commands for a compose file (menu on C-c C-d)."
  :lighter " Compose" :keymap ide-compose-mode-map)

(defun ide-compose--maybe-enable ()
  "Turn on `ide-compose-mode' in compose files."
  (when (and buffer-file-name (string-match-p ide-compose-file-regexp buffer-file-name))
    (ide-compose-mode 1)))

(add-hook 'yaml-ts-mode-hook #'ide-compose--maybe-enable)
(add-hook 'yaml-mode-hook #'ide-compose--maybe-enable)

(defun ide-compose--run-command ()
  "S-<f10> in a compose file brings the stack up."
  (when (bound-and-true-p ide-compose-mode)
    (ide-devops--cmdline (ide-compose--argv (ide-compose--file) "up" "--build"))))

(add-hook 'ide-run-command-functions #'ide-compose--run-command)

;;;; Terraform linting ---------------------------------------------------------------------

(defun ide-terraform--tflint-plugin-dir ()
  "tflint's plugin directory inside the sandbox (its default is under the home directory)."
  (ide-lang-tools-dir "tflint" "plugins"))

(defun ide-terraform--tflint-argv (file)
  "Command line linting the module that contains FILE (saved) and printing JSON."
  (let ((dir (directory-file-name (file-name-directory (expand-file-name file))))
        (conf (ide-devops--local-config '(".tflint.hcl"))))
    (append (list (concat "TFLINT_PLUGIN_DIR=" (ide-terraform--tflint-plugin-dir))
                  "tflint" (concat "--chdir=" dir) "--format" "json")
            (when conf (list "--config" conf)))))

(defun ide-terraform--parse-tflint (output buffer)
  "Flymake diagnostics for BUFFER's file from tflint's JSON OUTPUT.
Findings in other files of the module are skipped; plugin and config errors become a
note on the first line."
  (let* ((json (condition-case nil (json-parse-string output :object-type 'alist :array-type 'list)
                 (error nil)))
         (this (file-name-nondirectory (buffer-file-name buffer)))
         diags)
    (if (null json)
        (when (string-match-p "tflint\\|plugin\\|[Ee]rror" output)
          (push (ide-lang-diag buffer "tflint" 1 nil :note (car (split-string (string-trim output) "\n"))) diags))
      (dolist (i (alist-get 'issues json))
        (let* ((r (alist-get 'range i)) (start (alist-get 'start r)) (end (alist-get 'end r)))
          (when (equal (file-name-nondirectory (alist-get 'filename r)) this)
            (push (ide-lang-diag buffer "tflint" (alist-get 'line start) (alist-get 'column start)
                                 (alist-get 'severity (alist-get 'rule i))
                                 (format "%s (%s)" (alist-get 'message i) (alist-get 'name (alist-get 'rule i)))
                                 (and (equal (alist-get 'line start) (alist-get 'line end))
                                      (alist-get 'column end)))
                  diags))))
      (dolist (e (alist-get 'errors json))
        (push (ide-lang-diag buffer "tflint" 1 nil :note (alist-get 'message e)) diags)))
    (nreverse diags)))

;; Saved-only: tflint reads the module from disk.  `env' carries TFLINT_PLUGIN_DIR so plugins
;; (tflint --init) live in the sandbox.
(ide-lang-define-linter 'tflint :safe t
  :modes '(terraform-mode)
  :program (lambda () (and (executable-find "tflint") (executable-find "env")))
  :args #'ide-terraform--tflint-argv
  :parse #'ide-terraform--parse-tflint
  :stdin nil :saved-only t)

(defun ide-terraform--parse-validate (output buffer)
  "Flymake diagnostics for BUFFER's file from `terraform validate -json' OUTPUT."
  (let* ((json (json-parse-string output :object-type 'alist :array-type 'list))
         (this (file-name-nondirectory (buffer-file-name buffer)))
         diags)
    (dolist (d (alist-get 'diagnostics json))
      (let* ((r (alist-get 'range d)) (start (alist-get 'start r)) (end (alist-get 'end r))
             (text (format "%s%s" (alist-get 'summary d)
                           (if-let* ((x (alist-get 'detail d))) (concat ": " (car (split-string x "\n"))) ""))))
        (cond
         ((null r) (push (ide-lang-diag buffer "terraform" 1 nil :note text) diags))
         ((equal (file-name-nondirectory (alist-get 'filename r)) this)
          (push (ide-lang-diag buffer "terraform" (alist-get 'line start) (alist-get 'column start)
                               (alist-get 'severity d) text
                               (and (equal (alist-get 'line start) (alist-get 'line end)) (alist-get 'column end)))
                diags)))))
    (nreverse diags)))

(defcustom ide-terraform-validate-backend nil
  "Run `terraform validate -json' as a Flymake backend on save.
terraform-ls already validates a module once it is initialised, so this is off by
default and `ide-terraform-validate' (the menu) covers the on-demand case."
  :type 'boolean)

(ide-lang-define-linter 'terraform-validate
  :modes '(terraform-mode)
  :program (lambda () (and ide-terraform-validate-backend (executable-find "terraform")))
  :args (lambda (file)
          (list (concat "-chdir=" (directory-file-name (file-name-directory (expand-file-name file))))
                "validate" "-json" "-no-color"))
  :parse #'ide-terraform--parse-validate
  :stdin nil :saved-only t)

(defun ide-terraform-tflint ()
  "Run tflint over the module of this file and show everything it finds in the drawer."
  (interactive)
  (ide-lang-exe-or-error "tflint")
  (ide-devops--run "tflint" (append (list "env" (concat "TFLINT_PLUGIN_DIR=" (ide-terraform--tflint-plugin-dir)))
                                    (list "tflint" "--format" "compact"
                                          (concat "--chdir=" (directory-file-name (ide-devops--dir)))))))

(defun ide-terraform-tflint-init ()
  "Install the plugins named in .tflint.hcl (tflint --init; needs network) into the sandbox."
  (interactive)
  (ide-lang-exe-or-error "tflint")
  (ide-devops--run "tflint init" (list "env" (concat "TFLINT_PLUGIN_DIR=" (ide-terraform--tflint-plugin-dir))
                                       "tflint" "--init")))

(defun ide-terraform-validate ()
  "terraform validate for this module (run `terraform init' first for providers)."
  (interactive)
  (ide-lang-exe-or-error "terraform")
  (ide-devops--run "terraform validate" (list "terraform" "validate" "-no-color")))

(defun ide-terraform-fmt-check ()
  "Show what `terraform fmt' would change in this module (diff), without changing anything."
  (interactive)
  (ide-lang-exe-or-error "terraform")
  (ide-devops--run "terraform fmt" (list "terraform" "fmt" "-check" "-diff" "-no-color")))

(defun ide-terraform-trivy-config ()
  "Scan this module for security misconfigurations (trivy config)."
  (interactive)
  (ide-devops--trivy "config" (directory-file-name (ide-devops--dir))))

(defun ide-terraform-console ()
  "Open `terraform console' for this module in the drawer."
  (interactive)
  (ide-lang-exe-or-error "terraform")
  (ide-devops--run "terraform console" (list "terraform" "console") nil t))

(defun ide-terraform-providers ()
  "Show the provider requirements tree of this module."
  (interactive)
  (ide-lang-exe-or-error "terraform")
  (ide-devops--run "terraform providers" (list "terraform" "providers")))

(defun ide-terraform-docs-at-point ()
  "Open the registry documentation of the resource or data source at point in the browser."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*\\(resource\\|data\\)[ \t]+\"\\([a-z0-9]+\\)_\\([a-z0-9_]+\\)\"")
        (ide-lang-browse (format "https://registry.terraform.io/providers/hashicorp/%s/latest/docs/%s/%s"
                                 (match-string 2) (if (equal (match-string 1) "data") "data-sources" "resources")
                                 (match-string 3)))
      (user-error "Put point on a resource or data line"))))

;;;;; Outline

(defconst ide-terraform--block-regexp
  "^[ \t]*\\(resource\\|data\\|variable\\|output\\|module\\|provider\\|locals\\|terraform\\|moved\\|import\\|check\\)\\(?:[ \t]+\"\\([^\"]+\\)\"\\)?\\(?:[ \t]+\"\\([^\"]+\\)\"\\)?[ \t]*{"
  "A top-level Terraform block header: kind, then up to two quoted labels.")

(defun ide-terraform-imenu-index ()
  "Imenu index of the blocks of this file, grouped by kind (Resources, Variables, ...)."
  (let (groups)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward ide-terraform--block-regexp nil t)
        (let* ((kind (match-string 1))
               (name (string-join (delq nil (list (match-string 2) (match-string 3))) "."))
               (title (pcase kind
                        ("resource" "Resources") ("data" "Data sources") ("variable" "Variables")
                        ("output" "Outputs") ("module" "Modules") ("provider" "Providers")
                        (_ (capitalize kind))))
               (cell (cons (if (string-empty-p name) kind name) (copy-marker (match-beginning 0)))))
          (if-let* ((g (assoc title groups))) (setcdr g (append (cdr g) (list cell)))
            (push (list title cell) groups)))))
    (nreverse groups)))

(add-hook 'terraform-mode-hook
          (lambda ()
            (setq-local imenu-create-index-function #'ide-terraform-imenu-index)
            ;; terraform-format-on-save-mode (lang-iac) owns formatting, not terraform-ls.
            (setq-local ide-eglot-format-on-save nil)))

;;;###autoload (autoload 'ide-terraform-dispatch "lang-devops" nil t)
(transient-define-prefix ide-terraform-dispatch ()
  "Terraform: lint, validate, scan, explore.  Plan and apply live in `iac-dispatch'."
  [["Lint / validate"
    ("l" "tflint (module)" ide-terraform-tflint)
    ("I" "tflint --init (plugins)" ide-terraform-tflint-init)
    ("v" "validate" ide-terraform-validate)
    ("f" "fmt diff" ide-terraform-fmt-check)
    ("!" "diagnostics list" flymake-show-buffer-diagnostics)]
   ["Scan"
    ("t" "trivy config scan" ide-terraform-trivy-config)]
   ["Explore"
    ("d" "registry docs at point" ide-terraform-docs-at-point)
    ("p" "providers" ide-terraform-providers)
    ("c" "console" ide-terraform-console)
    ("o" "outline (imenu)" imenu)]
   ["Run"
    ("x" "init / plan / apply ..." iac-dispatch)
    ("R" "restart terraform-ls" eglot-reconnect)]])

(ide-lang-register-menu '(terraform-mode) #'ide-terraform-dispatch)

(provide 'lang-devops)
;;; lang-devops.el ends here
