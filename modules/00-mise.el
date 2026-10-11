;;; 00-mise.el --- Portable tool installation via mise -*- lexical-binding: t; -*-

;;; Commentary:
;; `ide-mise-setup' bootstraps mise into the sandbox if it is not already on
;; PATH, installs every tool in mise.toml, and puts the tool bin directories
;; on `exec-path' and PATH.  All mise state lives in var/mise and .cache/mise.
;; The resulting PATH is cached so startup never has to spawn mise.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)

(declare-function ide-run-with-timeout "00-core")

(defgroup ide-mise nil "Install IDE tools with mise." :group 'tools)

(defcustom ide-mise-version nil
  "mise release tag to download (for example \"v2026.10.7\"); nil means latest.
Pin this together with `ide-mise-sha256' for a supply-chain-safe bootstrap."
  :type '(choice (const nil) string))

(defcustom ide-mise-sha256 nil
  "Expected sha256 of the mise binary for this platform.
The checksum normally comes from the same GitHub release as the binary, which
detects corruption but not a compromised release.  Set this (with
`ide-mise-version') to verify against a value you obtained independently."
  :type '(choice (const nil) string))

(defcustom ide-azure-config-dir (expand-file-name "azure/" (expand-file-name "var/" user-emacs-directory))
  "Directory for Azure CLI state (logins, extensions), keeping it inside the sandbox.
Set to nil to use the CLI's default (~/.azure) and share logins with your shell.
An AZURE_CONFIG_DIR already set in the environment always wins."
  :type '(choice (const nil) directory))

(defcustom ide-contain-tool-state t
  "When non-nil, keep the toolchains' own homes and caches inside this directory.
Cargo, rustup, the Go module and build caches, uv, npm and pip would otherwise
write to ~/.cargo, ~/.rustup, ~/go, ~/.cache/... and ~/.npm.  A variable that is
already set in the environment always wins."
  :type 'boolean)

(defun ide-mise-contained-env ()
  "Alist of (VARIABLE . DIRECTORY) that keeps tool state in the sandbox."
  (let ((tools (expand-file-name "var/tools/" user-emacs-directory))
        (cache (expand-file-name ".cache/" user-emacs-directory)))
    `(("CARGO_HOME" . ,(expand-file-name "cargo/" tools))
      ("RUSTUP_HOME" . ,(expand-file-name "rustup/" tools))
      ("GOMODCACHE" . ,(expand-file-name "go-mod/" tools))
      ("GOCACHE" . ,(expand-file-name "go-build/" cache))
      ("UV_CACHE_DIR" . ,(expand-file-name "uv/" cache))
      ("npm_config_cache" . ,(expand-file-name "npm/" cache))
      ("PIP_CACHE_DIR" . ,(expand-file-name "pip/" cache)))))

(defvar ide-mise-dir (expand-file-name "mise/" (expand-file-name "var/" user-emacs-directory))
  "Sandboxed mise directory.")
(defvar ide-mise-manifest (expand-file-name "mise.toml" user-emacs-directory)
  "Tool manifest.")
(defvar ide-mise-path-cache (expand-file-name "path.eld" ide-mise-dir)
  "Cache of the tool PATH entries, read at startup.")

(defconst ide-mise-required-executables
  '(("python" "Python runtime") ("node" "Node.js runtime") ("go" "Go toolchain")
    ("uv" "Python package manager") ("rg" "ripgrep") ("fd" "fd") ("jq" "jq")
    ("pandoc" "Confluence conversion") ("direnv" "envrc")
    ("az" "Azure CLI") ("kubectl" "Kubernetes") ("helm" "Helm")
    ("terraform" "Terraform") ("packer" "Packer") ("ansible" "Ansible")
    ("ruff" "Python lint/format") ("pyright-langserver" "Python LSP")
    ("yaml-language-server" "YAML LSP") ("bash-language-server" "Bash LSP")
    ("docker-langserver" "Dockerfile LSP") ("ansible-language-server" "Ansible LSP")
    ("terraform-ls" "Terraform LSP") ("gopls" "Go LSP") ("dlv" "Go debugger")
    ("cargo" "Rust toolchain") ("rust-analyzer" "Rust LSP") ("cargo-nextest" "Rust test runner")
    ("taplo" "TOML LSP / formatter")
    ("tsc" "TypeScript (7: native LSP)") ("tsx" "TypeScript runner") ("prettier" "Web formatter")
    ("eslint" "JavaScript linter") ("vscode-html-language-server" "HTML LSP")
    ("vscode-css-language-server" "CSS LSP")
    ("sqls" "SQL LSP") ("sqlfluff" "SQL linter") ("sql-formatter" "SQL formatter") ("sqlcmd" "SQL Server client")
    ("lua-language-server" "Lua LSP") ("stylua" "Lua formatter")
    ("pwsh" "PowerShell")
    ("helm_ls" "Helm LSP") ("kubeconform" "Kubernetes schema check") ("kube-linter" "Kubernetes lint")
    ("kind" "local Kubernetes (testing)") ("trivy" "misconfiguration scanner")
    ("yamllint" "YAML lint") ("actionlint" "GitHub Actions lint")
    ("shellcheck" "shell lint") ("shfmt" "shell formatter") ("hadolint" "Dockerfile lint")
    ("tflint" "Terraform lint") ("marksman" "Markdown LSP") ("markdownlint-cli2" "Markdown lint"))
  "Executables the IDE expects, with a description each.")

(defconst ide-mise-process-buffer "*ide-mise*" "Drawer buffer for mise output.")

(defun ide-mise--run-dir ()
  "Directory mise runs in: user/ so that user/mise.toml adds the user's own tools."
  (let ((dir (expand-file-name "user/" user-emacs-directory)))
    (make-directory dir t)
    dir))

;;;; Environment --------------------------------------------------------------

(defun ide-mise--env ()
  "Environment entries that keep mise entirely inside the sandbox."
  (let ((root (expand-file-name ".cache/mise/" user-emacs-directory)))
    (append
     (list (concat "MISE_DATA_DIR=" (expand-file-name "data/" ide-mise-dir))
          (concat "MISE_STATE_DIR=" (expand-file-name "state/" ide-mise-dir))
          (concat "MISE_CONFIG_DIR=" (expand-file-name "config/" ide-mise-dir))
          (concat "MISE_CACHE_DIR=" root)
          (concat "MISE_GLOBAL_CONFIG_FILE=" ide-mise-manifest)
          ;; user/mise.toml is merged with the core manifest when mise runs from user/.
          (concat "MISE_TRUSTED_CONFIG_PATHS=" (directory-file-name (expand-file-name user-emacs-directory)))
          ;; Stop mise looking for mise.toml / .tool-versions in parent directories (~, /),
          ;; so tools from the user's unrelated home configuration never leak in.
          (concat "MISE_CEILING_PATHS="
                  (directory-file-name (file-name-directory (directory-file-name (expand-file-name user-emacs-directory)))))
          "MISE_YES=1" "MISE_NO_ANALYTICS=1")
     (when ide-contain-tool-state
       (cl-loop for (var . dir) in (ide-mise-contained-env)
                unless (getenv var) collect (concat var "=" dir))))))

(defun ide-mise-executable ()
  "Return the mise executable (sandbox copy first, then PATH), or nil."
  (let ((local (expand-file-name "bin/mise" ide-mise-dir)))
    (cond ((file-executable-p local) local)
          ((executable-find "mise")))))

;;;; PATH handling --------------------------------------------------------------

(defun ide-mise--merge-path (dirs)
  "Prepend DIRS to `exec-path' and PATH in `process-environment'; return DIRS."
  (let ((dirs (cl-remove-if-not #'file-directory-p dirs)))
    (dolist (d (reverse dirs))
      (setq exec-path (cons d (delete d exec-path))))
    (setenv "PATH" (mapconcat #'identity
                              (delete-dups (append dirs (split-string (or (getenv "PATH") "") path-separator t)))
                              path-separator))
    dirs))

(defun ide-mise-load-cached-path ()
  "Apply the cached tool PATH, if any.  Cheap enough to run at startup."
  (when (file-readable-p ide-mise-path-cache)
    (condition-case err
        (ide-mise--merge-path
         (with-temp-buffer (insert-file-contents ide-mise-path-cache) (read (current-buffer))))
      (error (message "mise: ignoring unreadable %s (%s); run M-x ide-mise-refresh-path"
                      ide-mise-path-cache (error-message-string err))))))

(defun ide-mise-refresh-path ()
  "Ask mise for the tool bin directories, apply and cache them."
  (interactive)
  (let ((mise (or (ide-mise-executable) (user-error "mise is not installed; run `ide-mise-setup'"))))
    (let* ((process-environment (append (ide-mise--env) process-environment))
           (default-directory (ide-mise--run-dir))
           (res (ide-run-with-timeout 30 mise "bin-paths")))
      (unless (eql (car res) 0)
        (user-error "`mise bin-paths' failed: %s" (string-trim (nth 2 res))))
      (let ((dirs (split-string (nth 1 res) "\n" t)))
        (make-directory ide-mise-dir t)
        (with-temp-file ide-mise-path-cache (prin1 dirs (current-buffer)))
        (ide-mise--merge-path dirs)
        (message "mise: %d tool directories on PATH" (length dirs))
        dirs))))

;;;; Bootstrap ------------------------------------------------------------------

(defun ide-mise--platform ()
  "Return the mise release platform suffix such as `linux-x64', or nil."
  (let ((os (pcase system-type ('gnu/linux "linux") ('darwin "macos")))
        (arch (cond ((string-match-p "\\`\\(x86_64\\|amd64\\)" system-configuration) "x64")
                    ((string-match-p "\\`\\(aarch64\\|arm64\\)" system-configuration) "arm64"))))
    (and os arch (concat os "-" arch))))

(defun ide-mise--parse-shasums (text asset)
  "Return the sha256 for ASSET found in SHASUMS256.txt TEXT, or nil."
  (when (string-match (format "^\\([0-9a-f]\\{64\\}\\)[ \t]+\\(?:\\./\\)?%s$" (regexp-quote asset)) text)
    (match-string 1 text)))

(defun ide-mise--curl (max-time &rest args)
  "Run curl with ARGS for at most MAX-TIME seconds; return stdout or signal."
  (let ((res (apply #'ide-run-with-timeout (+ max-time 10) "curl"
                    "-fsSL" "--retry" "2" "--max-time" (number-to-string max-time) args)))
    (unless (eql (car res) 0)
      (error "curl failed (%s): %s %s" (car res) (string-join args " ") (string-trim (nth 2 res))))
    (nth 1 res)))

(defun ide-mise--log (fmt &rest args)
  "Append a line to the mise drawer buffer."
  (with-current-buffer (get-buffer-create ide-mise-process-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (apply #'format fmt args) "\n"))))

(defun ide-mise-bootstrap ()
  "Download a checksum-verified mise binary into the sandbox."
  (interactive)
  (let* ((platform (or (ide-mise--platform) (user-error "Unsupported platform for automatic mise download")))
         (tag (or ide-mise-version
                  (let ((final-url (ide-mise--curl 30 "-I" "-o" "/dev/null" "-w" "%{url_effective}"
                                                   "https://github.com/jdx/mise/releases/latest")))
                    (if (string-match "/tag/\\(v[^/]+\\)\\'" final-url) (match-string 1 final-url)
                      (error "Could not determine latest mise release")))))
         (asset (format "mise-%s-%s" tag platform))
         (base (format "https://github.com/jdx/mise/releases/download/%s/" tag))
         (expected (or ide-mise-sha256
                       (ide-mise--parse-shasums (ide-mise--curl 30 (concat base "SHASUMS256.txt")) asset)
                       (error "No checksum published for %s" asset)))
         (bindir (expand-file-name "bin/" ide-mise-dir))
         (tmp (make-temp-file "mise-dl-"))
         (dest (expand-file-name "mise" bindir)))
    (unwind-protect
        (progn
          (ide-mise--log "Downloading %s" asset)
          (ide-mise--curl 600 "-o" tmp (concat base asset))
          (let ((actual (with-temp-buffer
                          (set-buffer-multibyte nil)
                          (insert-file-contents-literally tmp)
                          (secure-hash 'sha256 (current-buffer)))))
            (unless (equal actual expected)
              (error "Checksum mismatch for %s (expected %s, got %s)" asset expected actual)))
          (make-directory bindir t)
          (rename-file tmp dest t)
          (set-file-modes dest #o755)
          (ide-mise--log "Installed mise %s to %s (sha256 verified)" tag dest)
          dest)
      (when (file-exists-p tmp) (delete-file tmp)))))

;;;; Install --------------------------------------------------------------------

(defun ide-mise--az-python ()
  "Return the python interpreter of the installed azure-cli, from az's shebang."
  (when-let* ((az (executable-find "az")))
    (with-temp-buffer
      (insert-file-contents az nil 0 200)
      (when (looking-at "#!\\([^ \n]+\\)") (match-string 1)))))

(defun ide-mise--ensure-az-pip ()
  "mise installs azure-cli into a uv venv without pip, which `az extension add' needs."
  (when-let* ((py (ide-mise--az-python)))
    (when (and (file-executable-p py)
               (not (eql 0 (car (ide-run-with-timeout 30 py "-m" "pip" "--version"))))
               (executable-find "uv"))
      (ide-mise--log "Adding pip to the azure-cli environment")
      (let ((res (ide-run-with-timeout 180 "uv" "pip" "install" "pip" "--python" py)))
        (unless (eql (car res) 0)
          (ide-mise--log "Could not add pip: %s" (string-trim (nth 2 res))))))))

(defun ide-mise--post-install ()
  "Steps after a successful `mise install'."
  (ide-mise-refresh-path)
  (when (executable-find "az")
    (ide-mise--ensure-az-pip)
    (let ((listing (ide-run-with-timeout 60 "az" "extension" "list" "--query" "[].name" "-o" "tsv")))
      (unless (string-match-p "azure-devops" (nth 1 listing))
        (ide-mise--log "Adding the azure-devops az extension")
        (let ((res (ide-run-with-timeout 180 "az" "extension" "add" "--name" "azure-devops" "--yes")))
          (unless (eql (car res) 0)
            (ide-mise--log "az extension add failed (%s): %s" (car res) (string-trim (nth 2 res))))))))
  (when (and (not noninteractive) (fboundp 'ide-treesit-install-grammars))
    (ide-treesit-install-grammars))
  (ide-mise--log "Done.  M-x ide-mise-doctor shows what is available."))

(defun ide-mise-install (&optional callback)
  "Run `mise install' for the manifest in the drawer, then refresh PATH.
CALLBACK is called with t on success, nil on failure."
  (interactive)
  (let* ((mise (or (ide-mise-executable) (user-error "mise is not installed; run `ide-mise-setup'")))
         (buf (get-buffer-create ide-mise-process-buffer))
         (process-environment (append (ide-mise--env) process-environment))
         (default-directory (ide-mise--run-dir)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (goto-char (point-max)) (insert "\n$ mise install\n"))
      (special-mode))
    (unless noninteractive (display-buffer buf))
    (make-process
     :name "ide-mise-install" :buffer buf :command (list mise "install")
     :connection-type 'pipe
     :filter (lambda (proc s)
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t) (at-end (= (point) (point-max))))
                   (save-excursion (goto-char (point-max)) (insert s))
                   (when at-end (goto-char (point-max))))))
     :sentinel (lambda (proc _event)
                 (when (memq (process-status proc) '(exit signal))
                   (let ((ok (zerop (process-exit-status proc))))
                     (if ok (ide-mise--post-install)
                       (ide-mise--log "mise install failed (exit %s)" (process-exit-status proc)))
                     (message "mise install %s" (if ok "finished" "FAILED; see *ide-mise*"))
                     (when callback (funcall callback ok))))))))

;;;###autoload
(defun ide-mise-setup ()
  "Install mise (if needed) and every tool in mise.toml, all inside the sandbox."
  (interactive)
  (unless (executable-find "curl") (user-error "curl is required to download mise"))
  (unless (ide-mise-executable) (ide-mise-bootstrap))
  (ide-mise-install))

;;;###autoload
(defun ide-mise-edit-manifest ()
  "Open mise.toml to add or remove tools."
  (interactive)
  (find-file ide-mise-manifest))

;;;###autoload
(defun ide-mise-doctor ()
  "Show which expected executables are available and where they come from."
  (interactive)
  (let ((buf (get-buffer-create "*ide-mise-doctor*")) (missing 0))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "mise: %s\nmanifest: %s\n\n" (or (ide-mise-executable) "NOT INSTALLED") ide-mise-manifest))
        (dolist (e ide-mise-required-executables)
          (let ((path (executable-find (car e))))
            (unless path (cl-incf missing))
            (insert (format "%s %-24s %-26s %s\n" (if path "ok " "-- ") (car e) (cadr e) (or path "missing")))))
        (insert (format "\n%d missing.%s\n" missing
                        (if (> missing 0) "  Run M-x ide-mise-setup." ""))))
      (special-mode))
    (if noninteractive (princ (with-current-buffer buf (buffer-string))) (pop-to-buffer buf))
    missing))

(defun ide-mise-setup-batch ()
  "Synchronous `ide-mise-setup' for `emacs --batch -f ide-mise-setup-batch'."
  (let ((done nil) (ok nil))
    (unless (ide-mise-executable) (ide-mise-bootstrap))
    (ide-mise-install (lambda (r) (setq done t ok r)))
    (while (not done) (accept-process-output nil 0.2))
    (with-current-buffer ide-mise-process-buffer (princ (buffer-string)))
    (ide-mise-doctor)
    (kill-emacs (if ok 0 1))))

;;;; Startup ----------------------------------------------------------------------

(ide-mise-load-cached-path)

(when ide-contain-tool-state
  (pcase-dolist (`(,var . ,dir) (ide-mise-contained-env))
    (unless (getenv var)
      (make-directory dir t)
      (setenv var dir))))

(when (and ide-azure-config-dir (not (getenv "AZURE_CONFIG_DIR")))
  (make-directory ide-azure-config-dir t)
  (setenv "AZURE_CONFIG_DIR" ide-azure-config-dir))

(unless (or noninteractive (ide-mise-executable) (file-readable-p ide-mise-path-cache))
  (add-hook 'emacs-startup-hook
            (lambda () (message "Tools not installed yet: M-x ide-mise-setup"))))

(provide '00-mise)
;;; 00-mise.el ends here
