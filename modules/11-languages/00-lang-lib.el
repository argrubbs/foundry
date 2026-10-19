;;; 00-lang-lib.el --- Shared helpers for the language modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads first among the language modules (the name sorts before lang-*).  What
;; every language needs, written once:
;;
;; - `ide-lang-setup'            a language's modes: server, indentation, who formats on save
;; - `ide-lang-register'         its run / debug commands and its C-c m menu
;; - `ide-lang-run'              run a command in the bottom drawer
;; - `ide-lang-define-linter'    an external linter as a Flymake backend (Eglot manages
;;                               one server per buffer; Flymake merges any number of linters)
;; - `ide-lang-config-file'      the nearest configuration file of a tool
;; - `ide-lang-show-text'        a tool's output in a read-only buffer
;; - `ide-lang-sandbox-env'      an environment that keeps a tool's state in the sandbox
;; - `ide-lang-node-exe'         project-local node_modules/.bin tool, else PATH
;; - `ide-lang-ancestor'         nearest tree-sitter ancestor of a given type
;; - `ide-lang-fetch-extract'    download an archive into the sandbox
;; - `ide-lang-browse'           show a URL or file in the in-Emacs browser

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'compile)
(require 'project)
(require 'flymake)
(require 'treesit)

(use-package apheleia
  :commands (apheleia-mode apheleia-format-buffer))

(defvar ide-var-dir)
(defvar trusted-content)
(defvar eglot--managed-mode)
(defvar ide-eglot-format-on-save)
(defvar ide-run-command-functions)
(defvar ide-debug-functions)
(defvar apheleia-formatters)
(defvar apheleia-mode-alist)
(defvar eglot-server-programs)
(declare-function ide-add-drawer-buffer "02-layout")
(declare-function ide-mise-contained-env "00-mise")
(declare-function ide-docs--use-xwidget-p "15-docs")
(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function which-key-add-key-based-replacements "which-key")
(declare-function eglot-code-actions "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-ensure "eglot")
(declare-function apheleia-mode "apheleia")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")

;;;; Running ------------------------------------------------------------------------

(defgroup ide-lang nil "Language support." :group 'tools)

(defconst ide-lang-run-buffer-regexp "\\*run: .*\\*"
  "Buffers made by `ide-lang-run' dock into the bottom drawer.")

(when (fboundp 'ide-add-drawer-buffer)
  (ide-add-drawer-buffer ide-lang-run-buffer-regexp))

;; Test runners and build tools colour their output; render it instead of showing escapes.
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(defun ide-lang-root (&optional dir)
  "Project root for DIR (default `default-directory'), or DIR itself."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (if-let* ((p (project-current nil default-directory))) (project-root p) default-directory)))

(defun ide-lang-quote (arg)
  "ARG quoted for a shell command line, left bare when it is made of safe characters."
  (if (string-match-p "\\`[A-Za-z0-9_@%+=:,./-]+\\'" arg) arg (shell-quote-argument arg)))

(defun ide-lang-command-string (command)
  "Convert COMMAND (a string, or a list of arguments, nil ones skipped) to a shell command string."
  (if (stringp command) command (mapconcat #'ide-lang-quote (remq nil command) " ")))

(defun ide-lang-require-file (&optional save)
  "Signal a `user-error' if current buffer is not visiting a file.
When SAVE is non-nil, save the buffer first.  Returns `buffer-file-name'."
  (unless buffer-file-name (user-error "Buffer is not visiting a file"))
  (when (and save (buffer-modified-p)) (save-buffer))
  buffer-file-name)

(defun ide-lang-relative-file ()
  "Current buffer file name relative to `ide-lang-root', or nil."
  (when buffer-file-name
    (file-relative-name buffer-file-name (ide-lang-root))))

(defun ide-lang-code-action (kind)
  "Execute an Eglot code action matching KIND if supported."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot-server-capable)
             (eglot-server-capable :codeActionProvider))
    (ignore-errors
      (eglot-code-actions nil nil kind t))))

(defun ide-lang-run (name command &optional dir comint)
  "Run COMMAND (a string, or a list of arguments) in the drawer as *run: NAME*.
It runs in DIR (default: the project root).  Modified project buffers are saved
first.  With COMINT the buffer accepts input, for programs that read stdin."
  (let* ((default-directory (file-name-as-directory (or dir (ide-lang-root))))
         (root default-directory)
         (compilation-buffer-name-function (lambda (_mode) (format "*run: %s*" name)))
         (compilation-ask-about-save nil)
         ;; Only this project's files, not every modified buffer in Emacs.
         (compilation-save-buffers-predicate
          (lambda () (and buffer-file-name (file-in-directory-p buffer-file-name root)))))
    (compile (ide-lang-command-string command) comint)))

(defun ide-lang-exe-or-error (name &optional hint)
  "Path to executable NAME, or a `user-error' mentioning HINT."
  (or (executable-find name)
      (user-error "%s not found%s" name (if hint (concat "; " hint) "; run M-x ide-mise-setup"))))

(defun ide-lang-node-exe (name &optional dir)
  "Executable NAME from the nearest node_modules/.bin above DIR, else on PATH."
  (or (when-let* ((root (locate-dominating-file (or dir default-directory) "node_modules"))
                  (f (expand-file-name (concat "node_modules/.bin/" name) root)))
        (and (file-executable-p f) f))
      (executable-find name)))

(defun ide-lang-config-file (names &optional start)
  "The nearest file called one of NAMES in the directory of START or above, or nil.
START defaults to the buffer's file, else `default-directory'.  The closest
directory holding any of the names wins; inside it NAMES are tried in order."
  (let ((names (ensure-list names)))
    (when-let* ((dir (locate-dominating-file
                      (or start buffer-file-name default-directory)
                      (lambda (d) (cl-some (lambda (n) (file-exists-p (expand-file-name n d))) names)))))
      (cl-loop for n in names
               for f = (expand-file-name n dir)
               when (file-exists-p f) return f))))

(defun ide-lang-show-text (name text &optional mode)
  "Show TEXT in the read-only buffer NAME, in MODE (default `special-mode'), and return it."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (funcall (or mode #'special-mode))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    buf))

(defun ide-lang-sandbox-env (tool &rest extra)
  "Environment entries (\"VAR=VALUE\") that keep TOOL's state under var/tools/TOOL/.
The XDG data, config, cache and state directories and TMPDIR point there.  EXTRA
entries are (VAR . SUBDIR) pairs, each a directory under that home, or ready-made
\"VAR=VALUE\" strings.  The shared caches of `ide-mise-contained-env' (npm, trivy,
...) are added for the variables the environment does not set already."
  (let ((home (ide-lang-tools-dir tool)))
    (append
     (cl-loop for (var . sub) in (append '(("XDG_DATA_HOME" . "data") ("XDG_CONFIG_HOME" . "config")
                                           ("XDG_CACHE_HOME" . "cache") ("XDG_STATE_HOME" . "state")
                                           ("TMPDIR" . "tmp"))
                                         (cl-remove-if-not #'consp extra))
              for dir = (expand-file-name sub home)
              do (make-directory dir t)
              collect (concat var "=" dir))
     (cl-remove-if #'consp extra)
     (cl-loop for (var . dir) in (ide-mise-contained-env)
              unless (getenv var) collect (concat var "=" dir)))))

;;;; Flymake linters ----------------------------------------------------------------------

(defvar-local ide-lang--lint-procs nil "Alist (LINTER . PROCESS) of the running lint processes.")
(defvar-local ide-lang--trust-hinted nil "Linters that already told the user they are paused.")
(declare-function trusted-content-p "files")

(defun ide-lang-severity (string)
  "Flymake diagnostic type for the severity STRING a tool printed."
  (let ((s (downcase (or string ""))))
    (cond ((string-match-p "\\`\\(error\\|err\\|fatal\\|e\\|2\\)\\'" s) :error)
          ((string-match-p "\\`\\(warn\\|warning\\|w\\|1\\)\\'" s) :warning)
          (t :note))))

(defun ide-lang-diag (buffer linter line col severity message &optional end-col)
  "A Flymake diagnostic in BUFFER from LINTER at LINE and COL (1-based; COL may be nil).
SEVERITY is a string or a Flymake type; END-COL, when given, ends the region."
  (let* ((type (if (keywordp severity) severity (ide-lang-severity severity)))
         (region (with-current-buffer buffer
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (forward-line (max 0 (1- line)))
                       (let* ((bol (line-beginning-position)) (eol (line-end-position))
                              (beg (if (and col (> col 0)) (min eol (+ bol (1- col))) bol))
                              (end (cond ((and end-col (> end-col col)) (min eol (+ bol (1- end-col))))
                                         ((and col (< beg eol)) (min eol (1+ beg)))
                                         (t eol))))
                         (cons beg (if (= beg end) (min (point-max) (1+ end)) end))))))))
    (flymake-make-diagnostic buffer (car region) (cdr region) type
                             (format "%s: %s" linter (string-trim message)))))

(defun ide-lang-parse-regexp (linter regexp &rest groups)
  "A parser for LINTER output where each match of REGEXP is one diagnostic.
GROUPS is a plist of regexp group numbers: :line :col :severity :message
(:severity may be omitted, giving warnings)."
  (lambda (output buffer)
    (let (diags)
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((line (string-to-number (or (match-string (plist-get groups :line)) "1")))
                (col (when-let* ((g (plist-get groups :col)) (m (match-string g)))
                       (string-to-number m)))
                (sev (if-let* ((g (plist-get groups :severity))) (match-string g) "warning"))
                (msg (match-string (plist-get groups :message))))
            (push (ide-lang-diag buffer linter line col sev msg) diags))))
      (nreverse diags))))

(cl-defun ide-lang-define-linter (name &key modes program args parse (stdin t) (saved-only nil) safe)
  "Define Flymake backend `ide-lang-flymake-NAME' for an external linter and
enable it in the mode hooks of MODES.

PROGRAM is an executable name, or a function returning a path (or nil).
ARGS is a list of strings (\"%f\" stands for the file name) or a function of the
file name.  PARSE is called with the output and the source buffer and returns
diagnostics.  STDIN non-nil sends the buffer to the process, so unsaved edits are
linted; otherwise the saved file is read, and with SAVED-ONLY the backend stays
quiet while the buffer has unsaved changes.

Emacs 31 only runs a Flymake backend in untrusted content (a file outside
`trusted-content') when the backend is declared safe.  SAFE non-nil means the
tool merely reads text and always runs.  A linter that executes code from the
project (ESLint loads eslint.config.js, markdownlint and sqlfluff can load
code from their config) must leave SAFE nil: it then pauses, with a one-time
hint, until the project is trusted (`ide-lang-trust-project').

Eglot replaces a buffer's Flymake backends when it connects; the backend is put
back on `eglot-managed-mode-hook'."
  (let ((fn (intern (format "ide-lang-flymake-%s" name))))
    (defalias fn
      (lambda (report-fn &rest _)
        (let ((exe (if (functionp program) (funcall program) (executable-find program)))
              (src (current-buffer)))
          (when-let* ((old (alist-get name ide-lang--lint-procs)))
            (when (process-live-p old) (kill-process old)))
          (cond
           ;; An unsafe linter checks trust itself (see the docstring), so the user gets
           ;; one clear hint instead of Emacs's generic "Disabling ... (untrusted content)".
           ((and (not safe) (not (trusted-content-p)))
            (unless (memq name ide-lang--trust-hinted)
              (push name ide-lang--trust-hinted)
              (message "%s is paused: it runs project code. M-x ide-lang-trust-project to enable" name))
            (funcall report-fn nil))
           ((not exe) (funcall report-fn nil))
           ((and saved-only (buffer-modified-p)) (funcall report-fn nil))
           (t
            (let* ((file (buffer-file-name))
                   (argv (cons exe (if (functionp args) (funcall args file)
                                     (mapcar (lambda (a) (if (equal a "%f") (or file "-") a)) args))))
                   (input (and stdin (buffer-substring-no-properties (point-min) (point-max))))
                   (out (generate-new-buffer (format " *lint-%s*" name)))
                   (default-directory (ide-lang-root))
                   proc)
              (setq proc
                    (make-process
                     :name (format "lint-%s" name) :buffer out :command argv
                     :connection-type 'pipe :noquery t
                     :sentinel
                     (lambda (p _event)
                       (when (memq (process-status p) '(exit signal))
                         (unwind-protect
                             (when (and (buffer-live-p src)
                                        (eq p (alist-get name (buffer-local-value 'ide-lang--lint-procs src))))
                               (funcall report-fn
                                        (condition-case err
                                            (funcall parse (with-current-buffer out (buffer-string)) src)
                                          (error (flymake-log :warning "%s parser: %s" name (error-message-string err))
                                                 nil))))
                           (kill-buffer out))))))
              (setf (alist-get name ide-lang--lint-procs) proc)
              (when stdin
                (process-send-string proc input)
                (process-send-eof proc))))))))
    ;; Flymake only calls a backend in untrusted content when it is declared safe.  Safe
    ;; linters are; unsafe ones are declared too but refuse to run until the project is
    ;; trusted (the check is in the backend above), which gives a better message.
    (function-put fn 'flymake-always-safe t)
    (dolist (mode modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook (lambda ()
                         (add-hook 'flymake-diagnostic-functions fn nil t)
                         (unless (bound-and-true-p flymake-mode) (flymake-mode 1))))))
    (add-hook 'eglot-managed-mode-hook
              (lambda ()
                (when (and modes (apply #'derived-mode-p modes) (bound-and-true-p eglot--managed-mode))
                  (add-hook 'flymake-diagnostic-functions fn nil t)
                  (when (bound-and-true-p flymake-mode) (flymake-start)))))
    fn))

(defun ide-lang-trust-project ()
  "Trust this project's files, so linters that run project code (ESLint, ...) work.
Emacs refuses to run such Flymake backends in files it does not trust.  Only do
this for projects whose configuration you would run anyway.  The choice is saved."
  (interactive)
  (let ((root (abbreviate-file-name (ide-lang-root))))
    (when (y-or-n-p (format "Trust all files under %s (linters may run its config code)? " root))
      (customize-save-variable 'trusted-content
                               (cl-adjoin root (and (boundp 'trusted-content) trusted-content) :test #'equal))
      (when (bound-and-true-p flymake-mode) (flymake-start))
      (message "Trusting %s" root))))

;;;; Language menus and server settings ------------------------------------------------------

(defvar ide-lang-menus nil
  "Alist of (MODES . COMMAND): the per-language menu `ide-lang-menu' opens (C-c m).")

(defun ide-lang-register-menu (modes command)
  "Make COMMAND the `ide-lang-menu' for buffers derived from any of MODES."
  (setq ide-lang-menus (cl-remove command ide-lang-menus :key #'cdr))
  (push (cons modes command) ide-lang-menus))

;;;###autoload
(defun ide-lang-menu ()
  "Open the menu of the language of this buffer: build, test, debug, docs, server."
  (interactive)
  (if-let* ((cmd (cl-loop for (modes . c) in ide-lang-menus
                          when (apply #'derived-mode-p modes) return c)))
      (call-interactively cmd)
    (user-error "No language menu for %s" major-mode)))

(defvar eglot-workspace-configuration)

(defun ide-lang-workspace-config (key value)
  "Set KEY (a keyword such as :rust-analyzer) to VALUE in the default Eglot
workspace configuration, keeping what other languages put there."
  ;; Eglot loads lazily; 06-lsp's :config (registered first) sets the base value.
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration
                  (plist-put (copy-sequence (default-value 'eglot-workspace-configuration))
                             key value))))

(defun ide-lang-run-then (name command callback &optional dir)
  "Like `ide-lang-run', and call CALLBACK once COMMAND finishes successfully."
  (let ((buf (ide-lang-run name command dir)))
    (with-current-buffer buf
      (add-hook 'compilation-finish-functions
                (lambda (b status)
                  (when (and (eq b buf) (string-prefix-p "finished" status))
                    (funcall callback)))
                nil t))
    buf))

(with-eval-after-load '10-keybindings
  (defvar ide-keys-mode-map)
  (define-key ide-keys-mode-map (kbd "C-c m") #'ide-lang-menu))

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "C-c m" "language menu"))

;;;; Mode registration & setup -------------------------------------------------

(defun ide-lang-define-formatter (name command modes)
  "Register formatter NAME running COMMAND for MODES with Apheleia."
  (with-eval-after-load 'apheleia
    (setf (alist-get name apheleia-formatters) command)
    (dolist (mode (if (listp modes) modes (list modes)))
      (setf (alist-get mode apheleia-mode-alist) name))))

(defvar ide-lang-lazy-modules nil
  "Alist (FEATURE . MODES): language modules and the major modes that load them.")

(defun ide-lang-lazy (feature modes)
  "Load FEATURE, a lang-*.el module, the first time one of MODES starts.
The load happens before the mode's body, so hooks the module adds run for that
buffer too.  In batch runs the module loads right away."
  (push (cons feature modes) ide-lang-lazy-modules)
  (if noninteractive
      (require feature)
    (dolist (mode modes)
      (advice-add mode :before
                  (lambda (&rest _)
                    (dolist (m modes) (advice-remove m feature))
                    (condition-case err
                        (require feature)
                      (error (display-warning
                              'ide (format "Module %s failed: %s" feature (error-message-string err))
                              :error))))
                  `((name . ,feature) (depth . -100))))))

(cl-defun ide-lang-register (modes &key run debug menu)
  "Register RUN and DEBUG hook functions and MENU for MODES."
  (when menu (ide-lang-register-menu modes menu))
  (when run (add-hook 'ide-run-command-functions run))
  (when debug (add-hook 'ide-debug-functions debug)))

(cl-defun ide-lang-setup (modes &key server auto-mode ((:tab-width width)) (indent-tabs nil)
                                (eglot t) (format-on-save t) config)
  "Configure MODES (symbol or list of symbols) with common IDE conventions.
SERVER is added to `eglot-server-programs' (not needed when Eglot ships the entry).
AUTO-MODE is a regexp or list of regexps associated in `auto-mode-alist'.
:tab-width sets `tab-width' (and mode-specific indent offsets).
INDENT-TABS sets `indent-tabs-mode' (defaults to nil: spaces only).
EGLOT non-nil starts the language server in every buffer; a function starts it
only in the buffers where it returns non-nil.
FORMAT-ON-SAVE says who formats on save: t Apheleia (the server's formatting is
turned off), nil the language server, `off' neither (another mode does it), or a
function returning one of those per buffer.
CONFIG is a function run in each mode's hook."
  (let ((mode-list (if (listp modes) modes (list modes))))
    (when server
      (with-eval-after-load 'eglot
        (add-to-list 'eglot-server-programs
                     (cons (if (cdr mode-list) mode-list (car mode-list)) server))))
    (when auto-mode
      (let ((primary (car mode-list)))
        (dolist (pat (if (listp auto-mode) auto-mode (list auto-mode)))
          (add-to-list 'auto-mode-alist (cons pat primary)))))
    (dolist (mode mode-list)
      (let ((hook (intern (format "%s-hook" mode))))
        (when (eq eglot t)
          (add-hook hook #'eglot-ensure))
        (add-hook hook
                  (lambda ()
                    (when width
                      (setq-local tab-width width)
                      (dolist (var '(lua-ts-indent-offset
                                     typescript-ts-indent-offset
                                     js-indent-level
                                     css-indent-offset
                                     html-ts-indent-offset
                                     go-ts-mode-indent-offset
                                     powershell-indent-level))
                        (when (boundp var) (set (make-local-variable var) width))))
                    (setq-local indent-tabs-mode indent-tabs)
                    (let ((who (if (functionp format-on-save) (funcall format-on-save) format-on-save)))
                      (when who (setq-local ide-eglot-format-on-save nil))
                      (when (and (eq who t) (fboundp 'apheleia-mode)) (apheleia-mode 1)))
                    (when config
                      (funcall config))
                    (when (and (functionp eglot) (funcall eglot))
                      (eglot-ensure))))))))

;;;; Tree-sitter helpers ----------------------------------------------------------------------

(defun ide-lang-ancestor (types &optional node)
  "Nearest ancestor-or-self of NODE (default: the node at point) whose type is in TYPES."
  (treesit-parent-until (or node (treesit-node-at (point)))
                        (lambda (n) (member (treesit-node-type n) types))
                        t))

(defun ide-lang-region-or-node (&optional types)
  "Return (BEG END) of active region, nearest ancestor of TYPES, or current line."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (if-let* ((node (and types (ide-lang-ancestor types))))
        (list (treesit-node-start node) (treesit-node-end node))
      (list (line-beginning-position) (line-end-position)))))

;;;; Downloads and browsing -------------------------------------------------------------------

(defun ide-lang-tools-dir (&rest parts)
  "A path under var/tools/ in the sandbox (created on demand)."
  (let ((dir (apply #'expand-file-name (string-join parts "/")
                    (list (expand-file-name "tools/" ide-var-dir)))))
    (make-directory (file-name-directory (directory-file-name dir)) t)
    dir))

(defun ide-lang-github-asset-url (repo regexp)
  "URL of the latest release asset of REPO (owner/name) whose name matches REGEXP."
  (with-temp-buffer
    (unless (zerop (call-process "curl" nil t nil "-fsSL" "--max-time" "20"
                                 (format "https://api.github.com/repos/%s/releases/latest" repo)))
      (user-error "Could not query the latest release of %s" repo))
    (goto-char (point-min))
    (let ((assets (alist-get 'assets (json-parse-buffer :object-type 'alist :array-type 'list))))
      (or (cl-loop for a in assets
                   when (string-match-p regexp (alist-get 'name a))
                   return (alist-get 'browser_download_url a))
          (user-error "No asset matching %s in the latest %s release" regexp repo)))))

(defun ide-lang-fetch-extract (name url dir &optional kind)
  "Download URL and extract it into DIR, showing progress in the drawer.
KIND is `tar' (a .tar.gz, the default) or `zip'."
  (make-directory dir t)
  (ide-lang-run (format "install %s" name)
                (if (eq kind 'zip)
                    (let ((zip (shell-quote-argument (expand-file-name "download" dir)))
                          (u (shell-quote-argument url))
                          (d (shell-quote-argument dir)))
                      (format "curl -fsSL -o %s.zip %s && unzip -qo %s.zip -d %s && rm %s.zip && echo done"
                              zip u zip d zip))
                  (format "curl -fsSL %s | tar -xz -C %s && echo done"
                          (shell-quote-argument url) (shell-quote-argument dir)))
                dir))

(defun ide-lang-browse (url)
  "Show URL in the in-Emacs browser (xwidget WebKit) beside the code, else the system browser."
  (if (and (fboundp 'ide-docs--use-xwidget-p) (ide-docs--use-xwidget-p))
      (let ((display-buffer-overriding-action
             '(display-buffer-in-side-window (side . right) (slot . 2) (window-width . 0.45))))
        (xwidget-webkit-browse-url url))
    (browse-url url)))

(provide '00-lang-lib)
(provide 'lang-lib)
;;; 00-lang-lib.el ends here
