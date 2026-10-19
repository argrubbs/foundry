;;; lang-rust.el --- Rust: rust-analyzer, cargo, tests, debugger, docs -*- lexical-binding: t; -*-

;;; Commentary:
;; The whole Rust loop without leaving Emacs:
;;
;;   edit      rust-ts-mode + rust-analyzer (clippy diagnostics, inlay hints, rustfmt on save)
;;   run       S-<f10> runs the project; C-c m opens the cargo menu (build, run a chosen
;;             target, check, clippy, fmt, doc, bench, add/remove/update crates, tree)
;;   test      the test at point, the file, the whole project (cargo test or nextest),
;;             and the tests related to the function at point (rust-analyzer)
;;   debug     S-<f9> builds, then debugs the test or binary at point with codelldb
;;   explore   macro expansion, parent module, Cargo.toml, docs.rs / local docs in the
;;             in-Emacs browser, a REPL (evcxr)
;;
;; Toolchain (rust, rust-analyzer, clippy, rustfmt, nextest) comes from mise; the
;; debugger adapter is fetched by `ide-rust-install-debugger'.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'transient)
(require 'comint)

(require '00-lang-lib)
(require 'treesit)

(defvar dap-codelldb-debug-path)
(defvar dap-codelldb-download-url)
(declare-function dap-debug "dap-mode")
(declare-function treesit-node-prev-sibling "treesit")

(defgroup ide-rust nil "Rust support." :group 'ide-lang)

(defcustom ide-rust-test-runner 'cargo
  "Which runner executes tests: `cargo' (cargo test, includes doctests) or
`nextest' (cargo nextest, faster; falls back to cargo when not installed)."
  :type '(choice (const cargo) (const nextest)))

;;;; Mode and server ------------------------------------------------------------------

(ide-lang-setup 'rust-ts-mode
  :server '("rust-analyzer")
  :tab-width 4
  :indent-tabs nil
  :format-on-save nil)

(ide-lang-workspace-config
 :rust-analyzer
 '(:check (:command "clippy")
   :procMacro (:enable t)
   :cargo (:buildScripts (:enable t))
   :inlayHints (:closingBraceHints (:minLines 25)
                :lifetimeElisionHints (:enable "skip_trivial"))))

;;;; Cargo ----------------------------------------------------------------------------------

(defun ide-rust--cargo (&rest args)
  "Run `cargo ARGS' in the drawer."
  (ide-lang-run "cargo" (cons "cargo" args)))

(defun ide-rust--metadata ()
  "Parsed `cargo metadata' for the current project (no dependencies), or nil."
  (let ((default-directory (ide-lang-root)))
    (with-temp-buffer
      (when (zerop (call-process "cargo" nil '(t nil) nil "metadata" "--no-deps" "--format-version" "1"))
        (goto-char (point-min))
        (json-parse-buffer :object-type 'alist :array-type 'list)))))

(defun ide-rust--targets (kinds)
  "List of (LABEL KIND NAME PACKAGE) for the metadata targets whose kind is in KINDS."
  (cl-loop for pkg in (alist-get 'packages (ide-rust--metadata))
           append (cl-loop for tgt in (alist-get 'targets pkg)
                           for kind = (car (alist-get 'kind tgt))
                           when (member kind kinds)
                           collect (list (format "%s  (%s)" (alist-get 'name tgt) kind)
                                         kind (alist-get 'name tgt) (alist-get 'name pkg)))))

(dolist (spec '((ide-rust-build "build")
                (ide-rust-check "check" "--all-targets")
                (ide-rust-clippy "clippy" "--all-targets")
                (ide-rust-clippy-fix "clippy" "--fix" "--allow-dirty" "--allow-staged" "--all-targets")
                (ide-rust-fmt "fmt" "--all")
                (ide-rust-bench "bench")
                (ide-rust-clean "clean")
                (ide-rust-update "update")
                (ide-rust-tree "tree")))
  (let ((fn (car spec)) (args (cdr spec)))
    (defalias fn (lambda () (interactive) (apply #'ide-rust--cargo args)))))

(defun ide-rust-add (crate)
  "cargo add CRATE (features: CRATE --features a,b)."
  (interactive "sAdd crate: ")
  (ide-lang-run "cargo" (concat "cargo add " crate)))

(defun ide-rust-remove (crate)
  "cargo remove CRATE."
  (interactive "sRemove crate: ")
  (ide-rust--cargo "remove" crate))

(defun ide-rust-run (target)
  "cargo run for a binary or example TARGET chosen from the project."
  (interactive
   (let ((targets (ide-rust--targets '("bin" "example"))))
     (unless targets (user-error "No binary or example targets in this project"))
     (list (if (cdr targets)
               (assoc (completing-read "Run: " (mapcar #'car targets) nil t) targets)
             (car targets)))))
  (pcase-let ((`(,_label ,kind ,name ,pkg) target))
    (ide-rust--cargo "run" "-p" pkg (if (equal kind "example") "--example" "--bin") name)))

(defun ide-rust-doc ()
  "Build this project's docs and open them in the in-Emacs browser."
  (interactive)
  (let* ((meta (ide-rust--metadata))
         (dir (alist-get 'target_directory meta))
         (pkg (car (alist-get 'packages meta)))
         (crate (replace-regexp-in-string "-" "_" (alist-get 'name pkg))))
    (ide-lang-run-then "cargo doc" "cargo doc --no-deps"
                       (lambda () (ide-lang-browse (format "file://%s/doc/%s/index.html" dir crate))))))

;;;; Tests ------------------------------------------------------------------------------------

(defun ide-rust--test-command (&rest args)
  "The cargo command line for running tests with ARGS (a nextest or cargo test line)."
  (if (and (eq ide-rust-test-runner 'nextest) (executable-find "cargo-nextest"))
      (append '("cargo" "nextest" "run") args)
    (append '("cargo" "test") args)))

(defun ide-rust--module-path ()
  "Enclosing `mod' names of point, outermost first, as a list of strings."
  (let (names (node (treesit-node-parent (treesit-node-at (point)))))
    (while node
      (when (equal (treesit-node-type node) "mod_item")
        (push (treesit-node-text (treesit-node-child-by-field-name node "name") t) names))
      (setq node (treesit-node-parent node)))
    names))

(defun ide-rust--test-name-at-point ()
  "Qualified name (mod::path::fn) of the #[test] function around point, or nil."
  (when-let* ((fn (ide-lang-ancestor '("function_item")))
              (name (treesit-node-text (treesit-node-child-by-field-name fn "name") t)))
    (string-join (append (ide-rust--module-path) (list name)) "::")))

(defun ide-rust--file-test-filter ()
  "Arguments selecting the tests of the current file."
  (let ((rel (ide-lang-relative-file)))
    (cond
     ((string-match "\\`tests/\\([^/]+\\)\\.rs\\'" rel) (list "--test" (match-string 1 rel)))
     ((string-match "\\`\\(?:.*/\\)?src/\\(.*\\)\\.rs\\'" rel)
      (let ((m (match-string 1 rel)))
        (if (member m '("lib" "main")) nil
          (list (replace-regexp-in-string "/" "::" (replace-regexp-in-string "/mod\\'" "" m))))))
     (t nil))))

(defun ide-rust-test-all () "Run every test in the project." (interactive)
       (ide-lang-run "cargo test" (ide-rust--test-command)))

(defun ide-rust-test-file ()
  "Run the tests of the current file."
  (interactive)
  (ide-lang-run "cargo test" (apply #'ide-rust--test-command (ide-rust--file-test-filter))))

;;;; rust-analyzer requests ----------------------------------------------------------------------

(defun ide-rust--request (method &optional params)
  "Send rust-analyzer's custom request METHOD (a keyword) for the position at point."
  (let ((server (eglot--current-server-or-lose)))
    (jsonrpc-request server method
                     (or params (list :textDocument (eglot--TextDocumentIdentifier)
                                      :position (eglot--pos-to-lsp-position))))))

(defun ide-rust--runnable-command (runnable)
  "Shell command and environment for a rust-analyzer RUNNABLE: (ARGV . DIR)."
  (let* ((args (plist-get runnable :args))
         (cargo (append (plist-get args :cargoArgs) nil))
         (exe (append (plist-get args :executableArgs) nil))
         (cwd (or (plist-get args :cwd) (plist-get args :workspaceRoot) (ide-lang-root)))
         (argv (if (equal (plist-get runnable :kind) "shell")
                   (cons (plist-get args :program) (append (plist-get args :args) nil))
                 (append '("cargo") cargo (when exe (cons "--" exe))))))
    (cons argv cwd)))

(defun ide-rust--runnables ()
  "rust-analyzer's runnables for point: a list of plists."
  (append (ide-rust--request :experimental/runnables) nil))

(defun ide-rust--run-runnable (runnable)
  (pcase-let ((`(,argv . ,dir) (ide-rust--runnable-command runnable)))
    (ide-lang-run (or (plist-get runnable :label) "cargo") argv dir)))

(defun ide-rust--pick-runnable (&optional pattern)
  "Choose among the runnables at point, or take the first matching PATTERN."
  (let ((all (ide-rust--runnables)))
    (when pattern
      (setq all (cl-remove-if-not (lambda (r) (string-match-p pattern (plist-get r :label))) all)))
    (cond ((null all) nil)
          ((or pattern (null (cdr all))) (car all))
          (t (let ((table (mapcar (lambda (r) (cons (plist-get r :label) r)) all)))
               (cdr (assoc (completing-read "Run: " (mapcar #'car table) nil t) table)))))))

(defun ide-rust-run-at-point ()
  "Run what rust-analyzer offers at point: the test, the binary or the benchmark."
  (interactive)
  (if-let* ((r (ide-rust--pick-runnable)))
      (ide-rust--run-runnable r)
    (user-error "Nothing runnable at point")))

(defun ide-rust-test-at-point ()
  "Run the test at point (cargo test <path> -- --exact); the module's tests when point is in a mod."
  (interactive)
  (let ((runnable (and (bound-and-true-p eglot--managed-mode)
                       (ignore-errors (ide-rust--pick-runnable "\\`\\(test \\|doctest\\)")))))
    (cond
     (runnable (ide-rust--run-runnable runnable))
     ((ide-rust--test-name-at-point)
      (ide-lang-run "cargo test" (ide-rust--test-command (ide-rust--test-name-at-point) "--" "--exact" "--nocapture")))
     (t (user-error "No test at point")))))

(defun ide-rust-related-tests ()
  "Run the tests that exercise the function at point (rust-analyzer's related tests)."
  (interactive)
  (let ((related (append (ide-rust--request :rust-analyzer/relatedTests) nil)))
    (unless related (user-error "No related tests found"))
    (let* ((table (mapcar (lambda (x) (cons (plist-get (plist-get x :runnable) :label)
                                            (plist-get x :runnable)))
                          related)))
      (ide-rust--run-runnable (cdr (assoc (completing-read "Related test: " table nil t) table))))))

(defun ide-rust-runnables ()
  "Choose any runnable at point (run, test, bench) and run it in the drawer."
  (interactive)
  (ide-rust--run-runnable (or (ide-rust--pick-runnable) (user-error "Nothing runnable at point"))))

(defun ide-rust-expand-macro ()
  "Show the expansion of the macro call at point."
  (interactive)
  (let ((res (ide-rust--request :rust-analyzer/expandMacro)))
    (unless res (user-error "No macro at point"))
    (let ((buf (get-buffer-create (format "*rust macro: %s*" (plist-get res :name)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (plist-get res :expansion))
          (rust-ts-mode)
          (goto-char (point-min))
          (setq buffer-read-only t)))
      (pop-to-buffer buf))))

(defun ide-rust--goto-location (loc)
  "Visit the LSP Location or LocationLink LOC."
  (let* ((loc (if (vectorp loc) (aref loc 0) loc))
         (uri (or (plist-get loc :uri) (plist-get loc :targetUri)))
         (range (or (plist-get loc :range) (plist-get loc :targetSelectionRange)))
         (pos (plist-get range :start)))
    (find-file (eglot-uri-to-path uri))
    (goto-char (eglot--lsp-position-to-point pos))))

(defun ide-rust-open-cargo-toml ()
  "Open the Cargo.toml of the package this file belongs to."
  (interactive)
  (if-let* ((loc (ide-rust--request :experimental/openCargoToml)))
      (ide-rust--goto-location loc)
    (user-error "No Cargo.toml found")))

(defun ide-rust-parent-module ()
  "Go to the module that declares this file."
  (interactive)
  (let ((locs (ide-rust--request :experimental/parentModule)))
    (if (and locs (> (length locs) 0)) (ide-rust--goto-location locs) (user-error "No parent module"))))

(defun ide-rust-docs-at-point ()
  "Open the documentation of the symbol at point (docs.rs or local) in the browser."
  (interactive)
  (let* ((res (ide-rust--request :experimental/externalDocs))
         (url (cond ((stringp res) res)
                    ((listp res) (or (plist-get res :local) (plist-get res :web))))))
    (if url (ide-lang-browse url) (user-error "No documentation found for this symbol"))))

(defun ide-rust-reload-workspace ()
  "Reload the Cargo workspace (after editing Cargo.toml or build scripts)."
  (interactive)
  (jsonrpc-request (eglot--current-server-or-lose) :rust-analyzer/reloadWorkspace nil)
  (message "rust-analyzer: workspace reloaded"))

;;;; Debugging ---------------------------------------------------------------------------------------

(defun ide-rust-install-debugger ()
  "Download the codelldb debug adapter into the sandbox."
  (interactive)
  (require 'dap-codelldb)
  (ide-lang-fetch-extract "codelldb" dap-codelldb-download-url dap-codelldb-debug-path 'zip))

(defun ide-rust--artifact (json-lines)
  "The executable path of the last compiler artifact in cargo's JSON-LINES output."
  (let (exe)
    (dolist (line (split-string json-lines "\n" t))
      (when (string-prefix-p "{" line)
        (let ((m (ignore-errors (json-parse-string line :object-type 'alist :array-type 'list))))
          (when (and m (equal (alist-get 'reason m) "compiler-artifact")
                     (stringp (alist-get 'executable m)))
            (setq exe (alist-get 'executable m))))))
    exe))

(defun ide-rust--build-for-debug (cargo-args callback)
  "Build with CARGO-ARGS asynchronously, then call CALLBACK with the executable path."
  (let* ((default-directory (ide-lang-root))
         (out (generate-new-buffer " *cargo-debug-build*"))
         (err (generate-new-buffer "*run: cargo build (debug)*")))
    (message "Building for the debugger...")
    (make-process
     :name "cargo-debug-build" :buffer out :stderr err :noquery t
     :command (append '("cargo") cargo-args '("--message-format=json-render-diagnostics"))
     :sentinel (lambda (p _)
                 (when (memq (process-status p) '(exit signal))
                   (let ((exe (and (zerop (process-exit-status p))
                                   (ide-rust--artifact (with-current-buffer out (buffer-string))))))
                     (kill-buffer out)
                     (if exe
                         (progn (kill-buffer err) (funcall callback exe))
                       (pop-to-buffer err)
                       (message "Build failed; nothing to debug"))))))))

(defun ide-rust--debug-build-args (cargo-args)
  "Turn a runnable's CARGO-ARGS (run/test/bench ...) into a build that yields the executable."
  (pcase (car cargo-args)
    ("run" (cons "build" (cdr cargo-args)))
    ((or "test" "bench") (append (list (car cargo-args) "--no-run") (cdr cargo-args)))
    (_ (cons "build" (cdr cargo-args)))))

(defun ide-rust-debug ()
  "Debug the test or binary at point with codelldb.
Builds first (the compiler output stays in the drawer on errors), then launches
the executable under the debugger with breakpoints, locals and the debug REPL."
  (interactive)
  (require 'dap-mode)
  (require 'dap-codelldb)
  (unless (file-exists-p dap-codelldb-debug-path)
    (user-error "The debugger is not installed; run M-x ide-rust-install-debugger"))
  (let* ((r (or (ide-rust--pick-runnable "\\`\\(test \\|doctest\\|bench\\)")
                (ide-rust--pick-runnable "\\`run")
                (user-error "Nothing to debug at point")))
         (args (plist-get r :args))
         (cargo-args (append (plist-get args :cargoArgs) nil))
         (exe-args (append (plist-get args :executableArgs) nil))
         (cwd (or (plist-get args :cwd) (ide-lang-root)))
         (name (or (plist-get r :label) "Rust")))
    (ide-rust--build-for-debug
     (ide-rust--debug-build-args cargo-args)
     (lambda (program)
       (dap-debug (list :type "lldb" :request "launch" :name name :program program
                        :args (vconcat exe-args) :cwd cwd :sourceLanguages ["rust"]))))))

;;;; REPL ----------------------------------------------------------------------------------------------

(defun ide-rust-repl ()
  "Open an evcxr Rust REPL in the drawer."
  (interactive)
  (let ((exe (or (executable-find "evcxr") (user-error "evcxr not installed; run M-x ide-mise-setup"))))
    (pop-to-buffer (apply #'make-comint "evcxr" exe nil nil))))

(defun ide-rust-repl-send-region (beg end)
  "Send the region (or the current function) to the evcxr REPL."
  (interactive (ide-lang-region-or-node '("function_item" "struct_item" "impl_item")))
  (unless (get-buffer "*evcxr*") (ide-rust-repl))
  (comint-send-string (get-buffer-process "*evcxr*") (concat (buffer-substring-no-properties beg end) "\n")))

;;;; Run / debug hooks and menu -----------------------------------------------------------------------------

(defun ide-rust--run-command ()
  (when (derived-mode-p 'rust-ts-mode)
    (let ((rel (ide-lang-relative-file)))
      (if (and rel (string-prefix-p "tests/" rel))
          (ide-lang-command-string (ide-rust--test-command "--test" (file-name-base rel)))
        "cargo run"))))

;;;###autoload (autoload 'ide-rust-dispatch "lang-rust" nil t)
(transient-define-prefix ide-rust-dispatch ()
  "Rust: build, test, debug, explore."
  [["Build"
    ("b" "build" ide-rust-build)
    ("r" "run a target..." ide-rust-run)
    ("c" "check" ide-rust-check)
    ("l" "clippy" ide-rust-clippy)
    ("L" "clippy --fix" ide-rust-clippy-fix)
    ("f" "fmt" ide-rust-fmt)
    ("B" "bench" ide-rust-bench)
    ("C" "clean" ide-rust-clean)]
   ["Test"
    ("t" "test at point" ide-rust-test-at-point)
    ("F" "tests in file" ide-rust-test-file)
    ("a" "all tests" ide-rust-test-all)
    ("R" "related tests" ide-rust-related-tests)
    ("p" "runnables at point..." ide-rust-runnables)
    ("." "run at point" ide-rust-run-at-point)]
   ["Debug / REPL"
    ("d" "debug at point (codelldb)" ide-rust-debug)
    ("I" "install debugger" ide-rust-install-debugger)
    ("e" "REPL (evcxr)" ide-rust-repl)
    ("s" "send to REPL" ide-rust-repl-send-region)]
   ["Crates"
    ("+" "add" ide-rust-add)
    ("-" "remove" ide-rust-remove)
    ("u" "update" ide-rust-update)
    ("T" "tree" ide-rust-tree)]
   ["Explore"
    ("m" "expand macro" ide-rust-expand-macro)
    ("M" "parent module" ide-rust-parent-module)
    ("o" "open Cargo.toml" ide-rust-open-cargo-toml)
    ("D" "docs at point" ide-rust-docs-at-point)
    ("O" "project docs" ide-rust-doc)
    ("w" "reload workspace" ide-rust-reload-workspace)]])

(ide-lang-register '(rust-ts-mode rust-mode)
  :run #'ide-rust--run-command
  :debug (lambda () (when (derived-mode-p 'rust-ts-mode) (ide-rust-debug) t))
  :menu #'ide-rust-dispatch)

(provide 'lang-rust)
;;; lang-rust.el ends here
