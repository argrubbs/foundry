;;; lang-web.el --- TypeScript, JavaScript, JSX/TSX, HTML, CSS -*- lexical-binding: t; -*-

;;; Commentary:
;; The web loop without leaving Emacs:
;;
;;   edit      tree-sitter modes for .ts .tsx .js .jsx .html .css; Eglot (the native
;;             TypeScript 7 server, `tsc --lsp --stdio', or typescript-language-server for
;;             projects that ship TypeScript 5/6; vscode-html/css-language-server);
;;             prettier on save through Apheleia; ESLint as a Flymake backend; Emmet
;;   run       S-<f10> runs the file (tsx / node), or the test file for *.test.* files;
;;             C-c m opens the menu (package scripts, install/add/remove, dev server,
;;             type-check, outdated, audit)
;;   test      vitest or jest (detected from package.json): the test at point (the `it',
;;             `test' or `describe' title is found with tree-sitter and passed to -t), the
;;             file, everything, watch mode
;;   debug     S-<f9> debugs the file (or the test) with vscode-js-debug through dap-mode;
;;             an npm script can be debugged too.  The adapter is fetched by
;;             `ide-web-install-debugger'.
;;   preview   simple-httpd serves the project; impatient-mode updates the page while you
;;             type; saving reloads static previews, shown in the in-Emacs browser
;;   explore   organize imports, remove unused imports, fix all, test/source counterpart,
;;             MDN / TypeScript / npm docs for the symbol at point, a Node REPL

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'transient)

(defvar ide-run-command-functions)
(defvar ide-debug-functions)
(defvar ide-eglot-format-on-save)
(defvar ide-var-dir)
(defvar apheleia-formatters)
(defvar apheleia-mode-alist)
(defvar dap-js-path)
(defvar dap-js-debug-program)
(defvar httpd-root)
(defvar httpd-port)
(defvar httpd-host)
(defvar imp-default-user-filters)
(defvar emmet-jsx-major-modes)
(defvar emmet-move-cursor-between-quotes)
(defvar nodejs-repl-command)
(defvar nodejs-repl-process-name)
(defvar js-indent-level)
(defvar css-indent-offset)
(defvar html-ts-indent-offset)
(defvar typescript-ts-indent-offset)
(defvar compilation-environment)
(declare-function dap-debug "dap-mode")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-ensure "eglot")
(declare-function apheleia-mode "apheleia")
(declare-function apheleia-format-buffer "apheleia")
(declare-function emmet-mode "emmet-mode")
(declare-function httpd-start "simple-httpd")
(declare-function httpd-running-p "simple-httpd")
(declare-function impatient-mode "impatient-mode")
(declare-function imp--buffer-list "impatient-mode")
(declare-function nodejs-repl "nodejs-repl")
(declare-function nodejs-repl-send-region "nodejs-repl")
(declare-function nodejs-repl-send-buffer "nodejs-repl")
(declare-function nodejs-repl-switch-to-repl "nodejs-repl")
(declare-function xwidget-webkit-reload "xwidget")
(declare-function xwidget-webkit-uri "xwidget")
(declare-function xwidget-webkit-current-session "xwidget")
(declare-function ide-lang-run "00-lang-lib")
(declare-function ide-lang-run-then "00-lang-lib")
(declare-function ide-lang-root "00-lang-lib")
(declare-function ide-lang-ancestor "00-lang-lib")
(declare-function ide-lang-browse "00-lang-lib")
(declare-function ide-lang-node-exe "00-lang-lib")
(declare-function ide-lang-tools-dir "00-lang-lib")
(declare-function ide-lang-diag "00-lang-lib")
(declare-function ide-lang-define-linter "00-lang-lib")
(declare-function ide-lang-register-menu "00-lang-lib")
(declare-function ide-lang-fetch-extract "00-lang-lib")
(declare-function ide-lang-github-asset-url "00-lang-lib")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-child "treesit")
(declare-function treesit-node-child-by-field-name "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")

(defgroup ide-web nil "Web development support." :group 'ide-lang)

(defcustom ide-web-preview-port 8765
  "Port of the local preview server (simple-httpd, bound to localhost only)."
  :type 'integer)

(defcustom ide-web-reload-on-save t
  "Reload a static preview shown in the in-Emacs browser when a web file is saved."
  :type 'boolean)

(defconst ide-web-modes
  '(typescript-ts-mode tsx-ts-mode js-ts-mode css-ts-mode html-ts-mode)
  "Major modes this module owns.")

(defconst ide-web-script-modes '(typescript-ts-mode tsx-ts-mode js-ts-mode)
  "The TypeScript and JavaScript modes.")

;;;; Modes and servers -----------------------------------------------------------------------

(dolist (entry '(("\\.[cm]?ts\\'" . typescript-ts-mode)
                 ("\\.tsx\\'" . tsx-ts-mode)
                 ("\\.[cm]?js\\'" . js-ts-mode)
                 ("\\.jsx\\'" . js-ts-mode)
                 ("\\.css\\'" . css-ts-mode)
                 ("\\.html?\\'" . html-ts-mode)))
  (add-to-list 'auto-mode-alist entry))

;; js-ts-mode and typescript-ts-mode embed the jsdoc grammar in comments and ask to
;; install it when it is missing; `ide-treesit-install-grammars' then fetches it.
(defvar treesit-language-source-alist)
(with-eval-after-load 'treesit
  (unless (assq 'jsdoc treesit-language-source-alist)
    (push '(jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc") treesit-language-source-alist)))

(defvar ide-web--tsc-native-cache (make-hash-table :test #'equal)
  "Executable path -> whether it is the native (TypeScript 7+) compiler.")

(defun ide-web--tsc-native-p (exe)
  "Non-nil when EXE is a TypeScript 7+ `tsc', which speaks LSP itself."
  (and exe
       (let ((hit (gethash exe ide-web--tsc-native-cache 'unset)))
         (if (not (eq hit 'unset)) hit
           (puthash exe
                    (with-temp-buffer
                      (and (zerop (or (ignore-errors (call-process exe nil '(t nil) nil "--version")) 1))
                           (progn (goto-char (point-min))
                                  (re-search-forward "Version \\([0-9]+\\)" nil t))
                           (>= (string-to-number (match-string 1)) 7)))
                    ide-web--tsc-native-cache)))))

(defun ide-web--ts-server (&rest _)
  "Command line of the TypeScript/JavaScript language server.
TypeScript 7 ships its own server (`tsc --lsp --stdio'); typescript-language-server
cannot drive it (it needs tsserver.js, which 7 does not have), so it is used only when
no 7+ compiler is found, for projects that bring TypeScript 5 or 6."
  (let ((tsc (ide-lang-node-exe "tsc"))
        (tsls (ide-lang-node-exe "typescript-language-server")))
    (cond ((ide-web--tsc-native-p tsc) (list tsc "--lsp" "--stdio"))
          (tsls (list tsls "--stdio"))
          (t (user-error "No TypeScript language server; run M-x ide-mise-setup")))))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(((js-ts-mode :language-id "javascript")
                  (tsx-ts-mode :language-id "typescriptreact")
                  (typescript-ts-mode :language-id "typescript"))
                 . ide-web--ts-server))
  (add-to-list 'eglot-server-programs
               '(css-ts-mode . ("vscode-css-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(html-ts-mode . ("vscode-html-language-server" "--stdio"))))

(defun ide-web--setup ()
  "Buffer settings shared by every web mode: 2 spaces, prettier owns formatting."
  (setq-local tab-width 2 indent-tabs-mode nil
              js-indent-level 2 typescript-ts-indent-offset 2
              css-indent-offset 2 html-ts-indent-offset 2
              ide-eglot-format-on-save nil
              ;; so `ide-reformat-buffer' falls through to Apheleia (prettier)
              eglot-ignored-server-capabilities
              '(:documentFormattingProvider :documentRangeFormattingProvider
                :documentOnTypeFormattingProvider))
  (add-hook 'after-save-hook #'ide-web--reload-preview nil t)
  (when (fboundp 'apheleia-mode) (apheleia-mode 1))
  (eglot-ensure))

(dolist (mode ide-web-modes)
  (add-hook (intern (format "%s-hook" mode)) #'ide-web--setup))

;; TypeScript 7 returns inlay hints and diagnostics on its own; nothing to configure
;; beyond the defaults, but keep parameter-name hints quiet unless they add information.
;; vscode-css-language-server ignores unknown properties unless told otherwise.
(ide-lang-workspace-config
 :css '(:validate t :lint (:unknownProperties "warning" :duplicateProperties "warning"
                           :emptyRules "warning")))

(ide-lang-workspace-config
 :typescript '(:inlayHints (:parameterNames (:enabled "literals")
                            :variableTypes (:enabled :json-false)
                            :functionLikeReturnTypes (:enabled t))))

;;;; Formatting: prettier ------------------------------------------------------------------------

(with-eval-after-load 'apheleia
  ;; apheleia-npx runs the project's node_modules/.bin/prettier first, else the
  ;; installed one, and never installs anything.  No indent flags: .prettierrc and
  ;; .editorconfig decide, prettier's own default is 2 spaces.
  (setf (alist-get 'ide-prettier apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath))
  (dolist (mode ide-web-modes)
    (setf (alist-get mode apheleia-mode-alist) 'ide-prettier)))

;;;; Project helpers ----------------------------------------------------------------------------------

(defun ide-web--project-dir ()
  "Directory of the nearest package.json above the buffer, else the project root."
  (expand-file-name (or (locate-dominating-file (or buffer-file-name default-directory) "package.json")
                        (ide-lang-root))))

(defun ide-web--package-json (&optional dir)
  "Parsed package.json of DIR (default: the nearest one) as an alist, or nil."
  (let ((f (expand-file-name "package.json" (or dir (ide-web--project-dir)))))
    (when (file-readable-p f)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents f)
          (json-parse-buffer :object-type 'alist :array-type 'list))))))

(defun ide-web--scripts (&optional dir)
  "Alist (NAME . COMMAND) of the package.json scripts."
  (cl-loop for (k . v) in (alist-get 'scripts (ide-web--package-json dir))
           collect (cons (symbol-name k) v)))

(defun ide-web--dependencies (&optional dir)
  "Names of all dependencies and devDependencies in package.json."
  (let ((pkg (ide-web--package-json dir)))
    (cl-loop for key in '(dependencies devDependencies)
             append (mapcar (lambda (c) (symbol-name (car c))) (alist-get key pkg)))))

(defun ide-web--package-manager (&optional dir)
  "The package manager of the project at DIR: pnpm, yarn, bun or npm.
Decided by the nearest lockfile, then by the `packageManager' field."
  (let ((d (expand-file-name (or dir (ide-web--project-dir)))) found)
    (while (and d (not found))
      (setq found
            (cond ((file-exists-p (expand-file-name "pnpm-lock.yaml" d)) "pnpm")
                  ((file-exists-p (expand-file-name "yarn.lock" d)) "yarn")
                  ((or (file-exists-p (expand-file-name "bun.lockb" d))
                       (file-exists-p (expand-file-name "bun.lock" d))) "bun")
                  ((file-exists-p (expand-file-name "package-lock.json" d)) "npm")))
      (let ((parent (file-name-directory (directory-file-name d))))
        (setq d (and (not found) (not (equal parent d)) parent))))
    (or found
        (when-let* ((field (alist-get 'packageManager (ide-web--package-json dir)))
                    ((string-match "\\`\\(pnpm\\|yarn\\|bun\\|npm\\)@" field)))
          (match-string 1 field))
        "npm")))

(defun ide-web--env ()
  "Environment entries that keep package managers and tsx inside the sandbox."
  (let ((cache (ide-lang-tools-dir "web" "cache"))
        (tmp (ide-lang-tools-dir "web" "tmp")))
    (make-directory cache t)
    (make-directory tmp t)
    (append (list (concat "npm_config_cache=" (expand-file-name "npm" cache))
                  (concat "YARN_CACHE_FOLDER=" (expand-file-name "yarn" cache))
                  (concat "BUN_INSTALL_CACHE_DIR=" (expand-file-name "bun" cache))
                  (concat "XDG_CACHE_HOME=" cache)
                  (concat "XDG_DATA_HOME=" (expand-file-name "data" cache)) ; vitest keeps a token here
                  (concat "XDG_STATE_HOME=" (expand-file-name "state" cache))
                  (concat "TMPDIR=" tmp))
            ;; npm warns about this key, so only pnpm projects get it
            (when (equal (ide-web--package-manager) "pnpm")
              (list (concat "npm_config_store_dir=" (expand-file-name "pnpm" cache)))))))

(defun ide-web--tmpdir-env ()
  "The TMPDIR=... entry of `ide-web--env' (tsx keeps its cache there)."
  (cl-find-if (lambda (e) (string-prefix-p "TMPDIR=" e)) (ide-web--env)))

(defun ide-web--run (name command &optional dir comint)
  "Run COMMAND in the drawer as *run: NAME* with the sandbox environment."
  (let ((process-environment (append (ide-web--env) process-environment)))
    (ide-lang-run name command (or dir (ide-web--project-dir)) comint)))

(defun ide-web--exec (tool &rest args)
  "Command list running TOOL (a project binary) with ARGS: local binary, else the package manager."
  (if-let* ((exe (ide-lang-node-exe tool)))
      (cons exe args)
    (pcase (ide-web--package-manager)
      ("pnpm" (append (list "pnpm" "exec" tool) args))
      ("yarn" (append (list "yarn" tool) args))
      ("bun" (append (list "bunx" tool) args))
      (_ (append (list "npx" "--no-install" tool) args)))))

;;;; Package scripts and dependencies ---------------------------------------------------------------------

(defun ide-web-run-script (script)
  "Run the package.json SCRIPT chosen from a list, with the project's package manager."
  (interactive
   (let ((scripts (ide-web--scripts)))
     (unless scripts (user-error "No scripts in package.json"))
     (list (completing-read
            "Script: "
            (lambda (str pred action)
              (if (eq action 'metadata)
                  `(metadata (annotation-function
                              . ,(lambda (s) (format "  %s" (cdr (assoc s scripts))))))
                (complete-with-action action scripts str pred)))
            nil t))))
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s run %s" pm script) (list pm "run" script))))

(defun ide-web-install ()
  "Install the dependencies (npm/pnpm/yarn/bun install)."
  (interactive)
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s install" pm) (list pm "install"))))

(defun ide-web--add-command (pm packages dev)
  "Command list adding PACKAGES with PM (DEV: as development dependencies)."
  (append (pcase pm
            ("npm" (list "npm" "install" (and dev "--save-dev")))
            ("bun" (list "bun" "add" (and dev "--dev")))
            (_ (list pm "add" (and dev "-D"))))
          packages))

(defun ide-web-add (packages &optional dev)
  "Add PACKAGES (space separated, e.g. react@18 zod); with a prefix argument as devDependencies."
  (interactive (list (read-string (if current-prefix-arg "Add dev dependency: " "Add dependency: "))
                     current-prefix-arg))
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s add" pm)
                  (ide-web--add-command pm (split-string packages) dev))))

(defun ide-web-remove (package)
  "Remove the dependency PACKAGE, chosen from package.json."
  (interactive (list (completing-read "Remove: " (or (ide-web--dependencies)
                                                     (user-error "No dependencies")) nil t)))
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s remove" pm)
                  (list pm (if (equal pm "npm") "uninstall" "remove") package))))

(defun ide-web-outdated ()
  "List outdated dependencies."
  (interactive)
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s outdated" pm) (list pm "outdated"))))

(defun ide-web-audit ()
  "Audit the dependencies for known vulnerabilities."
  (interactive)
  (let ((pm (ide-web--package-manager)))
    (ide-web--run (format "%s audit" pm) (list pm "audit"))))

(defun ide-web-open-package-json ()
  "Open the nearest package.json."
  (interactive)
  (find-file (expand-file-name "package.json" (or (locate-dominating-file default-directory "package.json")
                                                  (user-error "No package.json")))))

(defun ide-web--dev-script ()
  "Name of the script that starts a dev server: dev, start, serve, ..., or nil."
  (let ((names (mapcar #'car (ide-web--scripts))))
    (cl-find-if (lambda (n) (member n names)) '("dev" "start" "serve" "develop" "preview"))))

(defun ide-web-dev-server ()
  "Start the dev server script (dev, start, serve) in the drawer."
  (interactive)
  (let ((script (or (ide-web--dev-script) (user-error "No dev/start/serve script in package.json")))
        (pm (ide-web--package-manager)))
    (ide-web--run "dev server" (list pm "run" script))))

(defun ide-web-dev-server-open ()
  "Open the running dev server in the in-Emacs browser (URL read from its output)."
  (interactive)
  (let* ((buf (get-buffer "*run: dev server*"))
         (url (when buf
                (with-current-buffer buf
                  (save-excursion
                    (goto-char (point-min))
                    (when (re-search-forward "https?://\\(?:localhost\\|127\\.0\\.0\\.1\\)[:0-9]*/?" nil t)
                      (match-string 0)))))))
    (ide-lang-browse (or url (read-string "URL: " "http://localhost:5173/")))))

(defun ide-web-typecheck ()
  "Type-check the project (tsc --noEmit); errors in the drawer are clickable."
  (interactive)
  (let ((dir (or (locate-dominating-file (or buffer-file-name default-directory) "tsconfig.json")
                 (user-error "No tsconfig.json"))))
    (ide-web--run "tsc" (append (list (or (ide-lang-node-exe "tsc") (user-error "tsc not found")))
                                '("--noEmit" "--pretty" "false"))
                  dir)))

(defun ide-web-typecheck-watch ()
  "Type-check continuously (tsc --noEmit --watch)."
  (interactive)
  (let ((dir (or (locate-dominating-file (or buffer-file-name default-directory) "tsconfig.json")
                 (user-error "No tsconfig.json"))))
    (ide-web--run "tsc watch" (list (or (ide-lang-node-exe "tsc") (user-error "tsc not found"))
                                    "--noEmit" "--watch" "--preserveWatchOutput" "--pretty" "false")
                  dir)))

;;;; ESLint ------------------------------------------------------------------------------------------------

(defconst ide-web--eslint-configs
  '("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"
    "eslint.config.ts" "eslint.config.mts" "eslint.config.cts"
    ".eslintrc" ".eslintrc.js" ".eslintrc.cjs" ".eslintrc.json" ".eslintrc.yaml" ".eslintrc.yml")
  "File names that make ESLint relevant to a project.")

(defun ide-web--eslint-config-p ()
  "Non-nil when an ESLint configuration exists above the buffer's file."
  (locate-dominating-file
   (or buffer-file-name default-directory)
   (lambda (dir) (cl-some (lambda (n) (file-exists-p (expand-file-name n dir)))
                          ide-web--eslint-configs))))

(defun ide-web--eslint-exe ()
  "The project's eslint (else the installed one), only where ESLint is configured."
  (when (ide-web--eslint-config-p) (ide-lang-node-exe "eslint")))

(defun ide-web--eslint-parse (output buffer)
  "Flymake diagnostics for ESLint's JSON OUTPUT about BUFFER."
  (let ((start (string-match "^\\[" output)))
    (when start
      (let ((results (json-parse-string (substring output start)
                                        :object-type 'alist :array-type 'list :null-object nil)))
        (cl-loop for res in results
                 append (cl-loop for m in (alist-get 'messages res)
                                 for line = (alist-get 'line m)
                                 for msg = (alist-get 'message m)
                                 unless (or (null line) (string-prefix-p "File ignored" msg))
                                 collect (ide-lang-diag
                                          buffer "eslint" line (alist-get 'column m)
                                          (if (eql (alist-get 'severity m) 2) :error :warning)
                                          (if-let* ((rule (alist-get 'ruleId m)))
                                              (format "%s (%s)" msg rule) msg)
                                          (and (eql (alist-get 'endLine m) line)
                                               (alist-get 'endColumn m)))))))))

(ide-lang-define-linter
 'eslint
 :modes '(typescript-ts-mode tsx-ts-mode js-ts-mode)
 :program #'ide-web--eslint-exe
 :args (lambda (file)
         (list "--format" "json" "--stdin" "--stdin-filename" (or file "stdin.ts")))
 :parse #'ide-web--eslint-parse)

(defun ide-web-eslint-fix ()
  "Apply ESLint's automatic fixes to the current file."
  (interactive)
  (let ((exe (or (ide-lang-node-exe "eslint") (user-error "eslint not found")))
        (buf (current-buffer)))
    (unless buffer-file-name (user-error "Buffer has no file"))
    (save-buffer)
    (ide-lang-run-then "eslint --fix" (list exe "--fix" buffer-file-name)
                       (lambda () (when (buffer-live-p buf)
                                    (with-current-buffer buf (revert-buffer t t t))))
                       (ide-web--project-dir))))

;;;; Emmet ----------------------------------------------------------------------------------------------------

(use-package emmet-mode
  :commands emmet-mode
  :hook ((html-ts-mode css-ts-mode tsx-ts-mode js-ts-mode) . emmet-mode)
  :init (setq emmet-move-cursor-between-quotes t)
  :config
  (dolist (m '(tsx-ts-mode js-ts-mode typescript-ts-mode))
    (add-to-list 'emmet-jsx-major-modes m)))

;;;; Running --------------------------------------------------------------------------------------------------------

(defconst ide-web--test-file-regexp "\\.\\(?:test\\|spec\\)\\.[cm]?[jt]sx?\\'"
  "File names that hold tests.")

(defun ide-web--test-file-p (&optional file)
  (string-match-p ide-web--test-file-regexp (or file buffer-file-name "")))

(defun ide-web--run-file-command (file)
  "The shell command that runs FILE: tsx for TypeScript, node for JavaScript."
  (let ((ts (string-match-p "\\.[cm]?tsx?\\'" file)))
    (if ts
        (format "%s %s" (shell-quote-argument (or (ide-lang-node-exe "tsx") "tsx"))
                (shell-quote-argument file))
      (format "node %s" (shell-quote-argument file)))))

(defun ide-web--run-command ()
  "Hook for `ide-run-command-functions': run the file, or its tests."
  (when (and buffer-file-name (apply #'derived-mode-p ide-web-script-modes))
    (if (and (ide-web--test-file-p) (ide-web--test-runner))
        (mapconcat #'shell-quote-argument (ide-web--test-command :file buffer-file-name) " ")
      (format "env %s %s" (shell-quote-argument (ide-web--tmpdir-env))
              (ide-web--run-file-command buffer-file-name)))))

(add-hook 'ide-run-command-functions #'ide-web--run-command)

;;;; Tests: vitest and jest ----------------------------------------------------------------------------------------

(defun ide-web--test-runner ()
  "The test runner of the project: `vitest', `jest', or nil."
  (let* ((pkg (ide-web--package-json))
         (deps (ide-web--dependencies))
         (test-script (or (cdr (assoc "test" (ide-web--scripts))) "")))
    (cond ((or (member "vitest" deps) (string-match-p "\\bvitest\\b" test-script)) 'vitest)
          ((or (member "jest" deps) (string-match-p "\\bjest\\b" test-script)
               (alist-get 'jest pkg))
           'jest))))

(defun ide-web--test-command (&rest plist)
  "Command list running tests.  PLIST keys: :file :name :watch.
FILE is made relative to the package directory."
  (let* ((runner (or (ide-web--test-runner) (user-error "No vitest or jest in package.json")))
         (file (plist-get plist :file))
         (rel (and file (file-relative-name file (ide-web--project-dir))))
         (name (plist-get plist :name))
         (watch (plist-get plist :watch)))
    (append (apply #'ide-web--exec (symbol-name runner)
                   (pcase runner
                     ('vitest (unless watch (list "run")))
                     ('jest (when watch (list "--watch")))))
            (and rel (list rel))
            (and name (list "-t" name)))))

(defun ide-web--regexp-quote (s)
  "Escape S for a JavaScript regular expression (used by -t)."
  (replace-regexp-in-string "[][\\.*+?^${}()|/]" "\\\\\\&" s))

(defconst ide-web--test-callees '("it" "test" "describe" "suite" "bench")
  "Functions whose first argument titles a test block.")

(defun ide-web--call-title (call)
  "Title string of the CALL expression node when it is `it(\"x\", ...)' and friends, else nil."
  (let* ((fn (treesit-node-child-by-field-name call "function"))
         (head (and fn (if (equal (treesit-node-type fn) "member_expression")
                           (treesit-node-child-by-field-name fn "object")
                         fn)))
         (args (treesit-node-child-by-field-name call "arguments"))
         (first (and args (treesit-node-child args 0 t))))
    (when (and head (equal (treesit-node-type head) "identifier")
               (member (treesit-node-text head t) ide-web--test-callees)
               first (member (treesit-node-type first) '("string" "template_string")))
      (let ((text (treesit-node-text first t)))
        (substring text 1 (1- (length text)))))))

(defun ide-web--test-title-at-point ()
  "Title of the innermost it/test/describe block around point, or nil."
  (let ((node (treesit-node-at (point))) title)
    (while (and node (not title))
      (when (equal (treesit-node-type node) "call_expression")
        (setq title (ide-web--call-title node)))
      (setq node (treesit-node-parent node)))
    title))

(defun ide-web-test-at-point ()
  "Run the test (or describe block) at point."
  (interactive)
  (let ((title (or (ide-web--test-title-at-point) (user-error "No test at point"))))
    (ide-web--run "test" (ide-web--test-command :file buffer-file-name
                                                :name (ide-web--regexp-quote title)))))

(defun ide-web-test-file ()
  "Run the tests of the current file (a source file runs its test counterpart)."
  (interactive)
  (let ((file (if (ide-web--test-file-p) buffer-file-name
                (or (ide-web--existing-counterpart) (user-error "No test file for this file")))))
    (ide-web--run "test" (ide-web--test-command :file file))))

(defun ide-web-test-all ()
  "Run every test in the project."
  (interactive)
  (ide-web--run "test" (ide-web--test-command)))

(defun ide-web-test-watch ()
  "Run the tests in watch mode (rerun on change) for the current file."
  (interactive)
  (ide-web--run "test watch"
                (ide-web--test-command :watch t
                                       :file (if (ide-web--test-file-p) buffer-file-name
                                               (ide-web--existing-counterpart)))
                nil t))

;;;; Test / source counterpart ----------------------------------------------------------------------------------------

(defun ide-web--counterpart-candidates (file)
  "Likely counterparts of FILE: tests for a source file, sources for a test file."
  (let* ((dir (file-name-directory file))
         (name (file-name-nondirectory file))
         (exts '("ts" "tsx" "js" "jsx" "mts" "mjs")))
    (if (string-match "\\`\\(.*\\)\\.\\(?:test\\|spec\\)\\.\\([cm]?[jt]sx?\\)\\'" name)
        (let* ((base (match-string 1 name))
               (up (file-name-directory (directory-file-name dir)))
               (dirs (list dir (and (equal (file-name-nondirectory (directory-file-name dir)) "__tests__") up)
                           (and (equal (file-name-nondirectory (directory-file-name dir)) "__tests__")
                                (expand-file-name "src/" up)))))
          (cl-loop for d in (delq nil dirs)
                   append (mapcar (lambda (e) (expand-file-name (concat base "." e) d)) exts)))
      (let ((base (file-name-sans-extension name)) (ext (file-name-extension name)))
        (append
         (cl-loop for kind in '("test" "spec")
                  append (mapcar (lambda (e) (expand-file-name (format "%s.%s.%s" base kind e) dir))
                                 (cons ext (remove ext exts))))
         (cl-loop for e in (cons ext (remove ext exts))
                  collect (expand-file-name (format "__tests__/%s.test.%s" base e) dir)))))))

(defun ide-web--existing-counterpart ()
  "The existing test/source counterpart of the current file, or nil."
  (and buffer-file-name
       (cl-find-if #'file-exists-p (ide-web--counterpart-candidates buffer-file-name))))

(defun ide-web-toggle-test ()
  "Jump between a source file and its test file (create the test file when missing)."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (if-let* ((other (ide-web--existing-counterpart)))
      (find-file other)
    (if (ide-web--test-file-p)
        (user-error "No source file found for this test")
      (let ((new (car (ide-web--counterpart-candidates buffer-file-name))))
        (when (y-or-n-p (format "Create %s? " (file-name-nondirectory new)))
          (find-file new))))))

;;;; Code actions -------------------------------------------------------------------------------------------------------------

(defun ide-web--code-action (kind)
  "Apply the LSP source action KIND (for example \"source.organizeImports\") to the buffer."
  (unless (bound-and-true-p eglot--managed-mode) (user-error "Eglot is not running in this buffer"))
  (eglot-code-actions nil nil kind t))

(defun ide-web-organize-imports ()
  "Sort and merge the imports."
  (interactive) (ide-web--code-action "source.organizeImports"))

(defun ide-web-remove-unused-imports ()
  "Delete the imports that nothing uses."
  (interactive) (ide-web--code-action "source.removeUnusedImports"))

(defun ide-web-fix-all ()
  "Apply every automatic fix the language server offers."
  (interactive) (ide-web--code-action "source.fixAll"))

(defun ide-web-format ()
  "Format the buffer with prettier."
  (interactive) (apheleia-format-buffer 'ide-prettier))

;;;; Documentation -----------------------------------------------------------------------------------------------------------------

(defun ide-web--symbol-at-point ()
  "The symbol at point (CSS properties and HTML tags keep their hyphens), or nil."
  (let ((s (thing-at-point 'symbol t)))
    (and s (not (string-empty-p s)) (substring-no-properties s))))

(defun ide-web-docs-at-point (&optional query)
  "Search MDN for the symbol at point (QUERY overrides it) in the in-Emacs browser."
  (interactive (list (when current-prefix-arg (read-string "MDN: "))))
  (let ((q (or query (ide-web--symbol-at-point) (read-string "MDN: "))))
    (ide-lang-browse (format "https://developer.mozilla.org/en-US/search?q=%s" (url-hexify-string q)))))

(defun ide-web-docs-typescript (&optional query)
  "Search the TypeScript documentation for the symbol at point (QUERY overrides it)."
  (interactive (list (when current-prefix-arg (read-string "TypeScript docs: "))))
  (let ((q (or query (ide-web--symbol-at-point) (read-string "TypeScript docs: "))))
    (ide-lang-browse (format "https://www.typescriptlang.org/search?q=%s" (url-hexify-string q)))))

(defun ide-web--package-at-point ()
  "The npm package named by the import string at point, or nil."
  (let ((s (thing-at-point 'filename t)))
    (when (and s (string-match "\\`\\(@[^/\"']+/[^/\"']+\\|[^./\"'@][^/\"']*\\)" s))
      (match-string 1 s))))

(defun ide-web-docs-npm ()
  "Open the npm page of the package imported at point (or one chosen from package.json)."
  (interactive)
  (let ((pkg (or (ide-web--package-at-point)
                 (completing-read "Package: " (ide-web--dependencies)))))
    (ide-lang-browse (format "https://www.npmjs.com/package/%s" pkg))))

;;;; REPL ---------------------------------------------------------------------------------------------------------------------------------

(use-package nodejs-repl
  :commands (nodejs-repl nodejs-repl-send-region nodejs-repl-send-buffer nodejs-repl-switch-to-repl))

(defun ide-web-repl ()
  "Open a Node REPL in a window beside the code."
  (interactive)
  (let ((process-environment (append (ide-web--env) process-environment)))
    (nodejs-repl)))

(defun ide-web-repl-send-region (beg end)
  "Send the region, else the statement or function at point, to the Node REPL."
  (interactive
   (if (use-region-p) (list (region-beginning) (region-end))
     (if-let* ((n (ide-lang-ancestor '("function_declaration" "lexical_declaration" "class_declaration"
                                       "export_statement" "expression_statement" "variable_declaration"
                                       "if_statement" "for_statement" "for_in_statement"))))
         (list (treesit-node-start n) (treesit-node-end n))
       (list (line-beginning-position) (line-end-position)))))
  (unless (get-process nodejs-repl-process-name) (save-current-buffer (ide-web-repl)))
  (nodejs-repl-send-region beg end))

(defun ide-web-repl-send-buffer ()
  "Send the whole buffer to the Node REPL."
  (interactive)
  (unless (get-process nodejs-repl-process-name) (save-current-buffer (ide-web-repl)))
  (nodejs-repl-send-buffer))

;;;; Debugging: vscode-js-debug through dap-mode --------------------------------------------------------------------------

(defun ide-web-install-debugger ()
  "Download the vscode-js-debug adapter into the sandbox (dap-js's own installer needs lsp-mode)."
  (interactive)
  (require 'dap-js)
  (ide-lang-fetch-extract
   "js-debug"
   (ide-lang-github-asset-url "microsoft/vscode-js-debug" "\\`js-debug-dap-.*\\.tar\\.gz\\'")
   ;; the archive holds a js-debug/ directory, which is what `dap-js-path' names
   (file-name-directory (directory-file-name dap-js-path))))

(defun ide-web--debugger-ready ()
  (require 'dap-mode)
  (require 'dap-js)
  (unless (file-exists-p (expand-file-name "src/dapDebugServer.js" dap-js-path))
    (user-error "The debugger is not installed; run M-x ide-web-install-debugger")))

(defun ide-web--launch (name &rest plist)
  "A `dap-debug' launch plist for vscode-js-debug named NAME, with PLIST merged in."
  (append (list :type "pwa-node" :request "launch" :name name
                :cwd (ide-web--project-dir)
                :skipFiles ["<node_internals>/**"]
                :env (cl-loop for e in (ide-web--env)
                              for i = (string-search "=" e)
                              append (list (intern (concat ":" (substring e 0 i)))
                                           (substring e (1+ i)))))
          plist))

(defun ide-web--file-launch (file)
  "Launch plist that debugs FILE: through tsx for TypeScript, plain node otherwise."
  (if (string-match-p "\\.[cm]?tsx?\\'" file)
      (ide-web--launch (format "tsx: %s" (file-name-nondirectory file))
                       :program file
                       :runtimeExecutable (or (ide-lang-node-exe "tsx") "tsx"))
    (ide-web--launch (format "node: %s" (file-name-nondirectory file)) :program file)))

(defun ide-web--script-launch (script)
  "Launch plist that debugs the package.json SCRIPT."
  (ide-web--launch (format "%s run %s" (ide-web--package-manager) script)
                   :runtimeExecutable (ide-web--package-manager)
                   :runtimeArgs (vector "run" script)))

(defun ide-web--test-launch (file name)
  "Launch plist that debugs the tests of FILE (those matching NAME, when given)."
  (let* ((cmd (ide-web--test-command :file file :name name))
         (exe (car cmd)))
    (ide-web--launch (format "test: %s" (file-name-nondirectory file))
                     :runtimeExecutable exe
                     :runtimeArgs (vconcat (cdr cmd)))))

(defun ide-web-debug-file ()
  "Debug the current file (tsx for TypeScript, node for JavaScript)."
  (interactive)
  (ide-web--debugger-ready)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (dap-debug (ide-web--file-launch buffer-file-name)))

(defun ide-web-debug-script (script)
  "Debug the package.json SCRIPT (the debugger attaches to every node process it starts)."
  (interactive (list (completing-read "Debug script: "
                                      (or (mapcar #'car (ide-web--scripts)) (user-error "No scripts")) nil t)))
  (ide-web--debugger-ready)
  (dap-debug (ide-web--script-launch script)))

(defun ide-web-debug-test ()
  "Debug the test at point (or the file's tests) with vitest or jest."
  (interactive)
  (ide-web--debugger-ready)
  (let ((file (if (ide-web--test-file-p) buffer-file-name
                (or (ide-web--existing-counterpart) (user-error "No test file")))))
    (dap-debug (ide-web--test-launch
                file (and (ide-web--test-file-p) (ide-web--test-title-at-point)
                          (ide-web--regexp-quote (ide-web--test-title-at-point)))))))

(defun ide-web--debug-hook ()
  "Hook for `ide-debug-functions': debug the test file or the script."
  (when (and buffer-file-name (apply #'derived-mode-p ide-web-script-modes))
    (if (and (ide-web--test-file-p) (ide-web--test-runner))
        (ide-web-debug-test)
      (ide-web-debug-file))
    t))

(add-hook 'ide-debug-functions #'ide-web--debug-hook)

;;;; Live preview: simple-httpd + impatient-mode ------------------------------------------------------------------------------

(use-package simple-httpd :commands (httpd-start httpd-stop httpd-running-p))
(use-package impatient-mode
  :commands (impatient-mode imp-visit-buffer)
  :config
  ;; a nil filter serves the buffer as it is, instead of as highlighted source
  (dolist (m '(html-ts-mode css-ts-mode js-ts-mode))
    (add-to-list 'imp-default-user-filters (cons m nil))))

(defun ide-web--base-url ()
  (format "http://localhost:%d/" ide-web-preview-port))

(defun ide-web--serve (root)
  "Serve ROOT on localhost at `ide-web-preview-port' (restarting if the root changed)."
  (require 'simple-httpd)
  (unless (and (httpd-running-p) (equal (expand-file-name root) (expand-file-name httpd-root)))
    (when (httpd-running-p) (httpd-stop))
    (setq httpd-root (expand-file-name root))
    (let ((httpd-port ide-web-preview-port) (httpd-host 'local))
      (httpd-start))))

(defun ide-web-serve ()
  "Serve the project directory over HTTP on localhost."
  (interactive)
  (ide-web--serve (ide-lang-root))
  (message "Serving %s at %s" (ide-lang-root) (ide-web--base-url)))

(defun ide-web-stop-server ()
  "Stop the preview server."
  (interactive)
  (when (and (featurep 'simple-httpd) (httpd-running-p)) (httpd-stop))
  (message "Preview server stopped"))

(defun ide-web--static-url (file)
  "URL of FILE under the served project root."
  (concat (ide-web--base-url)
          (mapconcat #'url-hexify-string
                     (split-string (file-relative-name file (ide-lang-root)) "/") "/")))

(defun ide-web-preview ()
  "Open the current HTML file served from the project in the in-Emacs browser.
Saving reloads it (see `ide-web-reload-on-save')."
  (interactive)
  (unless buffer-file-name (user-error "Buffer has no file"))
  (ide-web--serve (ide-lang-root))
  (ide-lang-browse (ide-web--static-url buffer-file-name)))

(defun ide-web--live-buffers ()
  "Open web buffers of this project that should follow edits live (html, css, js)."
  (let ((root (ide-lang-root)))
    (cl-loop for b in (buffer-list)
             when (and (buffer-file-name b)
                       (memq (buffer-local-value 'major-mode b) '(html-ts-mode css-ts-mode js-ts-mode))
                       (file-in-directory-p (buffer-file-name b) root))
             collect b)))

(defun ide-web-preview-live ()
  "Preview the current HTML buffer live: the page updates as you type, unsaved.
The project's open CSS and JS buffers are updated live too."
  (interactive)
  (require 'impatient-mode)
  (ide-web--serve (ide-lang-root))
  (dolist (b (ide-web--live-buffers))
    (with-current-buffer b (unless (bound-and-true-p impatient-mode) (impatient-mode 1))))
  (unless (bound-and-true-p impatient-mode) (impatient-mode 1))
  (ide-lang-browse (format "%simp/live/%s/" (ide-web--base-url) (url-hexify-string (buffer-name)))))

(defun ide-web-preview-live-stop ()
  "Turn live preview off in every web buffer."
  (interactive)
  (dolist (b (ide-web--live-buffers))
    (with-current-buffer b (when (bound-and-true-p impatient-mode) (impatient-mode -1)))))

(defun ide-web--preview-buffers ()
  "Browser buffers showing a static page of the preview server."
  (when (fboundp 'xwidget-webkit-uri)
    (cl-loop for b in (buffer-list)
             when (and (eq (buffer-local-value 'major-mode b) 'xwidget-webkit-mode)
                       (let ((uri (with-current-buffer b (ignore-errors (xwidget-webkit-uri (xwidget-webkit-current-session))))))
                         (and uri (string-prefix-p (ide-web--base-url) uri)
                              (not (string-match-p "/imp/live/" uri)))))
             collect b)))

(defun ide-web--reload-preview ()
  "After-save hook: reload the static preview pages in the in-Emacs browser."
  (when (and ide-web-reload-on-save (featurep 'simple-httpd) (httpd-running-p)
             (not (bound-and-true-p impatient-mode)))
    (dolist (b (ide-web--preview-buffers))
      (with-current-buffer b (xwidget-webkit-reload)))))

;;;; Menu ---------------------------------------------------------------------------------------------------------------------------------------

;;;###autoload (autoload 'ide-web-dispatch "lang-web" nil t)
(transient-define-prefix ide-web-dispatch ()
  "Web: scripts, tests, debug, preview, explore."
  [["Run"
    ("r" "package script..." ide-web-run-script)
    ("v" "dev server" ide-web-dev-server)
    ("V" "open dev server" ide-web-dev-server-open)
    ("c" "type-check (tsc)" ide-web-typecheck)
    ("C" "type-check watch" ide-web-typecheck-watch)
    ("l" "eslint --fix" ide-web-eslint-fix)
    ("f" "format (prettier)" ide-web-format)]
   ["Test"
    ("t" "test at point" ide-web-test-at-point)
    ("F" "tests in file" ide-web-test-file)
    ("a" "all tests" ide-web-test-all)
    ("w" "watch" ide-web-test-watch)
    ("T" "test <-> source" ide-web-toggle-test)]
   ["Debug / REPL"
    ("d" "debug file" ide-web-debug-file)
    ("D" "debug test" ide-web-debug-test)
    ("s" "debug script..." ide-web-debug-script)
    ("I" "install debugger" ide-web-install-debugger)
    ("e" "Node REPL" ide-web-repl)
    ("x" "send to REPL" ide-web-repl-send-region)
    ("X" "send buffer" ide-web-repl-send-buffer)]
   ["Packages"
    ("i" "install" ide-web-install)
    ("+" "add" ide-web-add)
    ("-" "remove" ide-web-remove)
    ("u" "outdated" ide-web-outdated)
    ("A" "audit" ide-web-audit)
    ("j" "package.json" ide-web-open-package-json)]
   ["Preview"
    ("p" "preview file" ide-web-preview)
    ("P" "live preview" ide-web-preview-live)
    ("S" "serve project" ide-web-serve)
    ("q" "stop server" ide-web-stop-server)]
   ["Code / docs"
    ("o" "organize imports" ide-web-organize-imports)
    ("U" "remove unused imports" ide-web-remove-unused-imports)
    ("z" "fix all" ide-web-fix-all)
    ("m" "MDN docs" ide-web-docs-at-point)
    ("n" "TypeScript docs" ide-web-docs-typescript)
    ("N" "npm package docs" ide-web-docs-npm)
    ("R" "restart server" eglot-reconnect)]])

(ide-lang-register-menu ide-web-modes #'ide-web-dispatch)

(provide 'lang-web)
;;; lang-web.el ends here
