;;; lang-lua.el --- Lua: lua-language-server, stylua, REPL, busted, manual -*- lexical-binding: t; -*-

;;; Commentary:
;; Lua without leaving Emacs:
;;
;;   edit      lua-ts-mode + lua-language-server (diagnostics, hover, completion, rename);
;;             stylua formats on save through Apheleia, with 2 spaces unless the project
;;             has a stylua.toml / .stylua.toml, which then wins
;;   config    M-x ide-lua-write-luarc writes a .luarc.json (Neovim, LOVE, Hammerspoon,
;;             busted presets); M-x ide-lua-write-stylua-config writes a stylua.toml
;;   run       S-<f10> runs the file with the system lua (busted for *_spec.lua)
;;   REPL      lua -i in comint: send line, region, function, buffer
;;   test      busted at point (the `it' or `describe' around point), file, project;
;;             skipped with a message when busted is not on PATH
;;   explore   the Lua reference manual for the symbol at point in the in-Emacs browser,
;;             a whole-project LuaLS diagnostics report (lua-language-server --check)
;;
;; Not provided: a debugger.  local-lua-debugger-vscode has no release assets (it must
;; be built from source with npm), speaks DAP over stdio only (dap-mode needs a TCP
;; port) and does not support the system's Lua 5.5.
;; luacheck is not installed; LuaLS diagnostics cover it.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'transient)
(require 'comint)

(require '00-lang-lib)
(require 'treesit)

(defvar lua-ts-inferior-buffer)
(defvar lua-ts-luacheck-program)
(declare-function lua-ts-inferior-lua "lua-ts-mode")
(declare-function lua-ts-flymake-luacheck "lua-ts-mode")
(declare-function lua-ts-send-region "lua-ts-mode")
(declare-function lua-ts-send-buffer "lua-ts-mode")
(declare-function apheleia-format-buffer "apheleia")
(declare-function eglot-reconnect "eglot")

(defgroup ide-lua nil "Lua support." :group 'ide-lang)

(defcustom ide-lua-runtime-version "Lua 5.4"
  "The Lua version LuaLS assumes: \"Lua 5.1\" ... \"Lua 5.4\", \"LuaJIT\"."
  :type 'string)

(defcustom ide-lua-manual-version "5.4"
  "Which version of the Lua reference manual `ide-lua-manual' opens."
  :type 'string)

(defcustom ide-lua-indent-width 2
  "Indentation width, for Emacs and for stylua when the project has no config."
  :type 'integer)

;;;; Mode and server --------------------------------------------------------------------

(ide-lang-setup 'lua-ts-mode
  :server '("lua-language-server")
  :tab-width ide-lua-indent-width
  :indent-tabs nil
  :format-on-save t
  :config (lambda ()
            (unless (executable-find lua-ts-luacheck-program)
              (remove-hook 'flymake-diagnostic-functions #'lua-ts-flymake-luacheck t))))

(defun ide-lua--workspace-config ()
  "LuaLS settings: the configured runtime, no third-party prompts, no telemetry."
  `(:runtime (:version ,ide-lua-runtime-version)
    :workspace (:checkThirdParty :json-false)
    :telemetry (:enable :json-false)
    :completion (:callSnippet "Replace")
    :hint (:enable t)))

(ide-lang-workspace-config :Lua (ide-lua--workspace-config))

;;;; stylua ------------------------------------------------------------------------------------

(defun ide-lua-stylua-config (&optional dir)
  "The stylua.toml or .stylua.toml governing DIR (default: here), or nil."
  (let ((dir (or dir default-directory)))
    (cl-loop for name in '("stylua.toml" ".stylua.toml")
             for d = (locate-dominating-file dir name)
             when d return (expand-file-name name d))))

(defun ide-lua--stylua-args ()
  "Extra stylua arguments for the current buffer: the project's config, else our policy."
  (append
   (if-let* ((cfg (ide-lua-stylua-config)))
       (list "--config-path" cfg)
     (list "--indent-type" "Spaces" "--indent-width" (number-to-string ide-lua-indent-width)))
   (when buffer-file-name (list "--stdin-filepath" buffer-file-name))))

(ide-lang-define-formatter 'ide-lua-stylua
  '("stylua" (ide-lua--stylua-args) "-")
  'lua-ts-mode)

(defun ide-lua-write-stylua-config ()
  "Write a stylua.toml (2 spaces) at the project root unless one exists."
  (interactive)
  (let ((file (expand-file-name "stylua.toml" (ide-lang-root))))
    (when (file-exists-p file) (user-error "%s already exists" file))
    (with-temp-file file
      (insert (format "indent_type = \"Spaces\"\nindent_width = %d\ncolumn_width = 100\n"
                      ide-lua-indent-width)))
    (find-file file)))

;;;; LuaLS project configuration --------------------------------------------------------------------

(defconst ide-lua-luarc-presets
  '(("Plain Lua" . nil)
    ("Neovim" . (:globals ["vim"] :runtime "LuaJIT"))
    ("LOVE" . (:globals ["love"] :runtime "LuaJIT"))
    ("Hammerspoon" . (:globals ["hs"] :runtime "Lua 5.4"))
    ("busted" . (:globals ["describe" "it" "before_each" "after_each" "setup" "teardown"
                           "pending" "spy" "stub" "mock" "assert" "insulate" "expose"]
                 :runtime "Lua 5.4")))
  "Presets for `ide-lua-write-luarc': name to (:globals [...] :runtime VERSION).")

(defun ide-lua--luarc (preset &optional extra-globals)
  "The .luarc.json content (a hash-table-ready plist) for PRESET plus EXTRA-GLOBALS."
  (let* ((globals (append (plist-get preset :globals) extra-globals nil))
         (runtime (or (plist-get preset :runtime) ide-lua-runtime-version))
         (rc (list :$schema "https://raw.githubusercontent.com/LuaLS/vscode-lua/master/setting/schema.json"
                   :runtime.version runtime
                   :workspace.checkThirdParty :false
                   :telemetry.enable :false)))
    (if globals (append rc (list :diagnostics.globals (vconcat globals))) rc)))

(defun ide-lua-write-luarc (preset globals)
  "Write a .luarc.json at the project root.
PRESET is chosen from `ide-lua-luarc-presets'; GLOBALS is a comma separated list of
extra global names.  LuaLS picks the file up without a restart."
  (interactive
   (list (completing-read "Preset: " (mapcar #'car ide-lua-luarc-presets) nil t nil nil "Plain Lua")
         (read-string "Extra globals (comma separated, empty for none): ")))
  (let ((file (expand-file-name ".luarc.json" (ide-lang-root)))
        (rc (ide-lua--luarc (alist-get preset ide-lua-luarc-presets nil nil #'equal)
                            (split-string globals "[ ,]+" t))))
    (when (and (file-exists-p file) (not (y-or-n-p (format "Overwrite %s? " file))))
      (user-error "Kept the existing .luarc.json"))
    (with-temp-file file
      (insert (json-serialize rc))
      (json-pretty-print-buffer))
    (message "Wrote %s" file)
    file))

(defun ide-lua-check-project ()
  "Report every LuaLS diagnostic of the project in the drawer (lua-language-server --check)."
  (interactive)
  (let ((exe (ide-lang-exe-or-error "lua-language-server")))
    (ide-lang-run "lua check" (list exe "--check" (ide-lang-root) "--checklevel" "Warning"))))

;;;; Run -------------------------------------------------------------------------------------------------

(defun ide-lua--spec-file-p (&optional file)
  "Non-nil when FILE (default: this buffer's) looks like a busted spec."
  (when-let* ((f (or file buffer-file-name)))
    (string-match-p "_spec\\.lua\\'" f)))

(defun ide-lua--run-command ()
  "Command line that runs the current Lua file."
  (when-let* (((derived-mode-p 'lua-ts-mode))
              (rel (ide-lang-relative-file)))
    (if (and (ide-lua--spec-file-p) (executable-find "busted"))
        (concat "busted " (shell-quote-argument rel))
      (concat "lua " (shell-quote-argument rel)))))

(defun ide-lua-run-file ()
  "Run the current file with lua (busted for specs) in the drawer."
  (interactive)
  (ide-lang-run "lua" (or (ide-lua--run-command) (user-error "Not a Lua file"))))

;;;; REPL -----------------------------------------------------------------------------------------------

(defun ide-lua-repl ()
  "Open the Lua REPL (lua -i) beside the code."
  (interactive)
  (lua-ts-inferior-lua))

(defun ide-lua--send (beg end)
  "Send BEG..END to the REPL, keeping the selected window."
  (save-selected-window (lua-ts-send-region beg end)))

(defun ide-lua-send-line ()
  "Send the current line to the REPL."
  (interactive)
  (ide-lua--send (line-beginning-position) (line-end-position)))

(defun ide-lua-send-region (beg end)
  "Send the region (or, without one, the function around point) to the REPL."
  (interactive (ide-lang-region-or-node '("function_declaration" "function_definition")))
  (ide-lua--send beg end))

(defun ide-lua-send-buffer ()
  "Send the whole buffer to the REPL."
  (interactive)
  (save-selected-window (lua-ts-send-buffer)))

;;;; busted ------------------------------------------------------------------------------------------------

(defconst ide-lua--describe-calls '("describe" "context" "insulate" "expose")
  "busted block functions: their names prefix the names of the tests inside.")
(defconst ide-lua--it-calls '("it" "test" "spec" "pending")
  "busted test functions.")

(defun ide-lua--call-label (node)
  "(KIND . NAME) when NODE is a busted describe/it call with a string title, else nil."
  (when (equal (treesit-node-type node) "function_call")
    (let* ((fn (treesit-node-text (treesit-node-child-by-field-name node "name") t))
           (args (treesit-node-child-by-field-name node "arguments"))
           (first (and args (treesit-node-child args 0 t)))
           (str (and first (equal (treesit-node-type first) "string")
                     (treesit-node-child-by-field-name first "content"))))
      (when (and str (or (member fn ide-lua--describe-calls) (member fn ide-lua--it-calls)))
        (cons (if (member fn ide-lua--it-calls) 'it 'describe)
              (treesit-node-text str t))))))

(defun ide-lua--test-path ()
  "The busted titles around point, outermost first, as (KIND . NAME) entries."
  (let (path (node (treesit-node-at (point))))
    (while node
      (when-let* ((label (ide-lua--call-label node))) (push label path))
      (setq node (treesit-node-parent node)))
    path))

(defun ide-lua--escape-pattern (string)
  "Escape STRING for use inside a Lua pattern (busted --filter)."
  (replace-regexp-in-string "[][^$()%.*+?-]" "%\\&" string))

(defun ide-lua--filter-pattern (path)
  "Lua pattern selecting the busted test or block named by PATH, or nil when PATH is empty.
busted matches against the titles joined by spaces: an `it' is matched exactly, a
`describe' as a prefix."
  (when path
    (let ((full (mapconcat (lambda (e) (ide-lua--escape-pattern (cdr e))) path " ")))
      (concat "^" full (if (eq (car (car (last path))) 'it) "$" "")))))

(defun ide-lua--busted (&rest args)
  "Run busted with ARGS in the drawer, or say that it is not installed."
  (if (executable-find "busted")
      (ide-lang-run "busted" (cons "busted" args))
    (user-error "busted is not on PATH (luarocks install busted); tests skipped")))

(defun ide-lua-test-at-point ()
  "Run the busted test (or describe block) around point."
  (interactive)
  (let ((pattern (ide-lua--filter-pattern (ide-lua--test-path))))
    (unless pattern (user-error "No describe/it at point"))
    (ide-lua--busted (concat "--filter=" pattern) (ide-lang-relative-file))))

(defun ide-lua-test-file ()
  "Run the busted tests of the current file."
  (interactive)
  (ide-lua--busted (ide-lang-relative-file)))

(defun ide-lua-test-all ()
  "Run every busted test of the project."
  (interactive)
  (ide-lua--busted))

;;;; Manual ---------------------------------------------------------------------------------------------------

(defconst ide-lua--std-names
  '("assert" "collectgarbage" "dofile" "error" "getmetatable" "ipairs" "load" "loadfile"
    "next" "pairs" "pcall" "print" "rawequal" "rawget" "rawlen" "rawset" "require" "select"
    "setmetatable" "tonumber" "tostring" "type" "warn" "xpcall" "_G" "_VERSION")
  "Basic functions of the manual (anchor pdf-NAME).")

(defconst ide-lua--std-libs
  '("coroutine" "debug" "io" "math" "os" "package" "string" "table" "utf8")
  "Standard libraries: NAME.fn has the anchor pdf-NAME.fn.")

(defun ide-lua--manual-url (symbol)
  "Reference manual URL for SYMBOL (a name like string.format), or the manual's index."
  (let ((base (format "https://www.lua.org/manual/%s/manual.html" ide-lua-manual-version)))
    (cond
     ((and symbol (member symbol ide-lua--std-names)) (concat base "#pdf-" symbol))
     ((and symbol (string-match "\\`\\([a-z0-9]+\\)[.:]\\([A-Za-z_0-9]+\\)\\'" symbol)
           (member (match-string 1 symbol) ide-lua--std-libs))
      (concat base "#pdf-" (match-string 1 symbol) "." (match-string 2 symbol)))
     ((and symbol (member symbol ide-lua--std-libs)) (concat base "#6"))
     (t (concat base "#index")))))

(defun ide-lua--symbol-at-point ()
  "The dotted name at point (string.format), or nil."
  (save-excursion
    (let ((chars "A-Za-z0-9_.:"))
      (skip-chars-backward chars)
      (let ((beg (point)))
        (skip-chars-forward chars)
        (when (< beg (point))
          (string-trim (buffer-substring-no-properties beg (point)) "[.:]+" "[.:]+"))))))

(defun ide-lua-manual (symbol)
  "Show the Lua reference manual for SYMBOL (default: the name at point) in the browser."
  (interactive (list (ide-lua--symbol-at-point)))
  (ide-lang-browse (ide-lua--manual-url symbol)))

;;;; Menu --------------------------------------------------------------------------------------------------------

;;;###autoload (autoload 'ide-lua-dispatch "lang-lua" nil t)
(transient-define-prefix ide-lua-dispatch ()
  "Lua: run, REPL, test, project configuration, docs."
  [["Run"
    ("r" "run file" ide-lua-run-file)
    ("c" "check project (LuaLS)" ide-lua-check-project)
    ("f" "format buffer" apheleia-format-buffer)]
   ["REPL"
    ("e" "open REPL" ide-lua-repl)
    ("l" "send line" ide-lua-send-line)
    ("s" "send region / function" ide-lua-send-region)
    ("b" "send buffer" ide-lua-send-buffer)]
   ["Test (busted)"
    ("t" "test at point" ide-lua-test-at-point)
    ("F" "tests in file" ide-lua-test-file)
    ("a" "all tests" ide-lua-test-all)]
   ["Project"
    ("L" "write .luarc.json" ide-lua-write-luarc)
    ("S" "write stylua.toml" ide-lua-write-stylua-config)]
   ["Docs / server"
    ("D" "manual for symbol" ide-lua-manual)
    ("R" "restart server" eglot-reconnect)]])

(ide-lang-register '(lua-ts-mode)
  :run #'ide-lua--run-command
  :menu #'ide-lua-dispatch)

(provide 'lang-lua)
;;; lang-lua.el ends here
