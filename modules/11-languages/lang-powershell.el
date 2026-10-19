;;; lang-powershell.el --- PowerShell: PSES, Pester, REPL, help, debugger -*- lexical-binding: t; -*-

;;; Commentary:
;; PowerShell without leaving Emacs:
;;
;;   edit      powershell-mode + PowerShell Editor Services over Eglot (completion, hover,
;;             definition, PSScriptAnalyzer diagnostics, Invoke-Formatter on save)
;;   run       S-<f10> runs the script with `pwsh -NoProfile -File'; C-c m opens the menu
;;   test      Pester: the Describe/Context/It at point, the file, the whole project
;;   debug     S-<f9> debugs the script through PSES's debug service (dap-mode, over stdio)
;;   explore   Get-Help for the command at point (in a buffer or on learn.microsoft.com),
;;             Get-Command / module browsing, Invoke-ScriptAnalyzer, a pwsh REPL
;;
;; pwsh comes from mise.  Every pwsh process runs with its data, config, cache and .NET
;; home under var/tools/pwsh/ (see `ide-powershell--env'), so nothing lands in ~.
;; PSES and Pester are fetched by `ide-powershell-install-pses' and
;; `ide-powershell-install-pester'.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'comint)
(require 'compile)

(require '00-lang-lib)

(defvar compilation-error-regexp-alist-alist)
(defvar powershell-mode-map)
(declare-function dap-debug "dap-mode")
(declare-function dap-register-debug-provider "dap-mode")
(declare-function dap-register-debug-template "dap-mode")
(declare-function eglot-reconnect "eglot")

(defgroup ide-powershell nil "PowerShell support." :group 'ide-lang)

(defcustom ide-powershell-log-level "Warning"
  "PSES log level: Trace, Debug, Information, Warning, Error or None."
  :type 'string)

(defcustom ide-powershell-format-preset "OTBS"
  "PSScriptAnalyzer formatting preset used by PSES: OTBS, Allman, Stroustrup or Custom."
  :type '(choice (const "OTBS") (const "Allman") (const "Stroustrup") (const "Custom")))

;;;; Contained pwsh -------------------------------------------------------------------------

(defun ide-powershell--home (&rest parts)
  "A path under var/tools/pwsh/ in the sandbox."
  (apply #'ide-lang-tools-dir "pwsh" parts))

(defun ide-powershell--env ()
  "Environment assignments (\"K=V\" strings) that keep pwsh and .NET inside the sandbox."
  (let ((home (ide-powershell--home)))
    (dolist (d '("data" "config" "cache" "dotnet" "log" "tmp")) (make-directory (expand-file-name d home) t))
    (list (concat "XDG_DATA_HOME=" (expand-file-name "data" home))
          (concat "XDG_CONFIG_HOME=" (expand-file-name "config" home))
          (concat "XDG_CACHE_HOME=" (expand-file-name "cache" home))
          (concat "DOTNET_CLI_HOME=" (expand-file-name "dotnet" home))
          (concat "TMPDIR=" (expand-file-name "tmp" home))
          "POWERSHELL_TELEMETRY_OPTOUT=1" "DOTNET_CLI_TELEMETRY_OPTOUT=1"
          "DOTNET_NOLOGO=1" "NO_COLOR=1" "TERM=dumb")))

(defun ide-powershell--argv (&rest args)
  "Command line running pwsh with ARGS under the contained environment."
  (append '("env") (ide-powershell--env)
          (list (ide-lang-exe-or-error "pwsh")) args))

(defun ide-powershell--quote (string)
  "STRING as a single-quoted PowerShell literal."
  (concat "'" (replace-regexp-in-string "'" "''" string) "'"))

(defun ide-powershell--script-argv (script)
  "Argv running the PowerShell code SCRIPT (a string) in a fresh non-interactive pwsh."
  (ide-powershell--argv "-NoLogo" "-NoProfile" "-NonInteractive" "-Command"
                        (concat "$PSStyle.OutputRendering='PlainText'; $ProgressPreference='SilentlyContinue'; " script)))

(defun ide-powershell--eval (script)
  "Run SCRIPT in contained pwsh; return its output, or signal an error with the output."
  (with-temp-buffer
    (let* ((argv (ide-powershell--script-argv script))
           (status (apply #'call-process (car argv) nil t nil (cdr argv))))
      (unless (zerop status) (user-error "pwsh failed: %s" (string-trim (buffer-string))))
      (string-trim-right (buffer-string)))))

;;;; Mode and server ------------------------------------------------------------------------

(defun ide-powershell-pses-dir ()
  "Directory PSES is installed in (contains PowerShellEditorServices/)."
  (ide-lang-tools-dir "pses"))

(defun ide-powershell--pses-start-script ()
  (expand-file-name "PowerShellEditorServices/Start-EditorServices.ps1" (ide-powershell-pses-dir)))

(defun ide-powershell--pses-command (&rest extra)
  "Argv starting PSES over stdio (plus EXTRA switches); errors when PSES is missing.
The session-details and log files live under var/tools/pwsh/log/."
  (let ((start (ide-powershell--pses-start-script))
        (log (ide-powershell--home "log")))
    (unless (file-exists-p start)
      (user-error "PowerShell Editor Services is not installed; run M-x ide-powershell-install-pses"))
    (apply
     #'ide-powershell--argv
     "-NoLogo" "-NoProfile" "-NonInteractive" "-OutputFormat" "Text" "-File" start
     "-HostName" "Emacs" "-HostProfileId" "Emacs.Eglot" "-HostVersion" "1.0.0"
     "-LogPath" (expand-file-name "pses" log)
     "-LogLevel" ide-powershell-log-level
     "-SessionDetailsPath" (expand-file-name (format "session-%d.json" (emacs-pid)) log)
     "-BundledModulesPath" (ide-powershell-pses-dir)
     "-FeatureFlags" "@()" "-AdditionalModules" "@()"
     (append extra '("-Stdio")))))

(defun ide-powershell--server-contact (&optional _interactive _project)
  "Eglot contact for PSES."
  (ide-powershell--pses-command))

(defun ide-powershell--register-server ()
  "Make our PSES command the server for `powershell-mode' (replacing the package's)."
  (when (boundp 'eglot-server-programs)
    (setq eglot-server-programs
          (cons '(powershell-mode . ide-powershell--server-contact)
                (cl-remove-if (lambda (e) (eq (car e) 'powershell-mode)) eglot-server-programs)))))

(with-eval-after-load 'eglot (ide-powershell--register-server))
(add-hook 'powershell-mode-hook #'ide-powershell--register-server)

(ide-lang-workspace-config
 :powershell
 `(:scriptAnalysis (:enable t)
   ;; Formatting fixes layout only; it never rewrites aliases (ls -> Get-ChildItem) or casing.
   :codeFormatting (:preset ,ide-powershell-format-preset :autoCorrectAliases :json-false
                    :useCorrectCasing :json-false :trimWhitespaceAroundPipe t)
   :enableProfileLoading :json-false
   :pester (:useLegacyCodeLens :json-false)))

(defun ide-powershell-install-pses ()
  "Download the latest PowerShell Editor Services release into the sandbox."
  (interactive)
  (let ((dir (ide-powershell-pses-dir)))
    (ide-lang-fetch-extract
     "pses"
     (ide-lang-github-asset-url "PowerShell/PowerShellEditorServices" "\\`PowerShellEditorServices\\.zip\\'")
     dir 'zip)))

;;;; Running --------------------------------------------------------------------------------

(defalias 'ide-powershell--shell-string #'ide-lang-command-string)

(defun ide-powershell--pester-file-p (&optional file)
  (string-match-p "\\.[Tt]ests\\.ps1\\'" (or file buffer-file-name "")))

(defun ide-powershell--run-command ()
  (when (derived-mode-p 'powershell-mode)
    (if (ide-powershell--pester-file-p)
        (ide-lang-command-string (ide-powershell--pester-argv buffer-file-name nil))
      (ide-lang-command-string
       (ide-powershell--argv "-NoLogo" "-NoProfile" "-File" buffer-file-name)))))

(defun ide-powershell-run ()
  "Run the current script with pwsh -NoProfile -File in the drawer (Pester files run Pester)."
  (interactive)
  (ide-lang-require-file t)
  (if (ide-powershell--pester-file-p) (ide-powershell-test-file)
    (ide-lang-run "pwsh" (or (ide-powershell--run-command) (user-error "Not a PowerShell file")))))

;;;; Pester ----------------------------------------------------------------------------------

(defun ide-powershell-pester-installed-p ()
  "Non-nil when Pester 5 or newer is in the contained module path."
  (file-directory-p (ide-powershell--home "data" "powershell" "Modules" "Pester")))

(defun ide-powershell-install-pester ()
  "Install Pester from the PowerShell Gallery into var/tools/pwsh (CurrentUser scope)."
  (interactive)
  (ide-lang-run "install pester"
                (ide-powershell--script-argv
                 (concat "Set-PSRepository PSGallery -InstallationPolicy Trusted; "
                         "Install-Module Pester -Scope CurrentUser -Force -SkipPublisherCheck "
                         "-MinimumVersion 5.0 -Confirm:$false; "
                         "Get-Module Pester -ListAvailable | Select-Object Name,Version,Path | Format-List"))))

(defun ide-powershell--pester-argv (path full-name)
  "Argv running Pester on PATH, limited to FULL-NAME (a wildcard string) when non-nil."
  (ide-powershell--script-argv
   (concat "Import-Module Pester -MinimumVersion 5.0 -ErrorAction Stop; "
           "$c = New-PesterConfiguration; "
           "$c.Run.Path = " (ide-powershell--quote path) "; "
           (when full-name
             (concat "$c.Filter.FullName = " (ide-powershell--quote full-name) "; "))
           "$c.Run.Exit = $true; $c.Output.Verbosity = 'Detailed'; "
           "$c.Output.StackTraceVerbosity = 'Filtered'; "
           "Invoke-Pester -Configuration $c")))

(add-to-list 'compilation-error-regexp-alist-alist
             '(ide-powershell-pester "\\bat .*?, \\(.+?\\.ps[m]?1\\):\\([0-9]+\\)\\s-*$" 1 2))

(defun ide-powershell--pester (path full-name)
  "Run Pester on PATH (filtered by FULL-NAME) in the drawer."
  (unless (ide-powershell-pester-installed-p)
    (user-error "Pester is not installed; run M-x ide-powershell-install-pester"))
  (let ((buf (ide-lang-run "pester" (ide-powershell--pester-argv path full-name))))
    (with-current-buffer buf
      (setq-local compilation-error-regexp-alist '(ide-powershell-pester powershell)))
    buf))

(defconst ide-powershell--block-regexp
  "^[ \t]*\\(Describe\\|Context\\|It\\)[ \t]+\\(?:-Name[ \t]+\\)?\\('\\(?:[^']\\|''\\)*'\\|\"\\(?:[^\"`]\\|`.\\)*\"\\)"
  "Matches the head of a Pester Describe, Context or It block; group 2 is the quoted name.")

(defun ide-powershell--block-name (quoted)
  "The text of the PowerShell string literal QUOTED."
  (let ((body (substring quoted 1 -1)))
    (if (eq (aref quoted 0) ?')
        (replace-regexp-in-string "''" "'" body)
      (replace-regexp-in-string "`\\(.\\)" "\\1" body))))

(defun ide-powershell--block-contains-p (pos)
  "Non-nil when the block whose head matched just before point contains POS."
  (save-excursion
    (when (search-forward "{" nil t)
      (backward-char)
      (let ((end (ignore-errors (scan-sexps (point) 1))))
        (and end (<= pos end))))))

(defun ide-powershell--names-at-point ()
  "Names of the Pester blocks around point, outermost first, or nil."
  (let ((pos (point)) names)
    (save-excursion
      (end-of-line)
      (while (re-search-backward ide-powershell--block-regexp nil t)
        (let ((name (ide-powershell--block-name (match-string 2))))
          (save-excursion
            (goto-char (match-end 0))
            (when (ide-powershell--block-contains-p pos)
              (push name names))))))
    names))

(defun ide-powershell--full-name-filter (names)
  "Pester FullName filter for the block path NAMES (wildcarded: matches everything inside)."
  (concat (string-join (mapcar (lambda (n) (replace-regexp-in-string "[][*?`]" "`\\&" n)) names) ".") "*"))

(defun ide-powershell-test-at-point ()
  "Run the Pester It, Context or Describe block around point."
  (interactive)
  (ide-lang-require-file t)
  (let ((names (ide-powershell--names-at-point)))
    (unless names (user-error "No Describe/Context/It at point"))
    (ide-powershell--pester buffer-file-name (ide-powershell--full-name-filter names))))

(defun ide-powershell-test-file ()
  "Run every Pester test in the current file."
  (interactive)
  (ide-lang-require-file t)
  (ide-powershell--pester buffer-file-name nil))

(defun ide-powershell-test-all ()
  "Run every Pester test (*.Tests.ps1) under the project root."
  (interactive)
  (ide-powershell--pester (ide-lang-root) nil))

;;;; Analysis, help, browsing ---------------------------------------------------------------------

(defun ide-powershell--analyzer-import ()
  "PowerShell code importing PSScriptAnalyzer from PSES's bundle, else from the module path."
  (let ((bundled (expand-file-name "PSScriptAnalyzer" (ide-powershell-pses-dir))))
    (format "if (Test-Path %s) { Import-Module %s } else { Import-Module PSScriptAnalyzer }; "
            (ide-powershell--quote bundled) (ide-powershell--quote bundled))))

(defun ide-powershell-analyze ()
  "Run Invoke-ScriptAnalyzer on this file; the findings are jumpable in the drawer."
  (interactive)
  (ide-lang-require-file t)
  (ide-lang-run "psscriptanalyzer"
                (ide-powershell--script-argv
                 (concat (ide-powershell--analyzer-import)
                         "$r = Invoke-ScriptAnalyzer -Path " (ide-powershell--quote buffer-file-name) "; "
                         "foreach ($f in $r) { '{0}:{1}:{2}: {3}: {4} [{5}]' -f $f.ScriptPath, $f.Line, "
                         "$f.Column, $f.Severity, $f.Message, $f.RuleName }; "
                         "if (-not $r) { 'No findings.' }"))))

(defun ide-powershell--read-command (&optional prompt)
  "Command name at point, or one chosen from Get-Command."
  (or (and (not current-prefix-arg) (thing-at-point 'symbol t))
      (completing-read (or prompt "Command: ")
                       (split-string (ide-powershell--eval
                                      "Get-Command -All | Select-Object -ExpandProperty Name -Unique | Sort-Object")
                                     "\n" t)
                       nil nil)))

(defun ide-powershell-help (command)
  "Show Get-Help -Full for COMMAND (default: the symbol at point) in a buffer."
  (interactive (list (ide-powershell--read-command "Get-Help: ")))
  (let ((text (ide-powershell--eval
               (format "Get-Help %s -Full | Out-String -Width 100" (ide-powershell--quote command))))
        (buf (get-buffer-create (format "*PowerShell help: %s*" command))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text "\n")
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))

(defun ide-powershell-help-online (command)
  "Open the learn.microsoft.com page of COMMAND in the in-Emacs browser."
  (interactive (list (ide-powershell--read-command "Docs for: ")))
  (let ((module (ide-powershell--eval
                 (format "(Get-Command %s -ErrorAction Stop | Select-Object -First 1).Source"
                         (ide-powershell--quote command)))))
    (ide-lang-browse (format "https://learn.microsoft.com/powershell/module/%s/%s"
                             (downcase module) (downcase command)))))

(defun ide-powershell-browse-commands ()
  "Pick a module, then one of its commands, and show its help."
  (interactive)
  (let* ((modules (split-string (ide-powershell--eval
                                 "Get-Module -ListAvailable | Select-Object -ExpandProperty Name -Unique | Sort-Object")
                                "\n" t))
         (module (completing-read "Module: " modules nil t))
         (commands (split-string (ide-powershell--eval
                                  (format "Get-Command -Module %s | Select-Object -ExpandProperty Name | Sort-Object"
                                          (ide-powershell--quote module)))
                                 "\n" t)))
    (ide-powershell-help (completing-read (format "%s command: " module) commands nil t))))

;;;; REPL -------------------------------------------------------------------------------------------

(defconst ide-powershell--repl-buffer "*PowerShell*")

(defun ide-powershell--strip-escapes (string)
  "Drop the cursor-key mode sequences pwsh prints even when it is not on a terminal."
  (replace-regexp-in-string "\e\\[\\?1[hl]" "" string))

(defun ide-powershell-repl ()
  "Open a pwsh REPL over a pipe (contained environment, no profile).
There is no prompt: pwsh reads commands from stdin and prints their output.
Multi-line input is fine; send it with the `ide-powershell-send-*' commands."
  (interactive)
  (unless (comint-check-proc ide-powershell--repl-buffer)
    (let* ((argv (ide-powershell--argv "-NoLogo" "-NoProfile" "-NoExit" "-Command" "-"))
           (process-connection-type nil)
           (buf (apply #'make-comint-in-buffer "PowerShell" ide-powershell--repl-buffer
                       (car argv) nil (cdr argv))))
      (with-current-buffer buf
        (add-hook 'comint-preoutput-filter-functions #'ide-powershell--strip-escapes nil t))))
  (pop-to-buffer ide-powershell--repl-buffer))

(defun ide-powershell--send (text)
  "Send TEXT to the REPL, starting it when needed."
  (unless (comint-check-proc ide-powershell--repl-buffer)
    (save-window-excursion (ide-powershell-repl)))
  (let ((proc (get-buffer-process ide-powershell--repl-buffer)))
    (with-current-buffer ide-powershell--repl-buffer
      (save-excursion
        (goto-char (process-mark proc))
        (insert (propertize (concat (string-trim-right text) "\n") 'font-lock-face 'comint-highlight-input))
        (set-marker (process-mark proc) (point))))
    (comint-send-string proc (concat (string-trim-right text) "\n"))
    (when (string-search "\n" text) (comint-send-string proc "\n")))
  (display-buffer ide-powershell--repl-buffer))

(defun ide-powershell-send-line ()
  "Send the current line to the REPL."
  (interactive)
  (ide-powershell--send (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun ide-powershell-send-region (beg end)
  "Send the region to the REPL."
  (interactive "r")
  (ide-powershell--send (buffer-substring-no-properties beg end)))

(defun ide-powershell-send-defun ()
  "Send the function around point to the REPL."
  (interactive)
  (save-excursion
    (end-of-line)
    (if (re-search-backward "^[ \t]*\\(?:function\\|filter\\)\\_>" nil t)
        (let ((beg (point)))
          (search-forward "{")
          (backward-char)
          (forward-sexp)
          (ide-powershell--send (buffer-substring-no-properties beg (point))))
      (user-error "No function around point"))))

(defun ide-powershell-send-buffer ()
  "Send the whole buffer to the REPL."
  (interactive)
  (ide-powershell--send (buffer-substring-no-properties (point-min) (point-max))))

;;;; Debugging --------------------------------------------------------------------------------------

(defun ide-powershell--debug-command ()
  "Argv of the PSES debug service (stdio)."
  (ide-powershell--pses-command "-DebugServiceOnly"))

(defun ide-powershell--populate-debug-args (conf)
  "Fill CONF, a dap-mode launch plist, with the PSES server command and defaults."
  (let ((res (copy-sequence conf)))
    (dolist (item `((:dap-server-path . ,(ide-powershell--debug-command))
                    (:cwd . ,(ide-lang-root))
                    (:script . ,(or buffer-file-name (unless (plist-get conf :script) (read-file-name "Script: " nil nil t))))
                    (:args . [])
                    (:name . "PowerShell: Launch Script")
                    (:type . "PowerShell")))
      (unless (plist-get res (car item))
        (setq res (plist-put res (car item) (cdr item)))))
    (plist-put res :type "PowerShell")))

(with-eval-after-load 'dap-mode
  (dap-register-debug-provider "PowerShell" #'ide-powershell--populate-debug-args)
  (dap-register-debug-template
   "PowerShell: Launch Script"
   (list :type "PowerShell" :request "launch" :name "PowerShell: Launch Script"
         :script nil :cwd nil :args [] :createTemporaryIntegratedConsole :json-false)))

(defun ide-powershell-debug ()
  "Debug the current script with PSES's debug adapter (breakpoints, stepping, locals)."
  (interactive)
  (require 'dap-mode)
  (ide-lang-require-file t)
  (dap-debug (list :type "PowerShell" :request "launch" :name (format "PowerShell: %s" (buffer-name))
                   :script buffer-file-name :cwd (ide-lang-root) :args []
                   :createTemporaryIntegratedConsole :json-false)))

;;;; Server and menu ---------------------------------------------------------------------------------

(defun ide-powershell-show-log ()
  "Open the PSES log file."
  (interactive)
  (let ((logs (directory-files (ide-powershell--home "log" "pses") t "\\.log\\'")))
    (find-file-other-window (or (car (last logs)) (user-error "No PSES log yet")))))

;;;###autoload (autoload 'ide-powershell-dispatch "lang-powershell" nil t)
(transient-define-prefix ide-powershell-dispatch ()
  "PowerShell: run, test, debug, explore."
  [["Run"
    ("r" "run script" ide-powershell-run)
    ("a" "analyze (PSScriptAnalyzer)" ide-powershell-analyze)
    ("e" "REPL" ide-powershell-repl)
    ("l" "send line" ide-powershell-send-line)
    ("s" "send region" ide-powershell-send-region)
    ("f" "send function" ide-powershell-send-defun)
    ("b" "send buffer" ide-powershell-send-buffer)]
   ["Test (Pester)"
    ("t" "test at point" ide-powershell-test-at-point)
    ("T" "tests in file" ide-powershell-test-file)
    ("A" "all tests" ide-powershell-test-all)
    ("P" "install Pester" ide-powershell-install-pester)]
   ["Debug"
    ("d" "debug script" ide-powershell-debug)]
   ["Explore"
    ("h" "Get-Help" ide-powershell-help)
    ("H" "docs online" ide-powershell-help-online)
    ("c" "browse commands" ide-powershell-browse-commands)]
   ["Server"
    ("S" "install PSES" ide-powershell-install-pses)
    ("R" "restart server" eglot-reconnect)
    ("L" "server log" ide-powershell-show-log)]])

(ide-lang-register '(powershell-mode)
  :run #'ide-powershell--run-command
  :debug (lambda () (when (derived-mode-p 'powershell-mode) (ide-powershell-debug) t))
  :menu #'ide-powershell-dispatch)

(provide 'lang-powershell)
;;; lang-powershell.el ends here
