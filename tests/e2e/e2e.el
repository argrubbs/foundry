;;; e2e.el --- Drive a live Foundry the way a user would -*- lexical-binding: t; -*-

;;; Commentary:
;; End-to-end tests.  tests/e2e/run.sh starts a real Foundry frame with this file
;; loaded after init.el; once Elpaca has activated every package the tests run
;; inside that session, pressing keys with `execute-kbd-macro' and looking at
;; what a user would see: which buffer is current, where windows went, what the
;; buffer now contains, whether a menu or a prompt is up.  Nothing is mocked.
;;
;; The sample project comes from $E2E_PROJECT; results go to $E2E_REPORT and
;; the exit status is 0 only when every test passed.  Tests are named
;; e2e-NN-... because ERT runs them in name order, and later ones assume the
;; earlier ones have left the frame tidy.
;;
;; Writing a new test: open what you need with `e2e-open', press keys with
;; `e2e-keys', then `should' what the user would see.  Never press C-g in a
;; macro (it aborts the macro, not the prompt): finish prompts with RET, close
;; menus with `e2e-close-menu', and end with `e2e-reset'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)

(defvar e2e-project (getenv "E2E_PROJECT") "The sample project the tests open files from.")
(defvar e2e-report (getenv "E2E_REPORT") "Where the results are written.")
(defvar e2e-timeout 240 "Seconds before the run is abandoned as hung.")
(defvar e2e--messages-mark 1 "Where in *Messages* the error scan starts.")

;;;; Driving ----------------------------------------------------------------------------

(defun e2e-settle (&optional secs)
  "Let timers, process output and redisplay run for SECS (default 0.3)."
  (let ((end (+ (float-time) (or secs 0.3))))
    (while (< (float-time) end)
      (accept-process-output nil 0.05)
      (sit-for 0.05)))
  (redisplay t))

(defun e2e-keys (keys &optional wait)
  "Press KEYS (a `kbd' string) as the user would, then settle for WAIT seconds."
  (execute-kbd-macro (kbd keys))
  (e2e-settle wait))

(defun e2e-type (text &optional wait)
  "Type TEXT character by character, as the user would."
  (execute-kbd-macro text)
  (e2e-settle wait))

(defun e2e-open (file &optional wait)
  "Visit FILE of the sample project in the selected window; return its buffer."
  (prog1 (find-file (expand-file-name file e2e-project))
    ;; Eglot connects after the first command in the buffer; pretend one ran.
    (run-hooks 'post-command-hook)
    (e2e-settle (or wait 0.6))))

(defun e2e-wait-for (pred &optional secs)
  "Settle until PRED returns non-nil or SECS (default 10) pass; return PRED's value."
  (let ((end (+ (float-time) (or secs 10))) value)
    (while (and (not (setq value (funcall pred))) (< (float-time) end))
      (e2e-settle 0.2))
    value))

(defun e2e-side-window (side)
  "The window on SIDE of the frame (`left', `right', `bottom'), or nil."
  (cl-find-if (lambda (w) (eq (window-parameter w 'window-side) side)) (window-list)))

(defun e2e-menu-active-p ()
  "Non-nil when a transient menu is up."
  (bound-and-true-p transient--prefix))

(defun e2e-menu-command ()
  "The command of the transient menu that is up, or nil."
  (when (e2e-menu-active-p) (oref transient--prefix command)))

(defun e2e-close-menu ()
  "Close the transient menu if one is up, as C-q would."
  (when (e2e-menu-active-p)
    (e2e-keys "C-q")
    (when (e2e-menu-active-p) (transient--emergency-exit) (e2e-settle))))

(defun e2e-line ()
  "The text of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun e2e-last-message ()
  "The last line of *Messages*."
  (with-current-buffer (messages-buffer)
    (save-excursion
      (goto-char (point-max))
      (skip-chars-backward "\n")
      (buffer-substring-no-properties (line-beginning-position) (point)))))

(defun e2e-buffer-of (file)
  "The buffer visiting FILE of the sample project, or nil."
  (get-file-buffer (expand-file-name file e2e-project)))

(defun e2e-reset ()
  "Leave one window showing *scratch* and no buffer of the sample project."
  (e2e-close-menu)
  (while (> (minibuffer-depth) 0)
    (abort-minibuffers))
  (dolist (b (buffer-list))
    (when (and (buffer-file-name b)
               (string-prefix-p (file-name-as-directory e2e-project) (buffer-file-name b)))
      (with-current-buffer b (set-buffer-modified-p nil))
      (kill-buffer b)))
  (dolist (name '("*compilation*" "*Ilist*" "*Help*" "*Occur*"))
    (when (get-buffer name) (kill-buffer name)))
  (dolist (b (buffer-list))
    (when (string-match-p "\\`\\*\\(run: \\|Flymake diagnostics\\|magit\\)" (buffer-name b))
      (kill-buffer b)))
  (switch-to-buffer "*scratch*")
  (delete-other-windows)
  (e2e-settle))

(defun e2e-new-errors ()
  "Lines of *Messages* since the last call that look like errors."
  (with-current-buffer (messages-buffer)
    (let ((text (buffer-substring-no-properties (min e2e--messages-mark (point-max)) (point-max))))
      (setq e2e--messages-mark (point-max))
      (seq-filter (lambda (l) (string-match-p "\\(?:^\\|[^-]\\)\\(?:[Ee]rror\\|[Ff]ailed\\|wrong-type\\|void-\\)" l))
                  (split-string text "\n" t)))))

;;;; Startup ------------------------------------------------------------------------------

(ert-deftest e2e-01-startup-is-clean ()
  "The first frame: themed, with the IDE modes on, no warning or error so far."
  (should (eq (car custom-enabled-themes) 'doom-ember))
  (should (bound-and-true-p doom-modeline-mode))
  (should (bound-and-true-p vertico-mode))
  (should (bound-and-true-p global-corfu-mode))
  (should (bound-and-true-p which-key-mode))
  (should (bound-and-true-p ide-keys-mode))
  (should (bound-and-true-p vscode-indent-global-mode))
  (should-not (and (get-buffer "*Warnings*")
                   (> (buffer-size (get-buffer "*Warnings*")) 0)))
  (let ((errors (with-current-buffer (messages-buffer)
                  (seq-filter (lambda (l) (string-match-p "[Ee]rror\\|[Ff]ailed\\|void-" l))
                              (split-string (buffer-string) "\n" t)))))
    (should (equal errors nil))))

(ert-deftest e2e-02-ready-quickly ()
  "Packages are activated within a few seconds of starting."
  (should (boundp 'elpaca-after-init-time))
  (should (< (float-time (time-subtract elpaca-after-init-time before-init-time)) 5)))

;;;; Languages ----------------------------------------------------------------------------

(defconst e2e-language-files
  '(("hello.py" python-ts-mode nil)
    ("app.js" js-ts-mode ide-web-dispatch)
    ("main.go" go-ts-mode nil)
    ("lib.rs" rust-ts-mode ide-rust-dispatch)
    ("a.lua" lua-ts-mode ide-lua-dispatch)
    ("s.sh" bash-ts-mode ide-bash-dispatch)
    ("Dockerfile" dockerfile-ts-mode ide-dockerfile-dispatch)
    ("main.tf" terraform-mode iac-dispatch)
    ("config.toml" toml-ts-mode ide-toml-dispatch)
    ("deploy.yaml" yaml-ts-mode ide-k8s-dispatch)
    ("chart/templates/d.yaml" ide-helm-mode ide-k8s-dispatch)
    ("playbook.yml" yaml-ts-mode ide-k8s-dispatch)
    ("README.md" gfm-mode ide-markdown-menu)
    ("a.ps1" powershell-mode ide-powershell-dispatch))
  "(FILE MAJOR-MODE MENU): what opening FILE must give, and what C-c m opens.")

(ert-deftest e2e-03-every-language-opens ()
  "Each sample file gets its mode, its C-c m menu, and raises no error."
  (e2e-new-errors)
  (dolist (spec e2e-language-files)
    (pcase-let ((`(,file ,mode ,menu) spec))
      (e2e-open file)
      (should (eq major-mode mode))
      (when (derived-mode-p 'prog-mode)
        (should (bound-and-true-p flymake-mode)))
      ;; Python and Go have no C-c m menu yet (their commands are on S-<f10> and
      ;; the pytest / gotest commands); the table says nil for them.
      (when menu
        (e2e-keys "C-c m" 0.5)
        (should (eq (e2e-menu-command) menu))
        (e2e-close-menu))
      (let ((errors (e2e-new-errors)))
        (should (equal (list file errors) (list file nil))))))
  (e2e-reset))

(ert-deftest e2e-04-ansible-is-recognised ()
  "A playbook gets the ansible minor mode and the xref backend."
  (e2e-open "playbook.yml")
  (should (bound-and-true-p ansible))
  (should (memq 'ansible-xref-backend xref-backend-functions))
  (e2e-reset))

(ert-deftest e2e-05-manifest-is-recognised ()
  "A Kubernetes manifest gets `ide-k8s-mode'; a chart template the Helm mode."
  (e2e-open "deploy.yaml")
  (should (bound-and-true-p ide-k8s-mode))
  (e2e-open "chart/templates/d.yaml")
  (should (eq major-mode 'ide-helm-mode))
  (e2e-reset))

;;;; Typing -------------------------------------------------------------------------------

(ert-deftest e2e-10-python-indents-after-colon ()
  "RET after `def f():' indents the next line by four."
  (e2e-open "new.py")
  (e2e-type "def f():\rreturn 1")
  (should (equal (buffer-string) "def f():\n    return 1"))
  (e2e-reset))

(ert-deftest e2e-11-js-brace-splits-into-three-lines ()
  "Typing `{' closes it, and RET between the braces gives an indented middle line."
  (e2e-open "new.js")
  (e2e-type "function f() {\r")
  (should (equal (buffer-string) "function f() {\n  \n}"))
  (should (equal (e2e-line) "  "))
  (e2e-type "}")
  ;; the closing brace typed on the indented line is outdented to its opener
  (should (equal (e2e-line) "}"))
  (e2e-reset))

(ert-deftest e2e-12-tab-completes-a-word ()
  "TAB after a word offers completions instead of inserting indentation."
  (e2e-open "new.el")
  (e2e-type "(defu")
  (e2e-keys "TAB" 0.6)
  (should (or completion-in-region-mode
              (string-prefix-p "(defun" (e2e-line))))
  (should-not (string-match-p "\t\\|  " (e2e-line)))
  (e2e-reset))

(ert-deftest e2e-13-tab-indents-a-blank-line ()
  "On a blank line TAB goes where code belongs."
  (e2e-open "new.py")
  (e2e-type "if x:\r")
  (should (equal (e2e-line) "    "))
  (e2e-keys "TAB")
  (should (equal (e2e-line) "        "))
  (e2e-keys "<backtab>")
  (should (equal (e2e-line) "    "))
  (e2e-reset))

(ert-deftest e2e-14-markdown-tab-folds ()
  "TAB on a heading folds its body, as markdown-mode does (vscode-indent stays out)."
  (e2e-open "README.md")
  (goto-char (point-min))
  (should-not (bound-and-true-p vscode-indent-mode))
  (e2e-keys "TAB")
  (should (invisible-p (save-excursion (forward-line 2) (point))))
  (e2e-reset))

;;;; Saving -------------------------------------------------------------------------------

(ert-deftest e2e-20-saving-markdown-keeps-the-text ()
  "No formatter rewrites a Markdown file on save."
  (e2e-open "notes.md")
  (let ((before (buffer-string)))
    (goto-char (point-max))
    (e2e-type "more\n")
    (e2e-keys "C-x C-s" 1)
    (should (equal (buffer-string) (concat before "more\n")))
    (should-not (buffer-modified-p)))
  (e2e-reset))

(ert-deftest e2e-21-saving-yaml-keeps-the-layout ()
  "The YAML language server does not reformat on save."
  (e2e-open "deploy.yaml")
  (e2e-wait-for (lambda () (bound-and-true-p eglot--managed-mode)) 8)
  (goto-char (point-max))
  (e2e-type "extra:    1\n")
  (e2e-keys "C-x C-s" 1)
  (should (string-suffix-p "extra:    1\n" (buffer-string)))
  (e2e-reset))

;;;; Tool windows -------------------------------------------------------------------------

(ert-deftest e2e-30-project-tree-toggles ()
  "C-c w 1 opens the project tree on the left and closes it again."
  (e2e-open "hello.py")
  (e2e-keys "C-c w 1" 1)
  (should (e2e-side-window 'left))
  (should (eq (with-current-buffer (window-buffer (e2e-side-window 'left)) major-mode) 'treemacs-mode))
  (e2e-keys "C-c w 1" 0.5)
  (should-not (e2e-side-window 'left))
  (e2e-reset))

(ert-deftest e2e-31-structure-toggles ()
  "C-c w 7 shows the outline on the right, and C-c w 7 hides it."
  (e2e-open "hello.py")
  (e2e-keys "C-c w 7" 0.6)
  (should (e2e-side-window 'right))
  (e2e-keys "C-c w 7" 0.4)
  (should-not (e2e-side-window 'right))
  (e2e-reset))

(defun e2e-open-with-diagnostics ()
  "Open bad.py and wait for the language server to report its undefined name."
  (e2e-open "bad.py")
  (should (e2e-wait-for (lambda () (flymake-diagnostics)) 20)))

(ert-deftest e2e-32-problems-toggles ()
  "C-c w 6 opens the diagnostics drawer at the bottom; pressed again, it closes it."
  (e2e-open-with-diagnostics)
  (e2e-keys "C-c w 6" 0.6)
  (should (e2e-side-window 'bottom))
  (should (string-prefix-p "*Flymake diagnostics" (buffer-name (window-buffer (e2e-side-window 'bottom)))))
  (e2e-keys "C-c w 6" 0.4)
  (should-not (e2e-side-window 'bottom))
  (e2e-reset))

(ert-deftest e2e-33-escape-returns-to-the-code ()
  "C-c w e closes the drawer and puts the cursor back in the code."
  (e2e-open-with-diagnostics)
  (let ((code (selected-window)))
    (e2e-keys "C-c w 6" 0.6)
    (select-window (e2e-side-window 'bottom))
    (e2e-keys "C-c w e" 0.4)
    (should (eq (selected-window) code))
    (should-not (e2e-side-window 'bottom)))
  (e2e-reset))

(ert-deftest e2e-33b-problems-with-nothing-to-show ()
  "C-c w 6 in a clean buffer says so; it neither errors nor opens a drawer."
  (e2e-open "new.el")
  (e2e-keys "C-c w 6" 0.4)
  (should-not (e2e-side-window 'bottom))
  (should (string-match-p "No problems\\|No diagnostics" (e2e-last-message)))
  (e2e-reset))

(ert-deftest e2e-34-run-current-file ()
  "S-<f10> runs the file in a drawer at the bottom and shows its output."
  (e2e-open "hello.py")
  (e2e-keys "S-<f10>" 1)
  (let ((win (e2e-side-window 'bottom)))
    (should win)
    (should (with-current-buffer (window-buffer win) (derived-mode-p 'compilation-mode)))
    (when (executable-find "python")
      (should (e2e-wait-for (lambda () (with-current-buffer (window-buffer win)
                                         (string-match-p "^hello from e2e$" (buffer-string))))
                            15))))
  (e2e-keys "C-c w 4" 0.4)
  (should-not (e2e-side-window 'bottom))
  (e2e-reset))

(ert-deftest e2e-35-magit-status-and-back ()
  "C-c w 9 opens Magit for the project; q returns to the file."
  (e2e-open "hello.py")
  (e2e-keys "C-c w 9" 1.5)
  (should (eq major-mode 'magit-status-mode))
  (should (string-match-p "e2e sample" (buffer-string)))
  (e2e-keys "q" 0.5)
  (should (eq (current-buffer) (e2e-buffer-of "hello.py")))
  (e2e-reset))

(ert-deftest e2e-36-services-menu ()
  "C-c w 8 opens the Services & DevOps menu."
  (e2e-keys "C-c w 8" 0.5)
  (should (eq (e2e-menu-command) 'ide-services-dispatch))
  (e2e-close-menu)
  (should-not (e2e-menu-active-p)))

;;;; Search and commands ------------------------------------------------------------------

(ert-deftest e2e-40-consult-line-finds-text ()
  "M-s l with a word jumps to its line."
  (e2e-open "README.md")
  (goto-char (point-min))
  (e2e-keys "M-s l unique-needle RET" 0.5)
  (should (string-match-p "unique-needle" (e2e-line)))
  (e2e-reset))

(ert-deftest e2e-41-m-x-runs-a-command ()
  "M-x with completion runs a Foundry command."
  (let ((before ide-ai-inline-completion))
    (e2e-keys "M-x ide-ai-toggle-inline-completion RET" 0.5)
    (should (eq ide-ai-inline-completion (not before)))
    (e2e-keys "C-c a t" 0.5)
    (should (eq ide-ai-inline-completion before))))

(ert-deftest e2e-42-switch-buffer ()
  "M-s b switches to another open file by name."
  (e2e-open "hello.py")
  (e2e-open "app.js")
  (e2e-keys "M-s b hello.py RET" 0.5)
  (should (eq (current-buffer) (e2e-buffer-of "hello.py")))
  (e2e-reset))

(ert-deftest e2e-43-theme-picker ()
  "C-c j t changes the theme, and the house theme comes back."
  (e2e-keys "C-c j t doom-one RET" 1)
  (should (eq (car custom-enabled-themes) 'doom-one))
  (e2e-keys "C-c j t doom-ember RET" 1)
  (should (eq (car custom-enabled-themes) 'doom-ember)))

(ert-deftest e2e-44-xref-definition ()
  "M-. on a Python name defined in the file jumps to its definition."
  (e2e-open "hello.py")
  (e2e-wait-for (lambda () (bound-and-true-p eglot--managed-mode)) 10)
  (goto-char (point-max))
  (search-backward "greet(")
  (e2e-keys "M-." 2)
  (should (string-prefix-p "def greet" (e2e-line)))
  (e2e-reset))

;;;; The end ------------------------------------------------------------------------------

(defconst e2e-known-noise
  '(;; tsgo ignores the shutdown request, Eglot kills it, and one of Eglot's own
    ;; timers then finds the connection gone (upstream).
    "Error running timer: (jsonrpc-error \"No current JSON-RPC connection\"")
  "Lines in *Messages* that look like errors but come from upstream packages.")

(ert-deftest e2e-99-no-errors-were-logged ()
  "Nothing in the whole run wrote an error to *Messages*, and *Warnings* holds
only the server warnings the config logs without showing (eglot, jsonrpc)."
  (when (get-buffer "*Warnings*")
    (let ((shown (with-current-buffer "*Warnings*"
                   (seq-remove (lambda (l) (string-match-p "Warning (\\(?:eglot\\|jsonrpc\\))" l))
                               (split-string (buffer-string) "\n" t)))))
      (should (equal shown nil))))
  (let ((errors (with-current-buffer (messages-buffer)
                  (seq-filter (lambda (l)
                                (and (string-match-p "\\(?:^\\|[^-]\\)\\(?:[Ee]rror\\|wrong-type\\|void-\\)" l)
                                     (not (cl-some (lambda (k) (string-prefix-p k l)) e2e-known-noise))))
                              (split-string (buffer-string) "\n" t)))))
    (should (equal errors nil))))

;;;; Driver -------------------------------------------------------------------------------

(defun e2e--own-frames (frames)
  "The (FUN . ARGS) of FRAMES that belong to Foundry or these tests, innermost first."
  (let (own)
    (dolist (f frames)
      (let ((fun (ignore-errors (backtrace-frame-fun f))))
        (when (and (symbolp fun)
                   (string-match-p "\\`\\(?:ide-\\|e2e-\\|vsi-\\|ansible-xref\\|azdo-\\|iac-\\)" (symbol-name fun))
                   (not (string-prefix-p "e2e--" (symbol-name fun))))
          (push (cons fun (seq-take (ignore-errors (backtrace-frame-args f)) 3)) own))))
    (setq own (nreverse own))
    ;; An error raised from a hook or a package shows no Foundry frame: give the raw top.
    (if (< (length own) 2)
        (mapcar (lambda (f) (cons (ignore-errors (backtrace-frame-fun f))
                                  (seq-take (ignore-errors (backtrace-frame-args f)) 3)))
                (seq-take frames 14))
      (seq-take own 12))))

(defun e2e--write-report (stats)
  "Write the results of STATS and the session's messages to `e2e-report'."
  (with-temp-file e2e-report
    (insert (format "E2E: %d tests, %d passed, %d failed, %d skipped\n\n"
                    (ert-stats-total stats)
                    (ert-stats-completed-expected stats)
                    (ert-stats-completed-unexpected stats)
                    (ert-stats-skipped stats)))
    (cl-loop for test across (ert--stats-tests stats)
             for result across (ert--stats-test-results stats)
             do (insert (format "%-8s %s\n"
                                (cond ((ert-test-skipped-p result) "SKIPPED")
                                      ((ert-test-result-expected-p test result) "passed")
                                      (t "FAILED"))
                                (ert-test-name test)))
             unless (or (ert-test-skipped-p result) (ert-test-result-expected-p test result))
             do (insert (format "         %s\n"
                                (string-replace "\n" "\n         "
                                                (string-trim
                                                 (pp-to-string (ert-test-result-with-condition-condition result)))))
                        (if (ert-test-result-with-condition-p result)
                            (concat "         backtrace:\n           "
                                    (string-replace "\n" "\n           "
                                                    (string-trim
                                                     (mapconcat (lambda (f) (truncate-string-to-width (format "%S" f) 160 nil nil "…"))
                                                                (e2e--own-frames (ert-test-result-with-condition-backtrace result))
                                                                "\n")))
                                    "\n")
                          "")))
    (insert "\n--- *Messages* ---\n")
    (insert (with-current-buffer (messages-buffer) (buffer-string)))
    (when (get-buffer "*Warnings*")
      (insert "\n--- *Warnings* ---\n")
      (insert (with-current-buffer "*Warnings*" (buffer-string))))))

(defun e2e--run ()
  "Run the tests in this live session, write the report, exit with the verdict."
  (let ((stats nil) (code 3))
    (unwind-protect
        (progn
          (setq stats (ert-run-tests "\\`e2e-" #'ignore))
          (condition-case err
              (e2e--write-report stats)
            (error (with-temp-file e2e-report
                     (insert (format "E2E: %d failed, but the report writer broke: %s\n"
                                     (ert-stats-completed-unexpected stats) (error-message-string err))))))
          (setq code (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
      (unless stats
        (with-temp-file e2e-report (insert "E2E: the driver itself failed; see the terminal\n")))
      (kill-emacs code))))

(defun e2e--start ()
  "Run the tests once the frame is up and every package is active."
  (run-with-timer e2e-timeout nil
                  (lambda ()
                    (with-temp-file e2e-report
                      (insert (format "E2E: TIMEOUT after %ds (a prompt or a hang); last buffer %s\n"
                                      e2e-timeout (buffer-name)))
                      (insert (with-current-buffer (messages-buffer) (buffer-string))))
                    (kill-emacs 2)))
  (run-with-idle-timer 1 nil #'e2e--run))

(unless (and e2e-project e2e-report)
  (error "E2E_PROJECT and E2E_REPORT must be set; use tests/e2e/run.sh"))

(if (boundp 'elpaca-after-init-time)
    (e2e--start)
  (add-hook 'elpaca-after-init-hook #'e2e--start))

(provide 'e2e)
;;; e2e.el ends here
