;;; smoke-test.el --- Automated validation of isolated config -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory ~/.config/emacs-foundry --batch \
;;             -l init.el -l tests/smoke-test.el

;;; Code:

(require 'ert)

;; Packages are lazy-loaded for startup speed; force the ones asserted on.
(dolist (feature '(magit org ob popper))
  (require feature nil t))

(ert-deftest test-isolation-directory ()
  "Ensure user-emacs-directory is sandboxed."
  (should (file-exists-p (expand-file-name "init.el" user-emacs-directory)))
  (should (not (string-equal (expand-file-name "~/.emacs.d/") (expand-file-name user-emacs-directory)))))

(ert-deftest test-state-contained ()
  "Runtime state lives inside the sandbox."
  (dolist (v (list custom-file recentf-save-file savehist-file elpaca-directory))
    (should (string-prefix-p (directory-file-name (expand-file-name user-emacs-directory))
                             (expand-file-name v))))
  (should (string-prefix-p (expand-file-name user-emacs-directory)
                           (expand-file-name (cdr (car backup-directory-alist))))))

(ert-deftest test-elpaca-initialized ()
  "Ensure elpaca package manager is active inside sandbox."
  ;; `elpaca-alist' was removed upstream; queues now live in `elpaca--queues'.
  (should (or (boundp 'elpaca-alist) (boundp 'elpaca--queues)))
  (should (fboundp 'elpaca-process-queues))
  (should (equal (file-name-as-directory (expand-file-name elpaca-directory))
                 (expand-file-name "elpaca/" user-emacs-directory))))

(ert-deftest test-completion-stack-loaded ()
  "Verify Vertico, Orderless, and Consult are available."
  (should (fboundp 'vertico-mode))
  (should (fboundp 'consult-ripgrep))
  (should (boundp 'orderless-matching-styles))
  (should (fboundp 'ide-complete-snippet))
  (should (eq (lookup-key ide-keys-mode-map (kbd "M-s s")) #'ide-complete-snippet)))

(ert-deftest test-lsp-and-treesitter ()
  "Verify Eglot and Tree-sitter configurations."
  (should (fboundp 'eglot))
  (should (boundp 'major-mode-remap-alist))
  (should (alist-get 'python-mode major-mode-remap-alist)))

(ert-deftest test-layout-and-tool-windows ()
  "Verify popper and tool-window configurations."
  (should (fboundp 'popper-mode))
  (should (boundp 'popper-reference-buffers))
  (should (fboundp 'treemacs))
  (should (fboundp 'imenu-list-smart-toggle)))

(ert-deftest test-vcs-and-azdo-integration ()
  "Verify Magit and Azure DevOps porcelain functions."
  (should (fboundp 'magit-status))
  (should (fboundp 'azdo-dispatch))
  (should (fboundp 'azdo-pr-dispatch))
  (should (boundp 'magit-status-sections-hook))
  (require 'magit)
  (should (memq 'azdo-magit-insert-pull-requests magit-status-sections-hook)))

(ert-deftest test-azdo-remote-parsing ()
  "Azure DevOps remote URLs are parsed into org/project/repo."
  (should (equal (azdo--parse-remote "https://dev.azure.com/acme/My%20Proj/_git/web")
                 '(:org "acme" :project "My Proj" :repo "web")))
  (should (equal (azdo--parse-remote "git@ssh.dev.azure.com:v3/acme/proj/repo")
                 '(:org "acme" :project "proj" :repo "repo")))
  (should (equal (azdo--parse-remote "https://acme.visualstudio.com/proj/_git/repo")
                 '(:org "acme" :project "proj" :repo "repo")))
  (should (null (azdo--parse-remote "https://github.com/a/b.git"))))

(ert-deftest test-magit-push-menu-not-shadowed ()
  "`P c' / `P l' live in the push menu; `P' itself must still be Magit's push."
  (require 'magit)
  (require 'devops-azdo-magit)
  (should (eq (lookup-key magit-status-mode-map (kbd "P")) 'magit-push))
  (should (eq (plist-get (cdr (transient-get-suffix 'magit-push "c")) :command) 'azdo-pr-create))
  (should (eq (plist-get (cdr (transient-get-suffix 'magit-push "l")) :command) 'azdo-pr-list))
  (should (eq (lookup-key magit-status-mode-map (kbd "O")) 'azdo-dispatch)))

(ert-deftest test-azdo-pr-status ()
  "Review status derivation from votes."
  (should (equal (azdo--pr-status '((reviewers . (((vote . 10)))))) "Approved"))
  (should (equal (azdo--pr-status '((reviewers . (((vote . -5)))))) "Waiting"))
  (should (equal (azdo--pr-status '((isDraft . t))) "Draft")))

(ert-deftest test-devops-and-iac-packages ()
  "Verify Docker, Kubernetes, Terraform, and Ansible modes."
  (should (fboundp 'docker))
  (should (fboundp 'kubel))
  (should (fboundp 'ghostel))
  (should (fboundp 'terraform-mode))
  (should (fboundp 'ansible))
  (should (fboundp 'azure-dispatch))
  (should (fboundp 'iac-dispatch)))

(ert-deftest test-ai-and-org-suites ()
  "Verify AI tools and Org-mode suites."
  (should (or (fboundp 'minuet-mode) (fboundp 'gptel)))
  (should (fboundp 'org-mode))
  (should (fboundp 'org-roam-node-find))
  (should (boundp 'org-babel-load-languages))
  (should (fboundp 'confluence-publish-buffer)))

(ert-deftest test-gptel-agent-and-skills ()
  "gptel-agent replaces agent-shell and sees Claude-style skill directories."
  (should (fboundp 'gptel-agent))
  (should (eq (key-binding (kbd "C-c a a")) 'ide-ai-agent))
  (should-not (locate-library "agent-shell"))
  (require 'gptel-agent)
  (should (member "~/.claude/skills/" gptel-agent-skill-dirs))
  (let ((tmp (make-temp-file "plug" t)))
    (unwind-protect
        (let* ((skills (expand-file-name ".claude/plugins/cache/mkt/plug/1.0/skills" tmp))
               (home-old (getenv "HOME")))
          (make-directory (expand-file-name "demo" skills) t)
          (with-temp-file (expand-file-name "demo/SKILL.md" skills)
            (insert "---\nname: demo\ndescription: A demo skill\n---\nBody\n"))
          (setenv "HOME" tmp)
          (unwind-protect
              (progn (should (equal (ide-ai-claude-plugin-skill-dirs) (list skills)))
                     (let ((gptel-agent-skill-dirs '("~/.claude/skills/")))
                       (ide-ai--refresh-skill-dirs)
                       (should (member skills gptel-agent-skill-dirs))
                       (should (assoc "demo" (gptel-agent--update-skills)))))
            (setenv "HOME" home-old)))
      (delete-directory tmp t))))

(defmacro smoke--with-user-dir (&rest body)
  "Run BODY with `ide-user-dir' pointing at an empty temporary directory."
  `(let* ((tmp (file-name-as-directory (make-temp-file "userdir" t)))
          (ide-user-dir tmp) (ide-user-load-errors nil) (ide-user-eval-new-forms nil))
     (unwind-protect (progn ,@body) (delete-directory tmp t))))

(defvar smoke--user-log nil)

(ert-deftest test-user-files-load-in-order-and-are-isolated ()
  (smoke--with-user-dir
    (setq smoke--user-log nil)
    (dolist (f '(("20-b.el" . "(push 'b smoke--user-log)") ("10-a.el" . "(push 'a smoke--user-log)")
                 ("15-broken.el" . "(error \"boom\")") ("16-syntax.el" . "(oops (")
                 (".hidden.el" . "(push 'hidden smoke--user-log)") ("#lock.el" . "(push 'lock smoke--user-log)")
                 ("notes.txt" . "(push 'txt smoke--user-log)")))
      (with-temp-file (expand-file-name (car f) tmp) (insert (cdr f))))
    (make-directory (expand-file-name "sub" tmp))
    (should (equal (mapcar #'file-name-nondirectory (ide-user-files)) '("10-a.el" "15-broken.el" "16-syntax.el" "20-b.el")))
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (should (= 2 (ide-user-load-all))))
    (should (equal (reverse smoke--user-log) '(a b)))
    (should (= 2 (length ide-user-load-errors)))
    (should (equal (cdr (assoc (expand-file-name "15-broken.el" tmp) ide-user-load-errors)) "boom"))))

(ert-deftest test-user-files-can-be-disabled ()
  (smoke--with-user-dir
    (setq smoke--user-log nil)
    (with-temp-file (expand-file-name "a.el" tmp) (insert "(push 'a smoke--user-log)"))
    (let ((process-environment (cons "IDE_NO_USER=1" process-environment)))
      (should (= 0 (ide-user-load-all))))
    (should-not smoke--user-log)))

(ert-deftest test-user-package-forms ()
  (dolist (spec '(("foo" nil nil) ("foo" "owner/foo" nil) ("foo" "https://example.org/foo.git" t)))
    (let* ((text (apply #'ide-user--package-form spec)) (form (car (read-from-string text))))
      (should (eq (car form) 'use-package))
      (should (eq (cadr form) 'foo))
      (should (macroexpand-all form))))
  (should (string-match-p ":host github :repo \"owner/foo\"" (ide-user--package-form "foo" "owner/foo")))
  (should (string-match-p ":ensure (:repo \"https://example.org/foo.git\")" (ide-user--package-form "foo" "https://example.org/foo.git")))
  (should (string-match-p ":demand t" (ide-user--package-form "foo" nil t)))
  (should-not (string-match-p ":demand" (ide-user--package-form "foo")))
  (should (ide-user--valid-package-name-p "magit-section"))
  (dolist (bad '("" "Foo" "a b" "a)(evil" "-x" "a;b")) (should-not (ide-user--valid-package-name-p bad))))

(ert-deftest test-user-add-package-writes-file-once ()
  (smoke--with-user-dir
    (let ((file (ide-user-add-package "demo-pkg" "owner/demo-pkg" t)))
      (should (equal file (expand-file-name "packages.el" tmp)))
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (string-match-p "\\`;;; packages.el" text))
        (should (string-match-p "(use-package demo-pkg" text)))
      (ide-user-add-package "other-pkg")
      (should (= 2 (with-temp-buffer (insert-file-contents file) (count-matches "(use-package " (point-min) (point-max)))))
      (should-error (ide-user-add-package "demo-pkg") :type 'user-error)
      (should-error (ide-user-add-package "Bad Name") :type 'user-error)
      ;; the generated file is loadable Lisp
      (with-temp-buffer (insert-file-contents file) (goto-char (point-min))
                        (should (condition-case nil (progn (while t (read (current-buffer)))) (end-of-file t)))))
    (kill-buffer (find-buffer-visiting (expand-file-name "packages.el" tmp)))))

(ert-deftest test-user-add-package-never-prompts-about-open-buffer ()
  "With packages.el open, adding another package refreshes the buffer silently
and refuses (rather than prompting) when the buffer has unsaved edits."
  (smoke--with-user-dir
    (let ((prompted nil) (file (expand-file-name "packages.el" tmp)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) (setq prompted t) t))
                ((symbol-function 'y-or-n-p) (lambda (&rest _) (setq prompted t) t)))
        (ide-user-add-package "first-pkg")
        (let ((buf (find-file-noselect file)))
          (ide-user-add-package "second-pkg")
          (should (string-match-p "second-pkg" (with-current-buffer buf (buffer-string))))
          (with-current-buffer buf (goto-char (point-max)) (insert ";; unsaved\n"))
          (should-error (ide-user-add-package "third-pkg") :type 'user-error)
          (should-not (string-match-p "third-pkg" (with-temp-buffer (insert-file-contents file) (buffer-string))))
          (with-current-buffer buf (set-buffer-modified-p nil))
          (kill-buffer buf)))
      (should-not prompted))))

(ert-deftest test-user-new-file-template ()
  (smoke--with-user-dir
    (let ((f (ide-user-new-file "rust")))
      (should (equal f (expand-file-name "rust.el" tmp)))
      (should (string-match-p "lexical-binding: t" (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (should-error (ide-user-new-file "rust") :type 'user-error)
      (should-error (ide-user-new-file "../evil") :type 'user-error)
      (kill-buffer (find-buffer-visiting f)))))

(ert-deftest test-user-examples-install-and-merge-tools ()
  (smoke--with-user-dir
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should (member "rust" (ide-user--example-names)))
      (let ((copied (ide-user-install-example "rust")))
        (should (equal (mapcar #'file-name-nondirectory copied) '("rust.el")))
        (kill-buffer (find-buffer-visiting (car copied))))
      (let ((toml (expand-file-name "mise.toml" tmp)))
        (should (string-match-p "rust-analyzer" (with-temp-buffer (insert-file-contents toml) (buffer-string))))
        ;; existing user work is kept, tools are merged without duplicates
        (with-temp-file toml (insert "# mine\n[tools]\njq = \"latest\"\nrust = \"1.80\"\n"))
        (ide-user-install-example "rust")
        (ide-user-install-example "shell-tools")
        (let ((text (with-temp-buffer (insert-file-contents toml) (buffer-string))))
          (should (string-match-p "# mine" text))
          (should (string-match-p "rust = \"1.80\"" text))
          (should-not (string-match-p "rust = \"latest\"" text))
          (should (= 1 (with-temp-buffer (insert text) (count-matches "^\\[tools\\]" (point-min) (point-max)))))
          (dolist (tool '("jq" "rust-analyzer" "shellcheck" "shfmt")) (should (string-match-p tool text)))))
      ;; a file the user already has is never overwritten
      (with-temp-file (expand-file-name "rust.el" tmp) (insert ";; mine\n"))
      (ide-user-install-example "rust")
      (should (equal (with-temp-buffer (insert-file-contents (expand-file-name "rust.el" tmp)) (buffer-string)) ";; mine\n")))))

(ert-deftest test-user-shipped-examples-are-valid ()
  "Every example reads and expands, and has a title line (the docs show them)."
  (dolist (f (directory-files ide-examples-dir t "\\.example\\'"))
    (let ((text (with-temp-buffer (insert-file-contents f) (buffer-string))))
      (cond ((string-suffix-p ".el.example" f)
             (should (string-match-p "\\`;;; [^ ]+ --- .+ -\\*- lexical-binding: t; -\\*-" text))
             (with-temp-buffer (insert text) (goto-char (point-min))
                               (condition-case nil (while t (macroexpand-all (read (current-buffer)))) (end-of-file nil))))
            (t (should (string-match-p "\\`# .+\n" text))
               (should (string-match-p "^\\[tools\\]" text)))))))

(ert-deftest test-user-drawer-helper ()
  (let ((display-buffer-alist display-buffer-alist)
        (ide-bottom-buffer-regexps (copy-sequence ide-bottom-buffer-regexps))
        (popper-reference-buffers (and (boundp 'popper-reference-buffers) (copy-sequence popper-reference-buffers))))
    (ide-add-drawer-buffer "\\*my-tool\\*")
    (ide-add-drawer-buffer "\\*my-tool\\*")
    (should (= 1 (cl-count "\\*my-tool\\*" ide-bottom-buffer-regexps :test #'equal)))
    ;; popper re-inserts its own rule when it restarts, so check presence, not position
    (let ((rule (assoc "\\*my-tool\\*" display-buffer-alist)))
      (should rule)
      (should (eq (cdr (assq 'side (cddr rule))) 'bottom)))
    (should (member "\\*my-tool\\*" popper-reference-buffers))))

(ert-deftest test-safe-mode-launcher ()
  "The launcher and the desktop entry both offer a safe mode that skips user/."
  (let ((launch (expand-file-name "launch.sh" user-emacs-directory))
        (desktop (expand-file-name "foundry.desktop.in" user-emacs-directory)))
    (should (string-match-p "--safe" (with-temp-buffer (insert-file-contents launch) (buffer-string))))
    (let ((text (with-temp-buffer (insert-file-contents desktop) (buffer-string))))
      (should (string-match-p "^Name=Foundry$" text))
      (should (string-match-p "^Actions=safe-mode;" text))
      (should (string-match-p "^\\[Desktop Action safe-mode\\]" text))
      (should (string-match-p "^Exec=.*launch.sh --safe" text)))
    (should (equal "1" (car (process-lines launch "--safe" "--batch" "--eval" "(princ (getenv \"IDE_NO_USER\"))"))))))

(ert-deftest test-safe-mode-skips-user-files-and-says-so ()
  (smoke--with-user-dir
    (with-temp-file (expand-file-name "a.el" tmp) (insert "(push 'a smoke--user-log)"))
    (setq smoke--user-log nil)
    (let ((process-environment (cons "IDE_NO_USER=1" process-environment)) (emacs-startup-hook nil))
      (should (ide-user-safe-mode-p))
      (should (= 0 (ide-user-load-all)))
      (should emacs-startup-hook))
    (should-not smoke--user-log)))

(ert-deftest test-mise-never-reads-configuration-above-the-ide-directory ()
  "Real mise: tools from a parent directory's .tool-versions / mise.toml must not leak in,
while the core and user manifests still merge."
  (skip-unless (ide-mise-executable))
  (let* ((mise (ide-mise-executable))   ; found before the test rebinds the sandbox directories
         (top (file-name-as-directory (make-temp-file "mise-leak" t)))
         (cfg (file-name-as-directory (expand-file-name "cfg" top)))
         (user-emacs-directory cfg)
         (ide-mise-dir (expand-file-name "var/mise/" cfg))
         (ide-mise-manifest (expand-file-name "mise.toml" cfg)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "user" cfg) t)
          (with-temp-file (expand-file-name ".tool-versions" top) (insert "shellcheck 0.10.0\n"))
          (with-temp-file (expand-file-name "mise.toml" top) (insert "[tools]\nshfmt = \"3.8.0\"\n"))
          (with-temp-file ide-mise-manifest (insert "[tools]\njq = \"1.7.1\"\n"))
          (with-temp-file (expand-file-name "user/mise.toml" cfg) (insert "[tools]\ntaplo = \"0.9.3\"\n"))
          (let* ((process-environment (append (ide-mise--env) process-environment))
                 (default-directory (ide-mise--run-dir))
                 (out (nth 1 (ide-run-with-timeout 60 mise "ls"))))
            (should (string-match-p "\\<jq\\>" out))
            (should (string-match-p "\\<taplo\\>" out))
            (should-not (string-match-p "shellcheck" out))
            (should-not (string-match-p "shfmt" out))))
      (delete-directory top t))))

(ert-deftest test-user-mise-integration ()
  "mise runs from user/ with the project trusted, so user/mise.toml merges with the core manifest."
  (should (equal (ide-mise--run-dir) (expand-file-name "user/" user-emacs-directory)))
  (should (cl-some (lambda (e) (string-prefix-p "MISE_TRUSTED_CONFIG_PATHS=" e)) (ide-mise--env)))
  (should (string-match-p "ide-user-load-all" (with-temp-buffer (insert-file-contents (expand-file-name "init.el" user-emacs-directory)) (buffer-string)))))

(ert-deftest test-two-space-indent-policy ()
  "YAML (and other offset-less modes) indent by 2; Python keeps PEP 8's 4."
  (should (= 2 standard-indent))
  (with-temp-buffer
    (yaml-ts-mode)
    (should (= 2 tab-width))
    (insert "a:\n")
    (indent-for-tab-command)
    (should (= 2 (current-column))))
  (with-temp-buffer
    (python-ts-mode)
    (should (= 4 python-indent-offset))))

(ert-deftest test-keybindings ()
  "IDE keys resolve to the expected commands."
  (should (eq (key-binding (kbd "C-c w 1")) 'ide-toggle-project-tree))
  (should (eq (key-binding (kbd "C-c w 7")) 'ide-toggle-structure))
  (should (eq (key-binding (kbd "C-c w 9")) 'magit-status))
  (should (eq (key-binding (kbd "M-.")) 'xref-find-definitions))
  (should (eq (lookup-key prog-mode-map (kbd "M-RET")) 'eglot-code-actions))
  (should (eq (key-binding (kbd "C-c d o")) 'azdo-dispatch)))

(ert-deftest test-mise-helpers ()
  "Pure helpers of the mise installer."
  (let ((sums "abc  ./other\n6eb1b890e90818417ca34c90dbbd47881917d5cd199f31b63b062ea9c6b18d85  ./mise-v1-linux-x64\n"))
    (should (equal (ide-mise--parse-shasums sums "mise-v1-linux-x64")
                   "6eb1b890e90818417ca34c90dbbd47881917d5cd199f31b63b062ea9c6b18d85"))
    (should (null (ide-mise--parse-shasums sums "mise-v1-macos-x64"))))
  (should (string-match-p "\\`\\(linux\\|macos\\)-\\(x64\\|arm64\\)\\'" (or (ide-mise--platform) "")))
  (let ((exec-path exec-path) (process-environment (copy-sequence process-environment))
        (dir temporary-file-directory))
    (ide-mise--merge-path (list dir "/nonexistent-dir"))
    (should (equal (car exec-path) dir))
    (should-not (member "/nonexistent-dir" exec-path)))
  (let* ((global "/home/u/.local/share/mise/installs/go/1.27/bin")
         (ours (expand-file-name "data/installs/go/1/bin" ide-mise-dir))
         (exec-path (list global ours "/usr/bin"))
         (process-environment (cons (concat "PATH=" global ":/home/u/.local/share/mise/shims:/usr/bin")
                                    process-environment)))
    (ide-mise--merge-path nil)
    (should (equal exec-path (list ours "/usr/bin")))
    (should (equal (getenv "PATH") "/usr/bin")))
  (should (file-readable-p ide-mise-manifest))
  (should (member "MISE_YES=1" (ide-mise--env)))
  (should (cl-some (lambda (e) (string-prefix-p (concat "MISE_DATA_DIR=" (expand-file-name user-emacs-directory)) e))
                   (ide-mise--env))))

;;;; Regression tests for the code-review fixes -------------------------------

(require 'cl-lib)

(defun smoke--git (dir &rest args)
  (let ((default-directory dir)
        (process-environment (append '("GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                                       "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t"
                                       "GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null")
                                     process-environment)))
    (with-temp-buffer
      (unless (zerop (apply #'call-process "git" nil t nil args))
        (error "git %S failed: %s" args (buffer-string))))))

(ert-deftest test-babel-confirms-emacs-lisp ()
  "Executable languages must prompt; data-only ones need not."
  (load (expand-file-name "modules/14-org/org-babel.el" user-emacs-directory) nil t)
  (should (ide-org-confirm-babel "emacs-lisp" ""))
  (should (ide-org-confirm-babel "python" ""))
  (should-not (ide-org-confirm-babel "json" "")))

(ert-deftest test-run-with-timeout ()
  "Bounded subprocess helper returns output, exit codes and kills hung children."
  (should (equal (ide-run-with-timeout 5 "sh" "-c" "echo hi; echo oops >&2; exit 3")
                 '(3 "hi\n" "oops\n")))
  (let ((start (float-time)))
    (should (eq (car (ide-run-with-timeout 0.4 "sleep" "10")) 'timeout))
    (should (< (- (float-time) start) 3)))
  (should (null (car (ide-run-with-timeout 1 "/nonexistent/program")))))

(ert-deftest test-azdo-wiql-quote ()
  "Apostrophes in project names cannot break out of the WIQL literal."
  (should (equal (azdo--wiql-quote "O'Brien") "O''Brien"))
  (should (equal (azdo--wiql-quote "x' OR 1=1 --") "x'' OR 1=1 --"))
  (cl-letf (((symbol-function 'azdo--ctx!) (lambda () '(:project "a' b"))))
    (should (string-match-p "TeamProject\\] = 'a'' b'" (azdo--assigned-wiql)))))

(ert-deftest test-azdo-cache-keeps-data-on-failure ()
  "A failed refresh must not replace good cached data with nil."
  (let ((azdo--cache (make-hash-table :test #'equal)))
    (azdo--cache-store 'k nil '(a b) nil)
    (should (equal (cdr (gethash 'k azdo--cache)) '(a b)))
    (azdo--cache-store 'k '(a b) azdo--failed nil)
    (should (equal (cdr (gethash 'k azdo--cache)) '(a b)))
    ;; A genuinely empty result is still stored.
    (azdo--cache-store 'k '(a b) nil nil)
    (should (null (cdr (gethash 'k azdo--cache))))))

(ert-deftest test-azdo-async-failure-and-timeout ()
  "Async requests report `azdo--failed' for unreachable hosts instead of nil."
  (let ((got 'unset) (azdo-request-timeout 4))
    (cl-letf (((symbol-function 'azdo--auth-header) (lambda () "Basic x")))
      (azdo--request-async "GET" "http://127.0.0.1:1/x" (lambda (d) (setq got d))))
    (let ((deadline (+ (float-time) 8)))
      (while (and (eq got 'unset) (< (float-time) deadline)) (accept-process-output nil 0.1)))
    (should (eq got azdo--failed))))

(ert-deftest test-azdo-ctx-cached ()
  "Repository context is computed once, not on every call."
  (let* ((dir (file-name-as-directory (make-temp-file "azrepo" t)))
         (default-directory dir)
         (azdo--ctx-cache (make-hash-table :test #'equal))
         (spawns 0))
    (smoke--git dir "init" "-q" ".")
    (smoke--git dir "remote" "add" "origin" "https://dev.azure.com/acme/proj/_git/web")
    (advice-add 'process-lines :before (lambda (&rest _) (cl-incf spawns)) '((name . smoke-count)))
    (unwind-protect
        (progn
          (should (equal (plist-get (azdo--ctx) :repo) "web"))
          (let ((first spawns))
            (dotimes (_ 10) (azdo--ctx))
            (should (= spawns first))))
      (advice-remove 'process-lines 'smoke-count)
      (delete-directory dir t))))

(ert-deftest test-azdo-capf-never-blocks-on-network ()
  "Typing # must only use cached data and never issue a synchronous request."
  (let ((azdo--cache (make-hash-table :test #'equal)) (sync-called nil))
    (cl-letf (((symbol-function 'azdo--ctx) (lambda () '(:org "o" :project "p" :repo "r" :remote "origin")))
              ((symbol-function 'azdo--request) (lambda (&rest _) (setq sync-called t) nil))
              ((symbol-function 'azdo--fetch-work-items-async) #'ignore))
      (with-temp-buffer
        (insert "Fix bug #12")
        (azdo-commit-capf)
        (azdo--cache-store (azdo--cache-key 'work-items) nil
                           '(((id . 123) (fields . ((System.Title . "Do thing, with comma")))))
                           nil)
        (puthash (azdo--cache-key 'work-items)
                 (cons (current-time) '(((id . 123) (fields . ((System.Title . "Do thing")))))) azdo--cache)
        (let ((r (azdo-commit-capf)))
          (should (member "#123" (nth 2 r))))))
    (should-not sync-called)))

(ert-deftest test-azdo-work-item-selection-with-commas ()
  "Titles containing commas can be selected and linked."
  (let* ((items '(((id . 1) (fields . ((System.Title . "Fix a, b") (System.WorkItemType . "Bug") (System.State . "Active"))))
                  ((id . 2) (fields . ((System.Title . "Other") (System.WorkItemType . "Task") (System.State . "New"))))))
         (answers (list (azdo--wi-line (car items)) (azdo--wi-line (cadr items)) "")))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop answers))))
      (should (equal (azdo--read-work-item-ids items) '(1 2))))
    (should (null (azdo--read-work-item-ids nil)))))

(ert-deftest test-azdo-request-bodies ()
  "Request bodies match the Azure DevOps REST schema."
  (let ((c (azdo--comment-body '("src/a.py" 7 right) "hi")))
    (should (equal (alist-get 'filePath (alist-get 'threadContext c)) "/src/a.py"))
    (should (equal (alist-get 'line (alist-get 'rightFileStart (alist-get 'threadContext c))) 7))
    (should (null (alist-get 'leftFileStart (alist-get 'threadContext c))))
    (should (equal (aref (alist-get 'comments c) 0) '((parentCommentId . 0) (content . "hi") (commentType . 1)))))
  (should (alist-get 'leftFileStart (alist-get 'threadContext (azdo--comment-body '("a" 3 left) "x"))))
  (let* ((pr '((lastMergeSourceCommit . ((commitId . "abc")))))
         (b (azdo--completion-body pr "merge" t nil nil))
         (o (alist-get 'completionOptions b)))
    (should (equal (alist-get 'status b) "completed"))
    (should (equal (alist-get 'mergeStrategy o) "noFastForward"))
    (should (eq (alist-get 'deleteSourceBranch o) t))
    (should (eq (alist-get 'bypassPolicy o) :false))
    (should-not (assq 'bypassReason o))
    (should (equal (alist-get 'bypassReason (alist-get 'completionOptions (azdo--completion-body pr "squash" nil t "hotfix"))) "hotfix")))
  (let ((b (azdo--pr-body "feat" "main" "T" "D" t '("guid1") '(5 6))))
    (should (equal (alist-get 'sourceRefName b) "refs/heads/feat"))
    (should (eq (alist-get 'isDraft b) t))
    (should (equal (aref (alist-get 'workItemRefs b) 1) '((id . "6"))))
    (should (equal (aref (alist-get 'reviewers b) 0) '((id . "guid1") (isRequired . t)))))
  ;; Bodies must serialize to JSON.
  (should (stringp (json-serialize (azdo--pr-body "f" "m" "T" "D" nil nil nil))))
  (let (captured)
    (cl-letf (((symbol-function 'azdo--put) (lambda (_b path body &rest _) (setq captured (list path body))))
              ((symbol-function 'azdo--me-id) (lambda () "me"))
              ((symbol-function 'azdo--ctx!) (lambda () '(:org "o" :project "p" :repo "r" :remote "origin"))))
      (azdo--cast-vote 42 10))
    (should (equal captured '("/pullrequests/42/reviewers/me" ((vote . 10)))))))

(ert-deftest test-azdo-hunk-position-in-real-diff ()
  "Inline comments resolve to the right file, line and side in a Magit diff."
  (require 'magit)
  (require 'magit-diff)
  (let* ((dir (file-name-as-directory (make-temp-file "azdiff" t)))
         (default-directory dir)
         (process-environment (append '("GIT_CONFIG_GLOBAL=/dev/null" "GIT_CONFIG_SYSTEM=/dev/null"
                                        "GIT_AUTHOR_NAME=t" "GIT_AUTHOR_EMAIL=t@t"
                                        "GIT_COMMITTER_NAME=t" "GIT_COMMITTER_EMAIL=t@t")
                                      process-environment)))
    (unwind-protect
        (progn
          (smoke--git dir "init" "-q" ".")
          (make-directory (expand-file-name "src" dir))
          (with-temp-file (expand-file-name "src/a.txt" dir)
            (insert "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight\n"))
          (smoke--git dir "add" ".") (smoke--git dir "commit" "-q" "-m" "a")
          (with-temp-file (expand-file-name "src/a.txt" dir)
            (insert "one\ntwo\nTHREE\nfour\nfive\nsix\nseven\neight\nnine\n"))
          (smoke--git dir "commit" "-q" "-am" "b")
          (let ((magit-display-buffer-function #'display-buffer))
            (magit-diff-range "HEAD~1..HEAD"))
          (with-current-buffer (magit-get-mode-buffer 'magit-diff-mode)
            (goto-char (point-min))
            (re-search-forward "^\\+THREE") (beginning-of-line)
            (should (equal (azdo--hunk-position) '("src/a.txt" 3 right)))
            (goto-char (point-min))
            (re-search-forward "^-three") (beginning-of-line)
            (should (equal (azdo--hunk-position) '("src/a.txt" 3 left)))
            (goto-char (point-min))
            (re-search-forward "^\\+nine") (beginning-of-line)
            (should (equal (azdo--hunk-position) '("src/a.txt" 9 right)))
            (goto-char (point-min))
            (re-search-forward "^ five") (beginning-of-line)
            (should (equal (azdo--hunk-position) '("src/a.txt" 5 right)))))
      (ignore-errors (kill-buffer (magit-get-mode-buffer 'magit-diff-mode)))
      (delete-directory dir t))))

;; Regression: JIT native compilation of our own files ran in a clean subprocess
;; without our macros and produced "Cannot load popper" / "void variable project".
(ert-deftest test-own-files-not-native-compiled ()
  "Config files are excluded from native JIT compilation."
  (dolist (f '("modules/02-layout.el" "modules/09-project.el" "modules/12-devops/devops-azdo.el"
               "init.el" "early-init.el"))
    (let ((path (expand-file-name f user-emacs-directory)))
      (should (cl-some (lambda (re) (string-match-p re path))
                       native-comp-jit-compilation-deny-list))))
  (should-not (cl-some (lambda (re) (string-match-p re "/elpaca/builds/popper/popper.el"))
                       native-comp-jit-compilation-deny-list)))

(ert-deftest test-foundry-branding ()
  "The name shows in the frame title, the scratch buffer and the launcher, without claiming a `foundry-' namespace."
  (should (equal (car frame-title-format) "Foundry — "))
  (should (string-match-p "Foundry" initial-scratch-message))
  (should-not (featurep 'foundry))
  (let (clash)
    (mapatoms (lambda (s) (when (and (string-prefix-p "foundry-" (symbol-name s)) (or (fboundp s) (boundp s))) (push s clash))))
    (should-not clash)))

(ert-deftest test-ui-sanitized ()
  "Verify that GUI cruft is disabled."
  (should (not menu-bar-mode))
  (should (not tool-bar-mode))
  (should (not scroll-bar-mode)))


;;; smoke-test.el ends here
