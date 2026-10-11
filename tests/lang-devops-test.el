;;; lang-devops-test.el --- Tests for modules/11-languages/lang-devops.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-devops-test.el -f ert-run-tests-batch-and-exit
;; Pure logic plus the real linters and pandoc on small files (skipped when a tool is missing).
;; No Docker daemon is needed: commands are only built, never run.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defmacro ide-devops-test--with-buffer (mode text &rest body)
  "Run BODY in a temp buffer in MODE holding TEXT."
  (declare (indent 2))
  `(with-temp-buffer
     (insert ,text)
     (funcall ,mode)
     (goto-char (point-min))
     ,@body))

(defun ide-devops-test--lint (backend file mode &optional server)
  "Run Flymake BACKEND on FILE visited in MODE; return the diagnostics as (LINE TYPE TEXT)."
  (let ((buf (find-file-noselect file)) res done)
    (unwind-protect
        (with-current-buffer buf
          (delay-mode-hooks (funcall mode))
          (cl-letf (((symbol-function 'eglot-current-server) (lambda () server)))
            (funcall backend (lambda (d &rest _) (setq res d done t))))
          (dotimes (_ 100) (unless done (accept-process-output nil 0.1)))
          (should done)
          (mapcar (lambda (d) (list (line-number-at-pos (flymake-diagnostic-beg d))
                                    (flymake-diagnostic-type d) (flymake-diagnostic-text d)))
                  res))
      (kill-buffer buf))))

(defun ide-devops-test--tmp (name content)
  "Write CONTENT to NAME in a fresh temp directory; return the path."
  (let ((f (expand-file-name name (make-temp-file "devops-test" t))))
    (with-temp-file f (insert content))
    f))

;;;; Registration -----------------------------------------------------------------------

(ert-deftest devops-menus-and-hooks-registered ()
  (dolist (pair '((toml-ts-mode . ide-toml-dispatch)
                  (bash-ts-mode . ide-bash-dispatch) (dockerfile-ts-mode . ide-dockerfile-dispatch)
                  (terraform-mode . ide-terraform-dispatch)))
    (should (eq (cl-loop for (modes . cmd) in ide-lang-menus
                         when (memq (car pair) modes) return cmd)
                (cdr pair))))
  (should (memq #'ide-dockerfile--run-command ide-run-command-functions))
  (should (memq #'ide-compose--run-command ide-run-command-functions))
  (dolist (fn '(ide-lang-flymake-hadolint ide-lang-flymake-shellcheck
                ide-lang-flymake-tflint ide-lang-flymake-terraform-validate))
    (should (fboundp fn))))

(ert-deftest devops-restore-linters-after-eglot ()
  (with-temp-buffer
    (dockerfile-ts-mode)
    (setq-local flymake-diagnostic-functions '(eglot-flymake-backend))
    (ide-devops--restore-linters)
    (should (memq 'ide-lang-flymake-hadolint flymake-diagnostic-functions))
    (should (memq 'eglot-flymake-backend flymake-diagnostic-functions))))

;;;; TOML -------------------------------------------------------------------------------

(ert-deftest devops-toml-sort-keys ()
  (skip-unless (treesit-language-available-p 'toml))
  (ide-devops-test--with-buffer #'toml-ts-mode
      "[package]\nversion = \"1\"\n# the name\nname = \"x\"  # trailing\n\nz = 1\ny = [\n  1,\n  2,\n]\n\n[dependencies]\nb = 1\na = 2\n"
    (search-forward "version")
    (ide-toml-sort-keys)
    (should (equal (buffer-string)
                   "[package]\n# the name\nname = \"x\"  # trailing\nversion = \"1\"\n\ny = [\n  1,\n  2,\n]\nz = 1\n\n[dependencies]\nb = 1\na = 2\n"))
    (search-forward "b = 1")
    (ide-toml-sort-keys)
    (should (string-suffix-p "[dependencies]\na = 2\nb = 1\n" (buffer-string)))))

(ert-deftest devops-toml-sort-tables ()
  (skip-unless (treesit-language-available-p 'toml))
  (ide-devops-test--with-buffer #'toml-ts-mode
      "top = 1\n\n[zeta]\nk = 1\n\n# about alpha\n[alpha]\nk = 2\n\n[[bin]]\nn = 1\n\n[[bin]]\nn = 2\n"
    (ide-toml-sort-tables)
    (should (equal (buffer-string)
                   "top = 1\n\n# about alpha\n[alpha]\nk = 2\n\n[[bin]]\nn = 1\n\n[[bin]]\nn = 2\n\n[zeta]\nk = 1\n"))))

(ert-deftest devops-toml-schema-directive ()
  (let ((buffer-file-name "/p/mise.toml")) (should (equal (ide-toml--default-schema) "mise")))
  (let ((buffer-file-name "/p/Cargo.toml")) (should (equal (ide-toml--default-schema) "Cargo")))
  (with-temp-buffer
    (insert "[tools]\n")
    (ide-toml-insert-schema "mise")
    (ide-toml-insert-schema "mise")
    (should (equal (buffer-string) "#:schema https://mise.jdx.dev/schema/mise.json\n[tools]\n"))))

;;;; Shell ------------------------------------------------------------------------------

(ert-deftest devops-bash-shfmt-flags-defer-to-editorconfig ()
  (cl-letf (((symbol-function 'ide-devops--local-config) (lambda (&rest _) nil)))
    (should (equal (ide-bash--shfmt-flags) '("-i" "2" "-ci"))))
  (cl-letf (((symbol-function 'ide-devops--local-config) (lambda (&rest _) "/p/.editorconfig")))
    (should-not (ide-bash--shfmt-flags))))

(ert-deftest devops-bash-shellcheck-parser ()
  (with-temp-buffer
    (insert "#!/bin/bash\necho $x\n")
    (let ((d (ide-bash--parse-shellcheck
              "[{\"line\":2,\"endLine\":2,\"column\":6,\"endColumn\":8,\"level\":\"info\",\"code\":2086,\"message\":\"Double quote\"}]"
              (current-buffer))))
      (should (equal (flymake-diagnostic-text (car d)) "shellcheck: [SC2086]: Double quote"))
      (should (eq (flymake-diagnostic-type (car d)) :note))
      (should (equal (buffer-substring (flymake-diagnostic-beg (car d)) (flymake-diagnostic-end (car d))) "$x")))))

(ert-deftest devops-bash-help-for-builtins-and-programs ()
  (should (string-match-p "change the shell working directory\\|Change the shell working directory"
                          (ide-bash--help-text "cd")))
  (skip-unless (executable-find "ls"))
  (should (string-match-p "Usage" (or (ide-bash--help-text "ls") ""))))

(ert-deftest devops-bash-command-at-point ()
  (skip-unless (treesit-language-available-p 'bash))
  (ide-devops-test--with-buffer #'bash-ts-mode "cd /tmp && grep -r foo .\n"
    (search-forward "grep")
    (should (equal (ide-bash--command-at-point) "grep"))))

(ert-deftest devops-bash-shellcheck-code-wiki-link ()
  (let (url)
    (cl-letf (((symbol-function 'ide-lang-browse) (lambda (u) (setq url u))))
      (ide-bash-shellcheck-wiki "SC2086")
      (should (equal url "https://www.shellcheck.net/wiki/SC2086"))
      (should-error (ide-bash-shellcheck-wiki "nope") :type 'user-error))))

(ert-deftest devops-shellcheck-and-bash-ls-do-not-duplicate ()
  "The own shellcheck backend stays quiet while a language server is connected."
  (skip-unless (executable-find "shellcheck"))
  (let ((f (ide-devops-test--tmp "a.sh" "#!/bin/bash\necho $x\n")))
    (should (cl-find "SC2086" (ide-devops-test--lint #'ide-lang-flymake-shellcheck f #'bash-ts-mode)
                     :key #'caddr :test #'string-match-p))
    (should-not (ide-devops-test--lint #'ide-lang-flymake-shellcheck f #'bash-ts-mode 'server))))

;;;; Dockerfile and Compose -------------------------------------------------------------

(defconst ide-devops-test--dockerfile
  "ARG BASE=ubuntu:24.04\nFROM ${BASE} AS build\nARG VERSION=\"1.0\"\nARG NAME\nRUN make\nFROM scratch AS final\nCOPY --from=build /out /\n")

(ert-deftest devops-dockerfile-args-and-stages-from-tree-sitter ()
  (skip-unless (treesit-language-available-p 'dockerfile))
  (ide-devops-test--with-buffer #'dockerfile-ts-mode ide-devops-test--dockerfile
    (should (equal (ide-dockerfile--args)
                   '(("BASE" . "ubuntu:24.04") ("VERSION" . "1.0") ("NAME"))))
    (should (equal (ide-dockerfile--stages) '("build" "final")))))

(ert-deftest devops-dockerfile-build-command ()
  (should (equal (ide-dockerfile--build-argv "/p/app/Dockerfile" "app:dev" '(("A" . "1") ("B" . "x y")) "build")
                 '("docker" "build" "-f" "/p/app/Dockerfile" "-t" "app:dev" "--target" "build"
                   "--build-arg" "A=1" "--build-arg" "B=x y" "/p/app")))
  (should (equal (ide-dockerfile--default-tag "/p/My App/") "my-app:dev"))
  (let ((buffer-file-name "/p/svc/Dockerfile") (default-directory "/p/svc/"))
    (with-temp-buffer
      (dockerfile-ts-mode)
      (setq buffer-file-name "/p/svc/Dockerfile")
      (should (equal (ide-dockerfile--run-command)
                     "docker build -f /p/svc/Dockerfile -t svc:dev /p/svc")))))

(ert-deftest devops-dockerfile-build-asks-for-args-and-runs-in-terminal ()
  (skip-unless (treesit-language-available-p 'dockerfile))
  (let (ran term answers)
    (setq answers (list "myimg:1" "" "2.0" ""))
    (cl-letf (((symbol-function 'read-string) (lambda (&rest _) (pop answers)))
              ((symbol-function 'ide-devops--run) (lambda (_n cmd &rest _) (setq ran cmd)))
              ((symbol-function 'ide-devops--terminal) (lambda (_n cmd _d) (setq term cmd))))
      (with-temp-buffer
        (insert ide-devops-test--dockerfile)
        (dockerfile-ts-mode)
        (setq buffer-file-name "/p/svc/Dockerfile")
        (ide-dockerfile-build-with-args)
        (should (string-match-p "-t myimg:1 --build-arg VERSION=2.0 /p/svc\\'" ran))
        (should-not (string-match-p "BASE=" ran))
        (setq answers (list "t:1"))
        (ide-dockerfile-build-and-run)
        (should (string-match-p "docker build .* && docker run --rm -it t:1\\'" term))))))

(ert-deftest devops-dockerfile-bake-targets ()
  (let ((hcl (ide-devops-test--tmp "docker-bake.hcl" "group \"default\" {\n  targets = [\"app\"]\n}\ntarget \"app\" {\n}\ntarget \"web\" {\n}\n"))
        (json (ide-devops-test--tmp "docker-bake.json" "{\"target\":{\"a\":{},\"b\":{}}}"))
        (compose (ide-devops-test--tmp "compose.yaml" "services:\n  web:\n    image: nginx\n  db:\n    image: postgres\nnetworks:\n  net:\n")))
    (should (equal (ide-dockerfile--bake-targets hcl) '("app" "web")))
    (should (equal (ide-dockerfile--bake-targets json) '("a" "b")))
    (should (equal (ide-dockerfile--bake-targets compose) '("web" "db")))))

(ert-deftest devops-dockerfile-hadolint-wiki-links ()
  (let (url diag)
    (cl-letf (((symbol-function 'ide-lang-browse) (lambda (u) (setq url u)))
              ((symbol-function 'flymake-diagnostics) (lambda (&rest _) diag)))
      (with-temp-buffer
        (insert "FROM ubuntu\n")
        ;; No diagnostic on empty line -> base wiki
        (ide-dockerfile-hadolint-wiki)
        (should (equal url "https://github.com/hadolint/hadolint/wiki"))
        ;; With DL diagnostic
        (setq diag (list (flymake-make-diagnostic (current-buffer) 1 5 :warning "hadolint: DL3006: tag it")))
        (ide-dockerfile-hadolint-wiki)
        (should (equal url "https://github.com/hadolint/hadolint/wiki/DL3006"))
        ;; With SC diagnostic
        (setq diag (list (flymake-make-diagnostic (current-buffer) 1 5 :warning "hadolint: SC2086: quote it")))
        (ide-dockerfile-hadolint-wiki)
        (should (equal url "https://www.shellcheck.net/wiki/SC2086"))))))

(ert-deftest devops-terminal-creates-drawer-ghostel-buffer ()
  (skip-unless (fboundp 'ghostel))
  (let ((buf (ide-devops--terminal "my-service" "echo test" default-directory)))
    (unwind-protect
        (progn
          (should (buffer-live-p buf))
          (should (string-prefix-p "*ghostel: my-service" (buffer-name buf)))
          (with-current-buffer buf
            (should (derived-mode-p 'ghostel-mode))
            (should-not ghostel-kill-buffer-on-exit)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest devops-hadolint-parser-maps-levels ()
  (with-temp-buffer
    (insert "FROM x\nRUN y\n")
    (let ((d (ide-dockerfile--parse-hadolint
              "[{\"code\":\"DL3006\",\"column\":1,\"level\":\"warning\",\"line\":1,\"message\":\"tag it\"},{\"code\":\"DL3015\",\"column\":1,\"level\":\"info\",\"line\":2,\"message\":\"x\"},{\"code\":\"DL1\",\"level\":\"error\",\"line\":2,\"message\":\"e\"}]"
              (current-buffer))))
      (should (equal (mapcar #'flymake-diagnostic-type d) '(:warning :note :error)))
      (should (equal (flymake-diagnostic-text (car d)) "hadolint: DL3006: tag it")))))

(ert-deftest devops-hadolint-real-run-honours-config ()
  (skip-unless (executable-find "hadolint"))
  (let* ((dir (make-temp-file "hl" t))
         (f (expand-file-name "Dockerfile" dir)))
    (with-temp-file f (insert "FROM ubuntu\nCMD echo hi\n"))
    (let ((diags (ide-devops-test--lint #'ide-lang-flymake-hadolint f #'dockerfile-ts-mode)))
      (should (cl-find "DL3006" diags :key #'caddr :test #'string-match-p))
      (should (cl-find "DL3025" diags :key #'caddr :test #'string-match-p)))
    (with-temp-file (expand-file-name ".hadolint.yaml" dir) (insert "ignored:\n  - DL3006\n"))
    (let ((diags (ide-devops-test--lint #'ide-lang-flymake-hadolint f #'dockerfile-ts-mode)))
      (should-not (cl-find "DL3006" diags :key #'caddr :test #'string-match-p))
      (should (cl-find "DL3025" diags :key #'caddr :test #'string-match-p)))))

(ert-deftest devops-compose-detection-and-commands ()
  (dolist (n '("compose.yaml" "/x/docker-compose.yml" "/x/compose.prod.yaml" "docker-compose.override.yml"))
    (should (string-match-p ide-compose-file-regexp n)))
  (should-not (string-match-p ide-compose-file-regexp "/x/my-compose-notes.yaml"))
  (should (equal (ide-compose--argv "/p/compose.yaml" "up" "-d")
                 '("docker" "compose" "-f" "/p/compose.yaml" "up" "-d")))
  (with-temp-buffer
    (setq buffer-file-name "/p/compose.yaml")
    (ide-compose-mode 1)
    (should (equal (ide-compose--run-command) "docker compose -f /p/compose.yaml up --build"))))

(ert-deftest devops-compose-config-validates-without-a-daemon ()
  (skip-unless (and (executable-find "docker")
                    (zerop (call-process "docker" nil nil nil "compose" "version"))))
  (let ((good (ide-devops-test--tmp "compose.yaml" "services:\n  web:\n    image: nginx\n  db:\n    image: postgres\n"))
        (bad (ide-devops-test--tmp "compose.yaml" "services:\n  web:\n    imag: nginx\n")))
    (should (zerop (apply #'call-process (car (ide-compose--argv good "config" "-q")) nil nil nil
                          (cdr (ide-compose--argv good "config" "-q")))))
    (should-not (zerop (apply #'call-process "docker" nil nil nil (cdr (ide-compose--argv bad "config" "-q")))))
    (with-current-buffer (find-file-noselect good)
      (unwind-protect (should (equal (sort (ide-compose--services) #'string<) '("db" "web")))
        (kill-buffer)))))

(ert-deftest devops-compose-interactive-actions ()
  (let (runs terms)
    (cl-letf (((symbol-function 'ide-compose--run) (lambda (name &rest args) (push (cons name args) runs)))
              ((symbol-function 'ide-devops--terminal) (lambda (name cmd dir) (push (list name cmd dir) terms)))
              ((symbol-function 'ide-compose--services) (lambda () '("web" "db"))))
      (with-temp-buffer
        (setq buffer-file-name "/p/compose.yaml")
        (ide-compose-mode 1)
        (ide-compose-up-foreground)
        (should (equal (car runs) '("up" "up" "--build")))
        (ide-compose-restart)
        (should (equal (car runs) '("restart" "restart")))
        (ide-compose-logs "web")
        (should (equal (car runs) '("logs" "logs" "-f" "--tail" "200" "web")))
        (ide-compose-exec "web")
        (should (equal (caar terms) "compose exec web"))
        (should (string-match-p "exec web sh -c" (cadar terms)))))))

;;;; Terraform --------------------------------------------------------------------------

(ert-deftest devops-terraform-imenu-groups-blocks ()
  (with-temp-buffer
    (insert "terraform {\n}\nvariable \"region\" {\n}\nresource \"aws_instance\" \"web\" {\n}\nresource \"aws_s3_bucket\" \"b\" {\n}\noutput \"ip\" {\n}\nlocals {\n}\n")
    (let ((idx (ide-terraform-imenu-index)))
      (should (equal (mapcar #'car idx) '("Terraform" "Variables" "Resources" "Outputs" "Locals")))
      (should (equal (mapcar #'car (cdr (assoc "Resources" idx))) '("aws_instance.web" "aws_s3_bucket.b")))
      (should (= (marker-position (cdr (cadr (assoc "Variables" idx)))) 15)))))

(ert-deftest devops-terraform-tflint-argv-and-parser ()
  (let ((argv (ide-terraform--tflint-argv "/p/mod/main.tf")))
    (should (string-prefix-p "TFLINT_PLUGIN_DIR=" (car argv)))
    (should (member "--chdir=/p/mod" argv))
    (should (member "json" argv)))
  (let* ((f (ide-devops-test--tmp "main.tf" "variable \"a\" {}\nresource \"x\" \"y\" {}\n"))
         (buf (find-file-noselect f))
         (json (format "{\"issues\":[{\"rule\":{\"name\":\"r1\",\"severity\":\"warning\"},\"message\":\"m1\",\"range\":{\"filename\":\"main.tf\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":9}}},{\"rule\":{\"name\":\"r2\",\"severity\":\"error\"},\"message\":\"other file\",\"range\":{\"filename\":\"other.tf\",\"start\":{\"line\":1,\"column\":1},\"end\":{\"line\":1,\"column\":2}}}],\"errors\":[]}")))
    (unwind-protect
        (let ((d (ide-terraform--parse-tflint json buf)))
          (should (= (length d) 1))
          (should (equal (flymake-diagnostic-text (car d)) "tflint: m1 (r1)")))
      (kill-buffer buf))))

(ert-deftest devops-terraform-validate-parser ()
  (let* ((f (ide-devops-test--tmp "main.tf" "a\nb\n"))
         (buf (find-file-noselect f))
         (json "{\"diagnostics\":[{\"severity\":\"error\",\"summary\":\"Bad\",\"detail\":\"why\\nmore\",\"range\":{\"filename\":\"main.tf\",\"start\":{\"line\":2,\"column\":1},\"end\":{\"line\":2,\"column\":2}}},{\"severity\":\"error\",\"summary\":\"Missing provider\",\"detail\":\"run init\"}]}"))
    (unwind-protect
        (let ((d (ide-terraform--parse-validate json buf)))
          (should (= (length d) 2))
          (should (equal (flymake-diagnostic-text (car d)) "terraform: Bad: why"))
          (should (eq (flymake-diagnostic-type (cadr d)) :note)))
      (kill-buffer buf))))

(ert-deftest devops-tflint-real-run ()
  (skip-unless (and (executable-find "tflint") (executable-find "env")))
  (let* ((f (ide-devops-test--tmp "main.tf" "variable \"unused\" {\n  type = string\n}\n"))
         (diags (ide-devops-test--lint #'ide-lang-flymake-tflint f #'terraform-mode)))
    (should (cl-find "terraform_unused_declarations" diags :key #'caddr :test #'string-match-p))
    (should (equal (car (cl-find "unused" diags :key #'caddr :test #'string-match-p)) 1))))

(ert-deftest devops-terraform-validate-real-run ()
  (skip-unless (executable-find "terraform"))
  (let* ((f (ide-devops-test--tmp "main.tf" "variable \"name\" {\n  type = string\n}\nresource \"terraform_data\" \"x\" {\n  input = var.nme\n}\n"))
         (ide-terraform-validate-backend t)
         ;; validate starts provider binaries, so the backend waits for a trusted project.
         (trusted-content :all)
         (diags (ide-devops-test--lint #'ide-lang-flymake-terraform-validate f #'terraform-mode)))
    (should (equal (mapcar #'car diags) '(5)))
    (should (string-match-p "undeclared input variable" (caddr (car diags))))))

(provide 'lang-devops-test)
;;; lang-devops-test.el ends here
