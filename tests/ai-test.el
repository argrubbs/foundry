;;; ai-test.el --- Tests for provider and model selection -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/ai-test.el
;; No network and no API keys: Ollama probing and keys are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'gptel)

(defmacro ai-test--with-state (&rest body)
  "Run BODY with isolated gptel defaults, backend cache, model file and environment."
  `(let ((saved-backend (default-value 'gptel-backend)) (saved-model (default-value 'gptel-model))
         (ide-ai--backends nil) (ide-ai--ollama-cache nil)
         (ide-ai-model-file (make-temp-file "ai-model" nil ".eld"))
         (ide-ai-default-model nil) (ide-ai-remember-model t)
         (process-environment (append '("ANTHROPIC_API_KEY=" "GEMINI_API_KEY=") process-environment)))
     (delete-file ide-ai-model-file)
     (unwind-protect (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil))) ,@body)
       (setq-default gptel-backend saved-backend gptel-model saved-model)
       (when (file-exists-p ide-ai-model-file) (delete-file ide-ai-model-file)))))

(defmacro ai-test--ollama (running models &rest body)
  "Pretend Ollama is RUNNING (or not) with MODELS installed."
  (declare (indent 2))
  `(cl-letf (((symbol-function 'ide-ai--ollama-probe) (lambda () (cons ,running ,models))))
     ,@body))

(defun ai-test--picks (&rest answers)
  "A `completing-read' stand-in answering with ANSWERS; records (PROMPT DEFAULT) calls."
  (setq ai-test--asked nil)
  (lambda (prompt coll _pred _req _init _hist default &rest _)
    (push (list prompt default (let ((md (funcall coll "" nil 'metadata))) (and md t))) ai-test--asked)
    (pop answers)))
(defvar ai-test--asked nil)

;;;; Registry and status ----------------------------------------------------------------------------

(ert-deftest ai-providers-are-well-formed ()
  (dolist (p ide-ai-providers)
    (should (symbolp (car p)))
    (should (stringp (ide-ai--pget p :name)))
    (should (fboundp (ide-ai--pget p :make)))
    (should (or (ide-ai--pget p :models) (ide-ai--pget p :models-fn)))))

(ert-deftest ai-status-reflects-keys-and-server ()
  (ai-test--with-state
    (let ((claude (ide-ai--provider 'claude)) (ollama (ide-ai--provider 'ollama)))
      (should-not (ide-ai--ready-p claude))
      (should (equal (ide-ai--status claude) "needs ANTHROPIC_API_KEY"))
      (let ((process-environment (cons "ANTHROPIC_API_KEY=sk-test" process-environment)))
        (should (ide-ai--ready-p claude))
        (should (equal (ide-ai--status claude) "ready")))
      (ai-test--ollama nil nil (should (equal (ide-ai--status ollama) "not running")))
      (ai-test--ollama t '(a) (should (equal (ide-ai--status ollama) "ready"))))))

(ert-deftest ai-api-key-falls-back-to-auth-source ()
  (ai-test--with-state
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args) (should (equal (plist-get args :host) "api.anthropic.com"))
                 (list (list :secret (lambda () "from-auth-source"))))))
      (should (equal (ide-ai--api-key (ide-ai--provider 'claude)) "from-auth-source")))
    (should-not (ide-ai--api-key (ide-ai--provider 'ollama)))))

(ert-deftest ai-ollama-models-come-from-the-server-or-the-default ()
  (ai-test--with-state
    (cl-letf (((symbol-function 'ide-run-with-timeout)
               (lambda (&rest _) (list 0 "{\"models\":[{\"name\":\"qwen2.5-coder:14b\"},{\"name\":\"llama3.2:3b\"}]}" ""))))
      (should (equal (ide-ai--ollama-models) '(qwen2.5-coder:14b llama3.2:3b))))
    (setq ide-ai--ollama-cache nil)
    (cl-letf (((symbol-function 'ide-run-with-timeout) (lambda (&rest _) (list 7 "" "connection refused"))))
      (should (equal (ide-ai--ollama-models) (list (intern ide-ai-ollama-model))))
      (should-not (car (ide-ai--ollama-probe))))
    (setq ide-ai--ollama-cache nil)
    (cl-letf (((symbol-function 'ide-run-with-timeout) (lambda (&rest _) (list 0 "<html>not json</html>" ""))))
      (should (equal (ide-ai--ollama-models) (list (intern ide-ai-ollama-model)))))))

;;;; Selection ---------------------------------------------------------------------------------------------

(ert-deftest ai-select-offers-current-defaults-and-annotations ()
  (ai-test--with-state
    (ai-test--ollama t '(qwen2.5-coder:7b llama3.2:3b)
      (ide-ai--apply (ide-ai--provider 'ollama) 'qwen2.5-coder:7b 'global)
      (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Ollama (local)" "llama3.2:3b")))
        (let ((sel (ide-ai-select-model)))
          (should (eq (car (car sel)) 'ollama))
          (should (eq (cdr sel) 'llama3.2:3b))
          (let ((asked (reverse ai-test--asked)))
            (should (string-match-p "Provider" (nth 0 (nth 0 asked))))
            (should (equal (nth 1 (nth 0 asked)) "Ollama (local)"))   ; current provider preselected
            (should (equal (nth 1 (nth 1 asked)) "qwen2.5-coder:7b")) ; current model first and default
            (should (nth 2 (nth 0 asked)))))))))                     ; annotated completion table

(ert-deftest ai-select-accepts-a-typed-model-id ()
  (ai-test--with-state
    (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Claude (Anthropic)" "claude-my-custom-1")))
      (let ((sel (ide-ai-select-model)))
        (should (eq (car (car sel)) 'claude))
        (should (eq (cdr sel) 'claude-my-custom-1))))))

(ert-deftest ai-picker-annotations-show-status ()
  (ai-test--with-state
    (ai-test--ollama nil nil
      (let* ((named (mapcar (lambda (p) (cons (ide-ai--pget p :name) p)) ide-ai-providers))
             (annot (lambda (c) (format " — %s" (ide-ai--status (cdr (assoc c named)))))))
        (should (equal (funcall annot "Claude (Anthropic)") " — needs ANTHROPIC_API_KEY"))
        (should (equal (funcall annot "Ollama (local)") " — not running"))))))

;;;; Applying ---------------------------------------------------------------------------------------------------

(ert-deftest ai-global-and-buffer-scope ()
  (ai-test--with-state
    (let ((claude (ide-ai--provider 'claude)))
      (ide-ai--apply (ide-ai--provider 'ollama) 'qwen2.5-coder:7b 'global)
      (with-temp-buffer
        (ide-ai--apply claude 'claude-sonnet-5-5 'buffer)
        (should (eq gptel-model 'claude-sonnet-5-5))
        (should (equal (gptel-backend-name gptel-backend) "Claude")))
      (should (eq (default-value 'gptel-model) 'qwen2.5-coder:7b))
      (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Ollama"))
      (ide-ai--apply claude 'claude-haiku-5-5 'global)
      (should (eq (default-value 'gptel-model) 'claude-haiku-5-5)))))

(ert-deftest ai-custom-models-are-added-to-the-backend ()
  (ai-test--with-state
    (let* ((p (ide-ai--provider 'claude))
           (b (ide-ai--apply p 'claude-brand-new-9 'global '(claude-brand-new-9))))
      (should (memq 'claude-brand-new-9 (gptel-backend-models b)))
      (should (eq b (ide-ai--backend p '(claude-sonnet-5-5))))        ; same backend object is reused
      (should (memq 'claude-brand-new-9 (gptel-backend-models b)))    ; and nothing is lost
      (should (memq 'claude-sonnet-5-5 (gptel-backend-models b))))))

(ert-deftest ai-set-model-scope-and-memory ()
  (ai-test--with-state
    (let ((process-environment (cons "ANTHROPIC_API_KEY=sk-test" process-environment)))
      (ai-test--ollama t '(qwen2.5-coder:7b)
        (ide-ai--apply (ide-ai--provider 'ollama) 'qwen2.5-coder:7b 'global)
        ;; Inside a chat/agent buffer: that session only, nothing remembered.
        (with-temp-buffer
          (setq-local gptel-mode t)
          (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Claude (Anthropic)" "claude-opus-5-5")))
            (ide-ai-set-model))
          (should (eq gptel-model 'claude-opus-5-5)))
        (should (eq (default-value 'gptel-model) 'qwen2.5-coder:7b))
        (should-not (file-exists-p ide-ai-model-file))
        ;; Elsewhere: the default for new sessions, remembered.
        (with-temp-buffer
          (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Claude (Anthropic)" "claude-sonnet-5-5")))
            (ide-ai-set-model)))
        (should (eq (default-value 'gptel-model) 'claude-sonnet-5-5))
        (should (equal (ide-ai--saved) '(:provider claude :model claude-sonnet-5-5)))
        ;; A prefix argument inside a chat buffer also sets the default.
        (with-temp-buffer
          (setq-local gptel-mode t)
          (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Ollama (local)" "qwen2.5-coder:7b")))
            (ide-ai-set-model t)))
        (should (eq (default-value 'gptel-model) 'qwen2.5-coder:7b))
        (should (equal (plist-get (ide-ai--saved) :provider) 'ollama))))))

(ert-deftest ai-set-model-refuses-an-unready-provider ()
  (ai-test--with-state
    (ide-ai--apply (ide-ai--provider 'ollama) 'qwen2.5-coder:7b 'global)
    (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Gemini (Google)" "gemini-2.5-pro")))
      (let ((err (should-error (ide-ai-set-model) :type 'user-error)))
        (should (string-match-p "GEMINI_API_KEY" (cadr err)))))
    (should (eq (default-value 'gptel-model) 'qwen2.5-coder:7b))
    (should-not (file-exists-p ide-ai-model-file))))

(ert-deftest ai-remember-can-be-turned-off ()
  (ai-test--with-state
    (let ((process-environment (cons "ANTHROPIC_API_KEY=sk" process-environment)) (ide-ai-remember-model nil))
      (with-temp-buffer
        (cl-letf (((symbol-function 'completing-read) (ai-test--picks "Claude (Anthropic)" "claude-sonnet-5-5")))
          (ide-ai-set-model)))
      (should-not (file-exists-p ide-ai-model-file)))))

;;;; Startup ---------------------------------------------------------------------------------------------------------

(defmacro ai-test--no-network (&rest body)
  `(cl-letf (((symbol-function 'ide-run-with-timeout) (lambda (&rest _) (error "network used at startup")))
             ((symbol-function 'ide-ai--ollama-probe) (lambda () (error "Ollama probed at startup"))))
     ,@body))

(ert-deftest ai-startup-defaults-to-ollama-without-touching-the-network ()
  (ai-test--with-state
    (ai-test--no-network
      (ide-ai--apply-startup-model)
      (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Ollama"))
      (should (eq (default-value 'gptel-model) (intern ide-ai-ollama-model))))))

(ert-deftest ai-startup-precedence-saved-then-configured ()
  (ai-test--with-state
    (let ((process-environment (append '("ANTHROPIC_API_KEY=sk" "GEMINI_API_KEY=gk") process-environment)))
      (ai-test--no-network
        ;; configured default
        (let ((ide-ai-default-model '(claude . claude-haiku-5-5)))
          (ide-ai--apply-startup-model)
          (should (eq (default-value 'gptel-model) 'claude-haiku-5-5)))
        ;; a remembered choice beats the configured default
        (ide-ai--save (ide-ai--provider 'gemini) 'gemini-2.5-flash)
        (let ((ide-ai-default-model '(claude . claude-haiku-5-5)))
          (ide-ai--apply-startup-model)
          (should (eq (default-value 'gptel-model) 'gemini-2.5-flash))
          (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Gemini")))))))

(ert-deftest ai-startup-falls-back-when-the-provider-has-no-key ()
  (ai-test--with-state
    (ai-test--no-network
      (let ((ide-ai-default-model '(claude . claude-sonnet-5-5)))
        (ide-ai--apply-startup-model)
        (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Ollama")))
      ;; a corrupt model file never breaks startup either
      (with-temp-file ide-ai-model-file (insert "(((not valid"))
      (ide-ai--apply-startup-model)
      (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Ollama")))))

(ert-deftest ai-startup-registers-keyed-backends-for-the-gptel-menu ()
  (ai-test--with-state
    (let ((process-environment (cons "ANTHROPIC_API_KEY=sk" process-environment)))
      (ai-test--no-network
        (ide-ai--apply-startup-model)
        (should (assq 'claude ide-ai--backends))
        (should-not (assq 'gemini ide-ai--backends))))))

;;;; Extension --------------------------------------------------------------------------------------------------------------

(ert-deftest ai-users-can-add-their-own-provider ()
  (ai-test--with-state
    (let ((ide-ai-providers
           (append ide-ai-providers
                   '((local-vllm :name "My vLLM" :backend-name "vLLM" :make ide-ai--make-ollama :models (my-model))))))
      (ai-test--ollama t '(x)
        (cl-letf (((symbol-function 'completing-read) (ai-test--picks "My vLLM" "my-model")))
          (let ((sel (ide-ai-select-model)))
            (should (eq (car (car sel)) 'local-vllm))
            (should (eq (cdr sel) 'my-model))))))))

(ert-deftest ai-key-is-bound-and-explains-its-scopes ()
  (should (eq (key-binding (kbd "C-c a M")) 'ide-ai-set-model))
  (should (string-match-p "prefix argument" (documentation 'ide-ai-set-model)))
  (should (commandp 'ide-ai-set-model)))

;;;; Privacy: secrets never reach hosted models ---------------------------------------------------

(ert-deftest ai-private-file-classification ()
  (dolist (f '("/p/.env" "/p/.env.local" "/p/prod.tfvars" "/p/x.auto.tfvars" "/p/terraform.tfstate"
               "/p/terraform.tfstate.backup" "/p/server.pem" "/p/tls.key" "/home/u/.ssh/config"
               "/home/u/.aws/credentials" "/p/kubeconfig" "/p/my-secrets.yaml" "/p/Credentials.json"
               "/home/u/.netrc" "/home/u/.authinfo.gpg" "/home/u/.npmrc" "/p/id_rsa" "/p/secrets"
               "/p/vault.kdbx" "/p/api_token.txt"))
    (should (ide-ai-private-file-p f)))
  (dolist (f '("/p/main.tf" "/p/src/app.py" "/p/README.md" "/p/environment.py" "/p/envoy.yaml"
               "/p/id_rsa.pub" "/p/tokenizer.py" "/p/password_validator.go" "/p/secretary.org" nil))
    (should-not (ide-ai-private-file-p f)))
  (should (ide-ai-private-file-p (expand-file-name "var/azure/config" user-emacs-directory))))

(defmacro ai-test--with-hosted (hosted &rest body)
  "Run BODY with a hosted (Claude) or local (Ollama) provider current, and no prompts unless stubbed."
  (declare (indent 1))
  `(ai-test--with-state
     (let ((process-environment (cons "ANTHROPIC_API_KEY=sk-test" process-environment)))
       (if ,hosted (ide-ai--apply (ide-ai--provider 'claude) 'claude-sonnet-5-5 'global)
         (ide-ai--apply (ide-ai--provider 'ollama) 'qwen2.5-coder:7b 'global))
       ,@body)))

(defmacro ai-test--private-buffer (&rest body)
  `(with-temp-buffer (setq buffer-file-name "/p/prod.tfvars") (unwind-protect (progn ,@body) (setq buffer-file-name nil))))

(ert-deftest ai-hosted-detection ()
  (ai-test--with-hosted t (should (ide-ai-chat-hosted-p)))
  (ai-test--with-hosted nil (should-not (ide-ai-chat-hosted-p)))
  (let ((ide-ai-ollama-host "https://ollama.corp.example")) (should (ide-ai--provider-hosted-p (ide-ai--provider 'ollama))))
  (should (ide-ai--provider-hosted-p nil))
  (should-not (ide-ai--provider-hosted-p '(mine :name "Mine" :hosted nil)))
  (let ((ide-ai-fim-backend 'ollama)) (should-not (ide-ai-fim-hosted-p)))
  (let ((ide-ai-fim-backend 'gemini)) (should (ide-ai-fim-hosted-p))))

(ert-deftest ai-guard-refuses-private-and-confirms-whole-buffer-for-hosted ()
  (ai-test--with-hosted t
    (let ((asked 0) (answer t))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (cl-incf asked) answer)))
        (ai-test--private-buffer
          (should-error (ide-ai--guard-send t) :type 'user-error)
          (should-error (ide-ai--guard-send nil) :type 'user-error)   ; a region of a private file too
          (should (= asked 0)))
        (with-temp-buffer
          (ide-ai--guard-send t) (should (= asked 1))
          (setq answer nil)
          (should-error (ide-ai--guard-send t) :type 'user-error)
          (setq answer t asked 0)
          (ide-ai--guard-send nil) (should (= asked 0))               ; explicit region: no prompt
          (let ((ide-ai-confirm-whole-buffer nil)) (ide-ai--guard-send t) (should (= asked 0))))))))

(ert-deftest ai-guard-lets-local-models-see-everything ()
  (ai-test--with-hosted nil
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "no prompt expected"))))
      (ai-test--private-buffer (ide-ai--guard-send t) (ide-ai--guard-send nil)))))

(ert-deftest ai-mark-private-overrides-the-file-name ()
  (ai-test--with-hosted t
    (with-temp-buffer
      (should-not (ide-ai-private-buffer-p))
      (ide-ai-mark-private)
      (should (ide-ai-private-buffer-p))
      (should-error (ide-ai--guard-send nil) :type 'user-error)
      (ide-ai-mark-private t)
      (should-not (ide-ai-private-buffer-p)))))

(ert-deftest ai-region-action-sends-only-what-is-allowed ()
  (let (sent)
    (cl-letf (((symbol-function 'gptel-request) (lambda (prompt &rest _) (push prompt sent)))
              ((symbol-function 'display-buffer) #'ignore))
      (ai-test--with-hosted t
        ;; private file: refused, nothing sent
        (ai-test--private-buffer (insert "token = abc") (should-error (ide-ai-region-action "explain") :type 'user-error))
        (should-not sent)
        ;; whole buffer, declined: nothing sent
        (with-temp-buffer (insert "print(1)")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
            (should-error (ide-ai-region-action "explain") :type 'user-error)))
        (should-not sent)
        ;; whole buffer, accepted
        (with-temp-buffer (insert "print(1)")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))) (ide-ai-region-action "explain")))
        (should (= 1 (length sent)))
        ;; an explicit region needs no confirmation
        (with-temp-buffer (insert "print(1)\nprint(2)") (goto-char (point-min)) (set-mark (point)) (end-of-line)
          (transient-mark-mode 1) (activate-mark)
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "no prompt expected"))))
            (ide-ai-region-action "explain")))
        (should (= 2 (length sent)))
        (should-not (string-match-p "print(2)" (car sent))))
      ;; local model: no prompt, even for a private buffer
      (setq sent nil)
      (ai-test--with-hosted nil
        (ai-test--private-buffer (insert "x")
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "no prompt expected"))))
            (ide-ai-region-action "explain")))
        (should (= 1 (length sent)))))))

(ert-deftest ai-project-context-skips-private-files ()
  (let* ((dir (file-name-as-directory (make-temp-file "proj" t))) added
         (files (mapcar (lambda (n) (expand-file-name n dir)) '("app.py" ".env" "prod.tfvars" "README.md" "creds/secrets.yaml"))))
    (unwind-protect
        (progn (make-directory (expand-file-name "creds" dir))
               (dolist (f files) (with-temp-file f (insert "x")))
               (cl-letf (((symbol-function 'project-current) (lambda (&rest _) (cons 'transient dir)))
                         ((symbol-function 'project-files) (lambda (&rest _) files))
                         ((symbol-function 'gptel-add-file) (lambda (f) (push (file-name-nondirectory f) added))))
                 (ide-ai-add-project-context)
                 (should (equal (sort added #'string<) '("README.md" "app.py")))))
      (delete-directory dir t))))

(ert-deftest ai-gptel-context-commands-are-guarded ()
  (should (advice-member-p #'ide-ai--guard-gptel-file 'gptel-add-file))
  (should (advice-member-p #'ide-ai--guard-gptel-context 'gptel-add))
  (ai-test--with-hosted t
    (should-error (gptel-add-file "/p/.env") :type 'user-error)
    (ai-test--private-buffer (should-error (gptel-add) :type 'user-error))))

(ert-deftest ai-inline-completion-is-opt-in ()
  (let (calls (ide-ai-inline-completion nil))
    (cl-letf (((symbol-function 'minuet-auto-suggestion-mode) (lambda (arg) (push arg calls))))
      (with-temp-buffer (ide-ai--maybe-enable-minuet))
      (should-not calls))))

(ert-deftest ai-inline-completion-skips-private-buffers-on-hosted-backends ()
  (let (calls (ide-ai-fim-backend 'gemini) (ide-ai-inline-completion t))
    (cl-letf (((symbol-function 'minuet-auto-suggestion-mode) (lambda (arg) (push arg calls))))
      (ai-test--private-buffer (ide-ai--maybe-enable-minuet))
      (should-not calls)
      (with-temp-buffer (ide-ai--maybe-enable-minuet))
      (should (equal calls '(1)))
      ;; a local backend is fine everywhere, private files included
      (setq calls nil)
      (let ((ide-ai-fim-backend 'ollama)) (ai-test--private-buffer (ide-ai--maybe-enable-minuet)))
      (should (equal calls '(1)))
      ;; switching to a hosted backend turns suggestions off in private buffers that already have them
      (setq calls nil)
      (let ((priv (generate-new-buffer " priv")))
        (unwind-protect
            (progn (with-current-buffer priv (setq buffer-file-name "/p/.env") (setq-local minuet-auto-suggestion-mode t))
                   (ide-ai--disable-minuet-in-private-buffers)
                   (should (equal calls '(-1))))
          (with-current-buffer priv (setq buffer-file-name nil)) (kill-buffer priv))))))


;;; ai-test.el ends here
