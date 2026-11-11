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

(ert-deftest ai-ready-fn-decides-readiness-of-keyless-providers ()
  (ai-test--with-state
    (let* ((up nil)
           (ide-ai-providers (append ide-ai-providers
                                     `((mine :name "Mine" :backend-name "Mine" :make ide-ai--make-ollama
                                             :models (m) :ready-fn ,(lambda () up)))))
           (mine (ide-ai--provider 'mine)))
      ;; Ollama's state no longer decides for a provider with its own probe.
      (ai-test--ollama t '(a)
        (should-not (ide-ai--ready-p mine))
        (should (equal (ide-ai--status mine) "not running"))
        (setq up t)
        (should (ide-ai--ready-p mine))
        (should (equal (ide-ai--status mine) "ready"))))))

(ert-deftest ai-openai-compatible-provider-builds-a-local-backend ()
  (ai-test--with-state
    (let* ((entry (ide-ai-openai-compatible-provider
                   'ai-test-gw "Test gateway" "http://127.0.0.1:8002/v1" '(qwen-test)
                   :context-window 241 :max-output 16384 :vision t))
           (ide-ai-providers (cons entry ide-ai-providers))
           (p (ide-ai--provider 'ai-test-gw)))
      (should (fboundp (ide-ai--pget p :make)))
      (should (equal (ide-ai--pget p :models) '(qwen-test)))
      (should-not (ide-ai--provider-hosted-p p))
      ;; the probe asks GET .../v1/models, at most once per 10 seconds
      (let ((calls nil) (ide-ai--http-probes (make-hash-table :test 'equal)))
        (cl-letf (((symbol-function 'ide-run-with-timeout)
                   (lambda (_s &rest args) (push (car (last args)) calls) (list 0 ""))))
          (should (ide-ai--ready-p p))
          (should (ide-ai--ready-p p))
          (should (equal calls '("http://127.0.0.1:8002/v1/models")))))
      (let ((b (ide-ai--backend p '(qwen-test))))
        (should (equal (gptel-backend-name b) "Test gateway"))
        (should (equal (gptel-backend-host b) "127.0.0.1:8002"))
        (should (equal (gptel-backend-protocol b) "http"))
        (should (equal (gptel-backend-endpoint b) "/v1/chat/completions"))
        (should (memq 'media (get 'qwen-test :capabilities)))
        (should (memq 'tool-use (get 'qwen-test :capabilities)))
        (should (equal (get 'qwen-test :context-window) 241))
        (should (equal (get 'qwen-test :request-params) '(:max_tokens 16384)))))))

(ert-deftest ai-startup-applies-a-configured-keyless-provider-without-network ()
  (ai-test--with-state
    (let ((ide-ai-providers (cons (ide-ai-openai-compatible-provider
                                   'ai-test-gw "Test gateway" "http://127.0.0.1:8002/v1" '(qwen-test))
                                  ide-ai-providers)))
      (ai-test--no-network
        (let ((ide-ai-default-model '(ai-test-gw . qwen-test)))
          (ide-ai--apply-startup-model)
          (should (equal (gptel-backend-name (default-value 'gptel-backend)) "Test gateway"))
          (should (eq (default-value 'gptel-model) 'qwen-test)))
        ;; a remembered choice still wins
        (ide-ai--save (ide-ai--provider 'ollama) 'llama-x)
        (let ((ide-ai-default-model '(ai-test-gw . qwen-test)))
          (ide-ai--apply-startup-model)
          (should (eq (default-value 'gptel-model) 'llama-x)))))))

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

;; The guards advise gptel's real functions: `gptel-add' and `gptel-add-file'
;; are aliases, and dired, ibuffer and directory adds bypass them.
(ert-deftest ai-gptel-context-commands-are-guarded ()
  (require 'gptel-context)
  (should (advice-member-p #'ide-ai--guard-gptel-file 'gptel-context-add-file))
  (should (advice-member-p #'ide-ai--guard-gptel-region 'gptel-context--add-region))
  (let ((gptel-context nil))
    (ai-test--with-hosted t
      (should-error (gptel-add-file "/p/.env") :type 'user-error)
      (should-error (gptel-context-add-file "/p/.env") :type 'user-error)
      (ai-test--private-buffer
        (should-error (gptel-add) :type 'user-error)
        (should-error (gptel-context--add-buffer (current-buffer)) :type 'user-error))
      (should-not gptel-context))))

(ert-deftest ai-gptel-directory-add-skips-private-files ()
  (require 'gptel-context)
  (let* ((dir (file-name-as-directory (make-temp-file "ctx" t)))
         (gptel-context nil) (gptel-context-restrict-to-project-files nil))
    (unwind-protect
        (progn
          (dolist (f '("app.py" ".env" "prod.tfvars")) (with-temp-file (expand-file-name f dir) (insert "x")))
          (ai-test--with-hosted t (gptel-context-add-file dir))
          (should (equal (mapcar (lambda (e) (file-name-nondirectory (car e))) gptel-context) '("app.py")))
          (setq gptel-context nil)
          (ai-test--with-hosted nil (gptel-context-add-file dir))
          (should (= 3 (length gptel-context))))
      (delete-directory dir t))))

;;;; Send-time guard: every request, whatever command built it ------------------------------------

(defun ai-test--prompt-buffers ()
  (cl-count-if (lambda (b) (string-prefix-p " *gptel-prompt*" (buffer-name b))) (buffer-list)))

(defmacro ai-test--no-network (sent &rest body)
  "Run BODY with gptel's transports stubbed; requests that reach them are pushed onto SENT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'gptel-curl-get-response) (lambda (fsm) (push fsm ,sent)))
             ((symbol-function 'gptel--url-get-response) (lambda (fsm) (push fsm ,sent))))
     ,@body))

(ert-deftest ai-send-guard-refuses-private-buffers-for-hosted ()
  (should (advice-member-p #'ide-ai--guard-request 'gptel--realize-query))
  (let ((before (ai-test--prompt-buffers)) (gptel-context nil) sent)
    (ai-test--no-network sent
      (ai-test--with-hosted t
        ;; gptel-send, gptel-rewrite (a string prompt from the buffer) and plain requests
        (ai-test--private-buffer (insert "token = abc")
          (should-error (gptel-send) :type 'user-error)
          (should-error (gptel-request "rewrite this" :dry-run t) :type 'user-error)
          (should-error (gptel-request nil :dry-run t) :type 'user-error))
        (should-not sent)
        (with-temp-buffer (insert "print(1)") (gptel-send))
        (should (= 1 (length sent))))
      (setq sent nil)
      ;; local model: private buffers go through
      (ai-test--with-hosted nil
        (ai-test--private-buffer (insert "token = abc") (gptel-send))
        (should (= 1 (length sent)))))
    (should (= before (ai-test--prompt-buffers)))))

(ert-deftest ai-send-guard-checks-context-at-send-time ()
  (let* ((dir (file-name-as-directory (make-temp-file "ctx" t)))
         (env (expand-file-name ".env" dir)) (app (expand-file-name "app.py" dir)) sent)
    (unwind-protect
        (progn
          (dolist (f (list env app)) (with-temp-file f (insert "x")))
          (ai-test--no-network sent
            ;; Added while a local model was current, then sent to a hosted one.
            (ai-test--with-hosted t
              (let ((gptel-context (list app env)))
                (with-temp-buffer (insert "hi")
                  (should-error (gptel-request nil :dry-run t) :type 'user-error)
                  (let ((gptel-use-context nil)) (gptel-request nil :dry-run t))))
              (let ((gptel-context (list app)))
                (with-temp-buffer (insert "hi") (gptel-request nil :dry-run t)))
              ;; a context buffer marked private
              (let ((b (generate-new-buffer "notes")))
                (unwind-protect
                    (let ((gptel-context (list b)))
                      (with-current-buffer b (setq ide-ai-private-buffer t))
                      (with-temp-buffer (insert "hi")
                        (should-error (gptel-request nil :dry-run t) :type 'user-error)))
                  (kill-buffer b))))
            (should-not sent)))
      (delete-directory dir t))))

(ert-deftest ai-send-guard-uses-the-sending-buffers-backend ()
  (let* ((dir (make-temp-file "ctx" t)) (env (expand-file-name ".env" dir)) (gptel-context (list env)))
    (with-temp-file env (insert "x"))
    (unwind-protect
        (progn
          ;; default local, but this chat buffer talks to Claude
          (ai-test--with-hosted nil
            (let ((claude (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5))))
              (with-temp-buffer
                (setq-local gptel-backend claude gptel-model 'claude-sonnet-5-5)
                (insert "hi")
                (should-error (gptel-request nil :dry-run t) :type 'user-error))))
          ;; default hosted, but this buffer uses local Ollama
          (ai-test--with-hosted t
            (let ((ollama (ide-ai--backend (ide-ai--provider 'ollama) '(qwen2.5-coder:7b))))
              (with-temp-buffer
                (setq-local gptel-backend ollama gptel-model 'qwen2.5-coder:7b)
                (insert "hi")
                (should (gptel-request nil :dry-run t))))))
      (delete-directory dir t))))

;;;; Agent tools ------------------------------------------------------------------------------------

(defmacro ai-test--secret-tree (dir &rest body)
  "Bind DIR to a temp tree holding app.py, .env and conf/db-secrets.yaml, all containing needle."
  (declare (indent 1))
  `(let ((,dir (file-name-as-directory (make-temp-file "agent" t))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "conf" ,dir))
           (with-temp-file (expand-file-name "app.py" ,dir) (insert "needle = 1\n"))
           (with-temp-file (expand-file-name ".env" ,dir) (insert "needle=SECRET1\n"))
           (with-temp-file (expand-file-name "conf/db-secrets.yaml" ,dir) (insert "needle: SECRET2\n"))
           ,@body)
       (delete-directory ,dir t))))

(ert-deftest ai-agent-read-and-grep-hide-private-files-from-hosted ()
  (require 'gptel-agent-tools)
  (ai-test--with-hosted nil
    (let ((claude (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5)))
          (ollama (ide-ai--backend (ide-ai--provider 'ollama) '(qwen2.5-coder:7b))))
      (ai-test--secret-tree dir
        (let ((env (expand-file-name ".env" dir)) (app (expand-file-name "app.py" dir)))
          ;; hosted request: the backend comes from the request, not the buffer
          (let ((ide-ai--request-backend claude))
            (should-error (gptel-agent--read-file-lines env nil nil))
            (should-error (gptel-agent--read-file-lines env 1 1))
            (should (string-match-p "needle" (gptel-agent--read-file-lines app nil nil)))
            (should-error (gptel-agent--grep "needle" env))
            (let ((out (gptel-agent--grep "needle" dir)))
              (should (string-match-p "app\\.py" out))
              (should-not (string-match-p "SECRET" out))
              (should (string-match-p "withheld" out))))
          ;; local request: everything
          (let ((ide-ai--request-backend ollama))
            (should (string-match-p "SECRET1" (gptel-agent--read-file-lines env nil nil)))
            (should (string-match-p "SECRET2" (gptel-agent--grep "needle" dir)))))))))

(ert-deftest ai-agent-grep-filter-handles-every-output-format ()
  (ai-test--secret-tree dir
    (dolist (case `(("git" ,(concat "app.py:1:needle = 1\n.env:1:needle=SECRET1\n"
                                    "conf/db-secrets.yaml-0-ctx SECRET\nconf/db-secrets.yaml:1:needle: SECRET2"))
                    ("rg" ,(format "%sapp.py\n1:needle = 1\n\n%s.env\n1:needle=SECRET1\n--\n3:SECRET3\n\n%sconf/db-secrets.yaml\n1:needle: SECRET2"
                                   dir dir dir))))
      (with-temp-buffer
        (insert (cadr case))
        (let ((ide-ai--grep-root dir)) (ide-ai--grep-drop-private))
        (should (string-match-p "needle = 1" (buffer-string)))
        (should-not (string-match-p "SECRET" (buffer-string)))))))

;; Paths starting with digits, dashes in directory names, context lines and
;; separators, in a real git repository and without one (rg or grep).
(ert-deftest ai-agent-grep-filter-resolves-tricky-paths-and-fails-closed ()
  (require 'gptel-agent-tools)
  (let* ((dir (file-name-as-directory (make-temp-file "gr" t))) (default-directory dir))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "app.py" dir) (insert "a\nneedle = 1\nb\n"))
          (with-temp-file (expand-file-name "2024-secrets.yaml" dir) (insert "a\nneedle: SECRET_A\nb\n"))
          (with-temp-file (expand-file-name "1-prod.tfvars" dir) (insert "x = 1\nneedle = \"SECRET_B\"\n"))
          (make-directory (expand-file-name "x-1-y" dir))
          (with-temp-file (expand-file-name "x-1-y/.env" dir) (insert "needle=SECRET_C\nSECRET_CTX\n"))
          (with-temp-file (expand-file-name "secrets.yaml" dir) (insert "needle: SECRET_D\n"))
          (ai-test--with-hosted t
            (let ((ide-ai--request-backend (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5))))
              (dolist (git '(t nil))
                (when git (call-process "git" nil nil nil "init" "-q"))
                (unless git (delete-directory (expand-file-name ".git" dir) t))
                (dolist (ctx '(nil 1))
                  (let ((out (gptel-agent--grep "needle" dir nil ctx)))
                    (should (string-match-p "needle = 1" out))
                    (should-not (string-match-p "SECRET" out))
                    (should-not (string-match-p "\\`--\\|\n--\n--" out))))))))
      (delete-directory dir t)))
  ;; a line whose file cannot be told is dropped
  (with-temp-buffer
    (insert "mystery:line\nREADME.md:1:x")
    (let ((ide-ai--grep-root default-directory)) (ide-ai--grep-drop-private))
    (should-not (string-match-p "mystery" (buffer-string)))
    (should (string-match-p "README.md:1:x" (buffer-string)))))

(ert-deftest ai-agent-grep-filter-longest-prefix-quoted-paths-and-speed ()
  (let* ((dir (file-name-as-directory (make-temp-file "gr" t))))
    (unwind-protect
        (progn
          (dolist (f '("config" "config-2-credentials.json" "café.py"))
            (with-temp-file (expand-file-name f dir) (insert "x")))
          ;; `config' exists, but the line belongs to the private file
          (with-temp-buffer
            (insert "config-2-credentials.json:1:SECRET\nconfig:1:fine\n\"caf\\303\\251.py\":1:needle")
            (let ((ide-ai--grep-root dir)) (ide-ai--grep-drop-private))
            (should-not (string-match-p "SECRET" (buffer-string)))
            (should (string-match-p "config:1:fine" (buffer-string)))
            (should (string-match-p "caf.*needle" (buffer-string))))
          ;; an rg heading naming a whole file wins over a dash-number split
          (make-directory (expand-file-name "a-1-b" dir))
          (dolist (f '("a" "a-1-b/.env" "a-1-b/c.txt")) (with-temp-file (expand-file-name f dir) (insert "x")))
          (with-temp-buffer
            (insert (format "%sa-1-b/.env\n1:SECRET\n\n%sa-1-b/c.txt\n1:public text\n" dir dir))
            (let ((ide-ai--grep-root dir)) (ide-ai--grep-drop-private))
            (should-not (string-match-p "SECRET\\|\\.env" (buffer-string)))
            (should (string-match-p "c\\.txt\n1:public text" (buffer-string))))
          ;; real git grep quotes non-ASCII names
          (let ((default-directory dir))
            (call-process "git" nil nil nil "init" "-q")
            (with-temp-file (expand-file-name "café.py" dir) (insert "needle\n"))
            (with-temp-file (expand-file-name "config-2-credentials.json" dir) (insert "needle SECRET\n"))
            (require 'gptel-agent-tools)
            (ai-test--with-hosted t
              (let* ((ide-ai--request-backend (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5)))
                     (out (gptel-agent--grep "needle" dir)))
                (should (string-match-p "caf.*:1:needle" out))
                (should-not (string-match-p "SECRET" out)))))
          ;; 20k lines of -C2 output over 300 files, with many buffers open
          (dotimes (i 300) (with-temp-file (expand-file-name (format "f%03d.py" i) dir) (insert "x")))
          (let ((bufs (cl-loop for i below 300 collect (generate-new-buffer (format "ai-test-%d" i)))))
            (unwind-protect
                (with-temp-buffer
                  (dotimes (n 20000)
                    (insert (format "f%03d.py%s%d%sneedle line %d\n" (% n 300) (if (= 0 (% n 5)) ":" "-")
                                    n (if (= 0 (% n 5)) ":" "-") n))
                    (when (= 0 (% n 7)) (insert "--\n")))
                  (insert "config-2-credentials.json:1:SECRET\n")
                  (let ((ide-ai--grep-root dir) (t0 (float-time)))
                    (ide-ai--grep-drop-private)
                    (should (< (- (float-time) t0) 0.5)))
                  (should-not (string-match-p "SECRET" (buffer-string))))
              (mapc #'kill-buffer bufs))))
      (delete-directory dir t))))

(ert-deftest ai-system-and-credential-files-are-private ()
  (dolist (f '("/proc/self/environ" "/proc/1/environ" "/sys/class/x" "/dev/stdin"
               "~/.config/gh/hosts.yml" "~/.docker/config.json" "~/.vault-token" "~/.terraformrc"
               "~/.config/rclone/rclone.conf" "~/.kube/config" "~/.ssh/known_hosts" "~/.aws/config"
               "~/.azure/x" "~/.config/gcloud/credentials.db" "~/.netrc" "~/.authinfo.gpg"
               "~/.npmrc" "~/.pypirc" "~/.git-credentials" "~/.claude/.credentials.json"))
    (should (ide-ai--private-path-p (expand-file-name f))))
  ;; a symlink to a secret is judged by its target
  (let* ((dir (make-temp-file "lnk" t)) (env (expand-file-name ".env" dir)) (ln (expand-file-name "harmless" dir)))
    (unwind-protect
        (progn (with-temp-file env (insert "x")) (make-symbolic-link env ln)
               (should (ide-ai--private-path-p ln))
               (with-current-buffer (find-file-noselect ln)
                 (unwind-protect (should (ide-ai-private-buffer-p)) (kill-buffer))))
      (delete-directory dir t))))

(ert-deftest ai-agent-local-spill-files-are-private-for-hosted ()
  (require 'gptel-agent-tools)
  (let* ((dir (file-name-as-directory (make-temp-file "spill" t))) spill)
    (unwind-protect
        (ai-test--with-hosted nil
          (with-temp-file (expand-file-name "prod.tfvars" dir)
            (dotimes (i 2000) (insert (format "needle SECRET%d padding padding\n" i))))
          (let* ((ide-ai--request-backend (ide-ai--backend (ide-ai--provider 'ollama) '(qwen2.5-coder:7b)))
                 (out (gptel-agent--grep "needle" dir)))
            (should (string-match "Stored in: \\(.+\\)$" out))
            (setq spill (match-string 1 out)))
          (let ((ide-ai--request-backend (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5))))
            (should-error (gptel-agent--read-file-lines spill 1 5))))
      (when (and spill (file-exists-p spill)) (delete-file spill))
      (delete-directory dir t))))

;;;; Text that left a private buffer ----------------------------------------------------------------

(ert-deftest ai-menu-send-refuses-a-private-source-buffer ()
  (require 'gptel-transient)
  (let (sent (gptel-context nil) (chat "*ai-test-chat*"))
    (ai-test--no-network sent
      ;; to a new chat session, while hosted
      (ai-test--with-hosted t
        (ai-test--private-buffer (insert "AWS_SECRET=hunter2\n")
          (should-error (gptel--suffix-send (list (concat "g" chat))) :type 'user-error)
          (should-error (gptel--suffix-send nil) :type 'user-error)))
      (should-not (get-buffer chat))
      ;; to an existing chat that talks to Claude while the default is local
      (ai-test--with-hosted nil
        (let ((b (get-buffer-create chat)))
          (unwind-protect
              (progn
                (with-current-buffer b
                  (setq-local gptel-backend (ide-ai--backend (ide-ai--provider 'claude) '(claude-sonnet-5-5))
                              gptel-model 'claude-sonnet-5-5))
                (ai-test--private-buffer (insert "AWS_SECRET=hunter2\n")
                  (should-error (gptel--suffix-send (list (concat "g" chat))) :type 'user-error))
                (should-not (string-match-p "hunter2" (with-current-buffer b (buffer-string)))))
            (kill-buffer b)))))
    (should-not sent)))

;; Kills made in a private buffer are refused by gptel's commands that send a
;; kill directly; pasted text is the user's explicit choice and is not tracked.
(ert-deftest ai-private-kills-are-refused-by-gptels-kill-commands ()
  (require 'gptel-context)
  (require 'gptel-transient)
  (let (sent (gptel-context nil) (kill-ring nil) (kill-ring-yank-pointer nil)
             (interprogram-paste-function nil) (interprogram-cut-function nil))
    (ai-test--no-network sent
      (ai-test--with-hosted t
        (ai-test--private-buffer (insert "PASSWORD=hunter2") (kill-ring-save (point-min) (point-max)))
        (should-not (text-properties-at 0 (car kill-ring)))      ; no tagging
        (should-error (gptel-context-add-current-kill) :type 'user-error)
        (with-temp-buffer (should-error (gptel--suffix-send '("y")) :type 'user-error))
        ;; C-u: an older private kill chosen from the kill ring
        (with-temp-buffer (insert "print(1)") (kill-ring-save (point-min) (point-max)))
        (let ((current-prefix-arg '(4)))
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "PASSWORD=hunter2")))
            (with-temp-buffer (should-error (gptel--suffix-send '("y")) :type 'user-error))))
        (should-not sent)
        ;; the newest kill is ordinary: fine
        (gptel-context-add-current-kill)
        (with-temp-buffer (gptel--suffix-send '("y" "e")))
        (should (= 1 (length sent)))
        ;; pasted text is not tracked: a chat holding it, and later typing, still send
        (setq sent nil)
        (with-temp-buffer (insert "PASSWORD=hunter2 explain") (gptel-send))
        (should (= 1 (length sent)))))
    ;; local models may see private kills
    (ai-test--with-hosted nil
      (let ((gptel-context nil) (kill-ring nil) (interprogram-paste-function nil) (interprogram-cut-function nil))
        (ai-test--private-buffer (insert "PASSWORD=hunter2") (kill-ring-save (point-min) (point-max)))
        (gptel-context-add-current-kill)))))

;; Added under a local model, then sent after switching to a hosted one.
(ert-deftest ai-private-kill-in-context-is-refused-at-send-time ()
  (require 'gptel-context)
  (let (sent (gptel-context nil) (kill-ring nil) (kill-ring-yank-pointer nil)
             (interprogram-paste-function nil) (interprogram-cut-function nil))
    (unwind-protect
        (ai-test--no-network sent
          (ai-test--with-hosted nil
            (with-temp-buffer (insert "print(1)") (kill-ring-save (point-min) (point-max)))
            (gptel-context-add-current-kill)
            (ai-test--private-buffer (insert "PASSWORD=hunter2") (kill-ring-save (point-min) (point-max)))
            (gptel-context-add-current-kill t)       ; accumulated after a ---- separator
            (with-temp-buffer (insert "hi") (gptel-send))
            (should (= 1 (length sent))))
          (setq sent nil)
          (ai-test--with-hosted t
            (with-temp-buffer (insert "hi") (should-error (gptel-send) :type 'user-error))
            (should-not sent)
            ;; the secret survives the kill ring forgetting it
            (setq kill-ring nil) (garbage-collect)
            (with-temp-buffer (insert "hi") (should-error (gptel-send) :type 'user-error))
            ;; removing the private segment un-blocks the context
            (with-current-buffer " *gptel-kill-ring-context*"
              (goto-char (point-min))
              (when (search-forward "\n----\n" nil t) (delete-region (match-beginning 0) (point-max))))
            (with-temp-buffer (insert "hi") (gptel-send))
            (should (= 1 (length sent)))))
      (when (get-buffer " *gptel-kill-ring-context*") (kill-buffer " *gptel-kill-ring-context*")))))

(ert-deftest ai-indirect-buffers-of-private-files-are-private ()
  (let (sent (gptel-context nil))
    (ai-test--no-network sent
      (ai-test--with-hosted t
        (ai-test--private-buffer (insert "PASSWORD=hunter2")
          (let ((ind (make-indirect-buffer (current-buffer) "ai-test-indirect")))
            (unwind-protect
                (with-current-buffer ind
                  (should (ide-ai-private-buffer-p))
                  (should-error (gptel-send) :type 'user-error))
              (kill-buffer ind))))))
    (should-not sent)))

(ert-deftest ai-refused-request-resets-the-chat-status ()
  (let (statuses (env-dir (make-temp-file "ctx" t)))
    (unwind-protect
        (let ((gptel-context (list (expand-file-name ".env" env-dir))))
          (with-temp-file (car gptel-context) (insert "x"))
          (cl-letf (((symbol-function 'gptel--update-status) (lambda (msg &rest _) (push msg statuses))))
            (ai-test--with-hosted t
              (with-temp-buffer
                (setq-local gptel-mode t)
                (insert "hi")
                (should-error (gptel-request nil :dry-run t) :type 'user-error))))
          (should (equal statuses '(" Ready"))))
      (delete-directory env-dir t))))

(ert-deftest ai-tool-calls-see-their-requests-backend ()
  (let (seen)
    (ide-ai--with-request-backend (lambda (_fsm) (setq seen ide-ai--request-backend))
                                  (gptel-make-fsm :info (list :backend 'the-backend)))
    (should (eq seen 'the-backend))
    (should (advice-member-p #'ide-ai--with-request-backend 'gptel--handle-tool-use))))

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
