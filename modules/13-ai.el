;;; 13-ai.el --- Inline completion and agentic chat -*- lexical-binding: t; -*-

;;; Commentary:
;; GitHub Copilot (copilot.el) for ghost-text inline suggestions, or minuet
;; (local Ollama or cloud FIM); gptel for conversational / region-based work,
;; gptel-agent for autonomous tool-using sessions (file edits, shell, Elisp,
;; web, sub-agents and Agent Skills); agent-shell for ACP agents such as Pi.
;; emcp and elisp-dev-mcp serve this Emacs to outside agents over MCP.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'url-parse)                        ; small; parses OpenAI-compatible base URLs
(eval-when-compile (require 'gptel nil t))  ; struct types, for the byte-compiler

(defgroup ide-ai nil "AI assistant settings." :group 'tools)

(defmacro ide-ai--after-module (&rest body)
  "Run BODY now if this module has finished loading, else once it has.
For `use-package' :config bodies that call functions defined further down:
package.el may load a package (and so run its :config) while this file loads."
  `(if (featurep '13-ai) (progn ,@body) (with-eval-after-load '13-ai ,@body)))

(defcustom ide-ai-fim-backend 'copilot
  "Backend for inline completion: `copilot' (GitHub Copilot, needs a subscription
and one `copilot-login'), or minuet with `ollama', `gemini' or `openai-fim'."
  :type '(choice (const copilot) (const ollama) (const gemini) (const openai-fim)))

(defcustom ide-ai-inline-completion t
  "Start ghost-text inline completion in every programming buffer.
`ide-ai-toggle-inline-completion' (C-c a t) switches it on and off.  Private
buffers (secrets) never get suggestions from a hosted backend such as Copilot."
  :type 'boolean)

(defcustom ide-ai-ollama-model "qwen2.5-coder:7b" "Ollama FIM model." :type 'string)
(defcustom ide-ai-ollama-host "http://localhost:11434" "Ollama base URL." :type 'string)
(defcustom ide-ai-gemini-model "gemini-2.0-flash" "Gemini model for inline completion." :type 'string)

(declare-function minuet-auto-suggestion-mode "minuet")
(defvar minuet-provider)
(defvar minuet-openai-fim-compatible-options)
(defvar minuet-gemini-options)
(defvar minuet-active-mode-map)
(defvar minuet-auto-suggestion-debounce-delay)
(defvar minuet-auto-suggestion-block-predicates)
(defvar corfu--frame)

(defun ide-ai-apply-fim-backend ()
  "Configure minuet according to `ide-ai-fim-backend'."
  (interactive)
  (pcase ide-ai-fim-backend
    ('ollama
     (setq minuet-provider 'openai-fim-compatible
           minuet-openai-fim-compatible-options
           (let ((opts (copy-sequence minuet-openai-fim-compatible-options)))
             (setq opts (plist-put opts :model ide-ai-ollama-model))
             (setq opts (plist-put opts :end-point (concat ide-ai-ollama-host "/v1/completions")))
             (setq opts (plist-put opts :api-key (lambda () "ollama")))
             (setq opts (plist-put opts :name "Ollama"))
             (plist-put opts :optional '(:max_tokens 56)))))
    ('gemini
     (setq minuet-provider 'gemini
           minuet-gemini-options (plist-put (copy-sequence minuet-gemini-options)
                                            :model ide-ai-gemini-model)))
    ('openai-fim (setq minuet-provider 'openai-fim-compatible))
    ('copilot nil))                     ; copilot.el needs no minuet setup
  (ide-ai--disable-minuet-in-private-buffers)
  (message "Inline completion backend: %s" ide-ai-fim-backend))

(defun ide-ai-switch-backend (backend)
  "Switch the inline completion BACKEND."
  (interactive (list (intern (completing-read "FIM backend: " '("copilot" "ollama" "gemini" "openai-fim") nil t))))
  (setq ide-ai-fim-backend backend)
  (unless (eq backend 'copilot) (require 'minuet nil t))
  (ide-ai-apply-fim-backend)
  (ide-ai--refresh-inline))

(use-package minuet
  :ensure t
  :commands minuet-auto-suggestion-mode
  :bind (:map minuet-active-mode-map
         ("TAB" . minuet-accept-suggestion)
         ("<tab>" . minuet-accept-suggestion)
         ("C-<return>" . minuet-accept-suggestion-word)
         ("S-<return>" . minuet-accept-suggestion-line)
         ("M-n" . minuet-next-suggestion)
         ("M-p" . minuet-previous-suggestion)
         ("C-g" . minuet-dismiss-suggestion))
  :init
  (setq minuet-auto-suggestion-debounce-delay 0.4
        minuet-auto-suggestion-throttle-delay 1.0
        minuet-n-completions 1
        minuet-request-timeout 6
        minuet-context-window 2048
        ;; An unreachable model must not nag after every pause in typing.
        minuet-show-error-message-on-minibuffer nil)
  :config
  (ide-ai--after-module (ide-ai-apply-fim-backend))
  (add-to-list 'minuet-auto-suggestion-block-predicates
               (lambda () (and (bound-and-true-p corfu--frame) (frame-visible-p corfu--frame)))))

;; GitHub Copilot: the language server comes from mise (mise.toml pins it), so
;; copilot.el never downloads one itself.  Sign in once with M-x copilot-login.
(defvar copilot-completion-map)
(defvar copilot-disable-predicates)
(defvar copilot-install-dir)

(use-package copilot
  :ensure t
  :commands (copilot-mode copilot-login copilot-logout copilot-diagnose)
  :bind (:map copilot-completion-map
         ("TAB" . copilot-accept-completion)
         ("<tab>" . copilot-accept-completion)
         ("C-<return>" . copilot-accept-completion-by-word)
         ("S-<return>" . copilot-accept-completion-by-line)
         ("M-n" . copilot-next-completion)
         ("M-p" . copilot-previous-completion)
         ("C-g" . copilot-clear-overlay))
  :init
  (setq copilot-install-dir (expand-file-name "copilot/" ide-var-dir))
  :config
  ;; Never ask Copilot about secrets, and stay quiet while the completion popup is open.
  (add-to-list 'copilot-disable-predicates #'ide-ai-private-buffer-p)
  (add-to-list 'copilot-disable-predicates
               (lambda () (and (bound-and-true-p corfu--frame) (frame-visible-p corfu--frame)))))

;;;; gptel ------------------------------------------------------------------

(declare-function gptel-make-ollama "gptel-ollama")
(declare-function gptel-make-anthropic "gptel-anthropic")
(declare-function gptel-make-gemini "gptel-gemini")
(declare-function ide-run-with-timeout "00-core")
(declare-function gptel-request "gptel-request")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-add-file "gptel-context")
(declare-function gptel "gptel")
(defvar gptel-backend)
(defvar gptel-model)
(defvar gptel-default-mode)
(defvar gptel-mode-map)

(use-package gptel
  :commands (gptel gptel-send gptel-menu gptel-rewrite gptel-add gptel-add-file)
  :init (setq gptel-default-mode 'org-mode
              gptel-display-buffer-action '(display-buffer-reuse-window display-buffer-at-bottom))
  ;; Must stay the last form: it applies your saved or configured model after
  ;; gptel's own defaults, so nothing in user/ can be silently overwritten.
  :config (ide-ai--after-module (ide-ai--apply-startup-model)))

;;;; Providers and models -----------------------------------------------------------

(declare-function gptel-backend-name "gptel")
(defvar gptel-mode)

(defcustom ide-ai-default-model nil
  "Provider and model for chat and agent sessions: (PROVIDER-ID . MODEL), or nil.
Applied when gptel loads unless you saved a choice with `ide-ai-set-model'.
Example: (setq ide-ai-default-model \\='(claude . claude-sonnet-5-5))"
  :type '(choice (const nil) (cons symbol symbol)))

(defcustom ide-ai-remember-model t
  "Remember the model chosen as the default with `ide-ai-set-model'." :type 'boolean)

(defvar ide-ai-model-file (expand-file-name "ai-model.eld" ide-var-dir)
  "Where the remembered provider and model are stored.")

(defvar ide-ai-providers
  '((ollama :name "Ollama (local)" :backend-name "Ollama"
            :make ide-ai--make-ollama :models-fn ide-ai--ollama-models
            :ready-fn ide-ai--ollama-running-p)
    (claude :name "Claude (Anthropic)" :backend-name "Claude"
            :make ide-ai--make-claude :env "ANTHROPIC_API_KEY" :host "api.anthropic.com"
            :models (claude-fable-5-1 claude-opus-5-5 claude-sonnet-5-5 claude-haiku-5-5))
    (gemini :name "Gemini (Google)" :backend-name "Gemini"
            :make ide-ai--make-gemini :env "GEMINI_API_KEY" :host "generativelanguage.googleapis.com"
            :models (gemini-3.8-flash gemini-2.5-pro gemini-2.5-flash)))
  "Providers offered by `ide-ai-set-model': (ID . PLIST).
PLIST keys: :name (shown), :backend-name (the gptel backend's name), :make
(function (MODELS KEY-FUNCTION) returning a gptel backend), :models or
:models-fn (suggested models; any other model id can still be typed), for
hosted services :env (API key variable) and :host (auth-source host), and for
keyless servers :ready-fn (function returning non-nil while the server answers;
without it the Ollama probe is used) and :hosted (nil for your own machines).
Add your own entry in user/ to offer another endpoint;
`ide-ai-openai-compatible-provider' builds one for any OpenAI-compatible server.")

(defun ide-ai--provider (id) (assq id ide-ai-providers))
(defun ide-ai--pget (provider key) (plist-get (cdr provider) key))

(defun ide-ai--api-key (provider)
  "API key for PROVIDER from its environment variable or auth-source, or nil."
  (let ((env (ide-ai--pget provider :env)) (host (ide-ai--pget provider :host)))
    (and env
         (or (let ((v (getenv env))) (and v (not (string-empty-p v)) v))
             (ignore-errors
               (when-let* ((found (car (auth-source-search :host host :max 1))))
                 (let ((secret (plist-get found :secret))) (if (functionp secret) (funcall secret) secret))))))))

(defvar ide-ai--ollama-cache nil "(TIME RUNNING . MODELS) of the last probe.")

(defun ide-ai--ollama-probe ()
  "Return (RUNNING . MODELS) for the local Ollama server, cached for 10 seconds."
  (if (and ide-ai--ollama-cache (< (float-time (time-since (car ide-ai--ollama-cache))) 10))
      (cdr ide-ai--ollama-cache)
    (let* ((res (ide-run-with-timeout 4 "curl" "-s" "--max-time" "2" (concat ide-ai-ollama-host "/api/tags")))
           (models (and (eql (car res) 0)
                        (ignore-errors
                          (mapcar (lambda (m) (intern (alist-get 'name m)))
                                  (alist-get 'models (json-parse-string (nth 1 res) :object-type 'alist :array-type 'list))))))
           (probe (cons (and (eql (car res) 0) (stringp (nth 1 res)) (not (string-empty-p (nth 1 res))) t) models)))
      (setq ide-ai--ollama-cache (cons (current-time) probe))
      probe)))

(defun ide-ai--ollama-models ()
  "Models installed in the local Ollama, or the configured default when it is not running."
  (or (cdr (ide-ai--ollama-probe)) (list (intern ide-ai-ollama-model))))

(defun ide-ai--ollama-running-p ()
  "Non-nil while the local Ollama server answers."
  (car (ide-ai--ollama-probe)))

(defun ide-ai--ready-p (provider)
  "Non-nil when PROVIDER can be used right now."
  (cond ((ide-ai--pget provider :env) (and (ide-ai--api-key provider) t))
        ((ide-ai--pget provider :ready-fn) (and (funcall (ide-ai--pget provider :ready-fn)) t))
        (t (ide-ai--ollama-running-p))))

(defun ide-ai--status (provider)
  "Short status text for PROVIDER, shown next to it in the picker."
  (cond ((ide-ai--ready-p provider) "ready")
        ((ide-ai--pget provider :env) (format "needs %s" (ide-ai--pget provider :env)))
        (t "not running")))

(defun ide-ai--make-ollama (models _key)
  (gptel-make-ollama "Ollama" :host (replace-regexp-in-string "\\`https?://" "" ide-ai-ollama-host)
                     :stream t :models models))
(defun ide-ai--make-claude (models key) (gptel-make-anthropic "Claude" :stream t :key key :models models))
(defun ide-ai--make-gemini (models key) (gptel-make-gemini "Gemini" :stream t :key key :models models))

;;;; OpenAI-compatible servers (vLLM, llama.cpp, a local gateway, ...) ---------------

(declare-function gptel-make-openai "gptel-openai")

(defvar ide-ai--http-probes (make-hash-table :test 'equal)
  "URL -> (TIME . ANSWERED) of the last readiness probe.")

(defun ide-ai--http-answers-p (url)
  "Non-nil when GET URL succeeds; the answer is cached for 10 seconds."
  (let ((hit (gethash url ide-ai--http-probes)))
    (if (and hit (< (float-time (time-since (car hit))) 10))
        (cdr hit)
      (let* ((res (ide-run-with-timeout 4 "curl" "-sf" "-o" "/dev/null" "--max-time" "2" url))
             (ok (eql (car res) 0)))
        (puthash url (cons (current-time) ok) ide-ai--http-probes)
        ok))))

(cl-defun ide-ai-openai-compatible-provider (id name url models
                                                &key (key "none") (hosted nil) (context-window nil)
                                                (max-output nil) (vision nil) (tools t))
  "A provider entry for `ide-ai-providers': an OpenAI-compatible server at URL.
ID is a symbol, NAME the label shown, URL the API base (\"http://127.0.0.1:8002/v1\"),
MODELS a list of model id symbols.  KEY is sent as the bearer token (keyless
servers accept anything).  HOSTED nil (the default) means the server runs on
your own machines, so the privacy guards treat it like a local model.
CONTEXT-WINDOW is in thousands of tokens, MAX-OUTPUT caps the reply's tokens,
VISION non-nil allows images, TOOLS nil hides tool use (agents need it).
The server is probed (GET URL/models) only when the model picker shows it."
  (let*((u (url-generic-parse-url url))
         (host (format "%s:%d" (url-host u) (url-port u)))
         (path (string-remove-suffix "/" (url-filename u)))
         (caps (append (and tools '(tool-use)) '(json) (and vision '(media))))
         (specs (mapcar (lambda (m)
                          (append (list m :capabilities caps)
                                  (and context-window (list :context-window context-window))
                                  (and vision (list :mime-types '("image/png" "image/jpeg" "image/webp" "image/gif")))
                                  (and max-output (list :request-params (list :max_tokens max-output)))))
                        models))
         (make (intern (format "ide-ai--make-%s" id)))
         (ready (intern (format "ide-ai--%s-ready-p" id))))
    (defalias make
      (lambda (_models _key)
        (gptel-make-openai name :host host :protocol (url-type u) :endpoint (concat path "/chat/completions")
                           :stream t :key key :models specs))
      (format "Make the gptel backend for %s (%s)." name url))
    (defalias ready (lambda () (ide-ai--http-answers-p (concat url "/models")))
      (format "Non-nil while %s answers." name))
    (list id :name name :backend-name name :make make :models models :ready-fn ready :hosted hosted)))

(defvar ide-ai--backends nil "Alist (PROVIDER-ID . gptel backend) created so far.")

(defun ide-ai--backend (provider models)
  "Return PROVIDER's gptel backend, creating it if needed, offering MODELS."
  (let* ((id (car provider)) (existing (alist-get id ide-ai--backends)))
    (if existing
        ;; The slot is reached through the backend's own type, known only at
        ;; run time, so compiling needs no gptel struct definitions.
        (let ((type (type-of existing)))
          (setf (cl-struct-slot-value type 'models existing)
                (delete-dups (append models (cl-struct-slot-value type 'models existing))))
          existing)
      (setf (alist-get id ide-ai--backends)
            (funcall (ide-ai--pget provider :make) models
                     (lambda () (ide-ai--api-key provider)))))))

(defun ide-ai--models (provider &optional current)
  "Models to suggest for PROVIDER (symbols), including CURRENT when given."
  (delete-dups
   (append (and current (list current))
           (let ((fn (ide-ai--pget provider :models-fn)))
             (if fn (funcall fn) (ide-ai--pget provider :models))))))

(defun ide-ai--backend-provider (backend)
  "The provider BACKEND belongs to, or nil."
  (when backend
    (cl-find (gptel-backend-name backend) ide-ai-providers
             :key (lambda (p) (ide-ai--pget p :backend-name)) :test #'equal)))

(defun ide-ai--current-provider ()
  "The provider whose backend is current, or nil."
  (ide-ai--backend-provider (bound-and-true-p gptel-backend)))

(defun ide-ai--read (prompt candidates default annotate category &optional require-match)
  "`completing-read' over CANDIDATES in the given order, annotated, like agent-shell's picker."
  (completing-read
   (format-prompt prompt default)
   (lambda (string pred action)
     (if (eq action 'metadata)
         `(metadata (category . ,category) (display-sort-function . identity)
                    (annotation-function . ,annotate))
       (complete-with-action action candidates string pred)))
   nil require-match nil nil default))

(defun ide-ai-select-model ()
  "Choose a provider, then a model; return (PROVIDER . MODEL).
The current provider and model are offered as defaults.  Any model id can be
typed, not only the suggestions."
  (let* ((cur (ide-ai--current-provider))
         (cur-model (and (boundp 'gptel-model) gptel-model))
         (named (mapcar (lambda (p) (cons (ide-ai--pget p :name) p)) ide-ai-providers))
         (pname (ide-ai--read "Provider" (mapcar #'car named) (and cur (ide-ai--pget cur :name))
                              (lambda (c) (format " — %s" (ide-ai--status (cdr (assoc c named)))))
                              'ide-ai-provider t))
         (provider (cdr (assoc pname named)))
         (models (mapcar #'symbol-name (ide-ai--models provider (and (eq provider cur) cur-model))))
         (model (ide-ai--read "Model" models (car models)
                              (lambda (c) (if (and (eq provider cur) (equal c (and cur-model (symbol-name cur-model)))) " — current" ""))
                              'ide-ai-model)))
    (cons provider (intern model))))

(defun ide-ai--apply (provider model scope &optional models)
  "Make PROVIDER and MODEL current.  SCOPE is `buffer' or `global'."
  (let ((backend (ide-ai--backend provider (delete-dups (cons model (or models (list model)))))))
    (if (eq scope 'buffer)
        (progn (setq-local gptel-backend backend) (setq-local gptel-model model))
      (setq-default gptel-backend backend gptel-model model))
    backend))

(defun ide-ai--save (provider model)
  (make-directory (file-name-directory ide-ai-model-file) t)
  (with-temp-file ide-ai-model-file (prin1 (list :provider (car provider) :model model) (current-buffer))))

(defun ide-ai--saved ()
  (when (file-readable-p ide-ai-model-file)
    (ignore-errors (with-temp-buffer (insert-file-contents ide-ai-model-file) (read (current-buffer))))))

;;;###autoload
(defun ide-ai-set-model (&optional global)
  "Choose the AI provider and model for chat and `gptel-agent'.
Inside a chat or agent buffer this changes that session only.  Elsewhere, or
with a prefix argument, it sets the default for new sessions and remembers it."
  (interactive "P")
  (require 'gptel)
  (let* ((sel (ide-ai-select-model)) (provider (car sel)) (model (cdr sel))
         (scope (if (or global (not (bound-and-true-p gptel-mode))) 'global 'buffer)))
    (unless (ide-ai--ready-p provider)
      (user-error "%s is not ready: %s" (ide-ai--pget provider :name) (ide-ai--status provider)))
    (ide-ai--apply provider model scope (ide-ai--models provider model))
    (when (and (eq scope 'global) ide-ai-remember-model) (ide-ai--save provider model))
    (message "AI model: %s · %s (%s)" (ide-ai--pget provider :name) model
             (if (eq scope 'buffer) "this session" "default for new sessions"))))

(defun ide-ai--apply-startup-model ()
  "Register available backends and apply the remembered or configured default model.
Never touches the network: Ollama's installed models are only listed in the picker."
  (dolist (p ide-ai-providers)
    (when (or (null (ide-ai--pget p :env)) (ide-ai--api-key p))
      (ignore-errors (ide-ai--backend p (and (null (ide-ai--pget p :env))
                                              (or (ide-ai--pget p :models) (list (intern ide-ai-ollama-model))))))))
  (let* ((saved (ide-ai--saved))
         (choice (or (and saved (cons (plist-get saved :provider) (plist-get saved :model)))
                     ide-ai-default-model))
         (provider (and choice (ide-ai--provider (car choice)))))
    (if (and provider (or (null (ide-ai--pget provider :env)) (ide-ai--api-key provider)))
        (ide-ai--apply provider (cdr choice) 'global)
      (when choice (message "AI model %s is not available; using the Ollama default" (cdr choice)))
      (ide-ai--apply (ide-ai--provider 'ollama) (intern ide-ai-ollama-model) 'global))))

(defun ide-ai-add-project-context (&optional max-files)
  "Add project files to the gptel context (at most MAX-FILES, small text files)."
  (interactive "P")
  (require 'gptel)
  (let* ((proj (or (project-current) (user-error "Not in a project")))
         (limit (if (numberp max-files) max-files 40))
         (all (cl-remove-if-not
               (lambda (f) (and (file-regular-p f)
                                (< (file-attribute-size (file-attributes f)) 40000)
                                (not (string-match-p "\\.\\(png\\|jpe?g\\|gif\\|pdf\\|zip\\|lock\\|elc\\)\\'" f))))
               (project-files proj)))
         (files (cl-remove-if #'ide-ai-private-file-p all)))
    (dolist (f (seq-take files limit)) (gptel-add-file f))
    (message "Added %d files to AI context%s" (min limit (length files))
             (if (< (length files) (length all))
                 (format " (skipped %d private files)" (- (length all) (length files))) ""))))

(defconst ide-ai-actions
  '(("refactor" . "Refactor the following code for clarity and maintainability without changing behaviour. Return only the code.")
    ("explain" . "Explain what the following code does, concisely, noting any pitfalls.")
    ("tests" . "Write unit tests for the following code using the project's existing test framework. Return only code."))
  "Region actions offered by `ide-ai-region-action'.")

(defun ide-ai-region-action (action)
  "Run ACTION (refactor / explain / tests) on the region or buffer using gptel."
  (interactive (list (completing-read "AI action: " (mapcar #'car ide-ai-actions) nil t)))
  (require 'gptel)
  (ide-ai--guard-send (not (use-region-p)))
  (let* ((text (if (use-region-p) (buffer-substring-no-properties (region-beginning) (region-end))
                 (buffer-string)))
         (out (get-buffer-create (format "*gptel-%s*" action)))
         (lang (replace-regexp-in-string "-ts-mode\\|-mode" "" (symbol-name major-mode))))
    (with-current-buffer out (let ((inhibit-read-only t)) (erase-buffer) (insert "Thinking…\n")))
    (display-buffer out)
    (gptel-request (format "Language: %s\n\n%s" lang text)
      :system (cdr (assoc action ide-ai-actions))
      :callback (lambda (resp info)
                  (with-current-buffer out
                    (let ((inhibit-read-only t))
                      (erase-buffer)
                      (insert (if (stringp resp) resp (format "Request failed: %s" (plist-get info :status))))
                      (when (fboundp 'markdown-mode) (markdown-mode))))))))

;;;; Privacy: keep secrets away from hosted models ---------------------------------

(defcustom ide-ai-private-file-regexps
  '("\\(\\`\\|/\\)\\.env\\(\\.[^/]*\\)?\\'"                       ; .env, .env.local
    "\\.tfvars\\(\\.json\\)?\\'" "\\.tfstate\\(\\.[^/]*\\)?\\'"
    "\\.\\(pem\\|key\\|p12\\|pfx\\|jks\\|kdbx\\|gpg\\)\\'"
    "/id_\\(rsa\\|dsa\\|ecdsa\\|ed25519\\)\\'"
    "\\(\\`\\|/\\)[._]?\\(netrc\\|authinfo\\|npmrc\\|pypirc\\|git-credentials\\|pgpass\\)\\(\\.gpg\\)?\\'"
    "/\\.\\(ssh\\|aws\\|azure\\|gnupg\\|kube\\)/" "/kubeconfig\\'"
    "/[^/]*\\(secret\\|credential\\|password\\|token\\)s?[^/]*\\.\\(json\\|ya?ml\\|env\\|txt\\|ini\\|conf\\|cfg\\|toml\\|properties\\|xml\\|csv\\)\\'"
    "/\\(secrets?\\|credentials?\\)\\'"
    "\\`/\\(proc\\|sys\\|dev\\)/"                                  ; process environments, devices
    "/\\.config/\\(gh\\|hub\\|gcloud\\|rclone\\)/" "/\\.docker/config\\.json\\'"
    "/\\.\\(vault-token\\|terraformrc\\|my\\.cnf\\|s3cfg\\|boto\\)\\'" "/rclone\\.conf\\'"
    "/\\.password-store/" "/\\.local/share/keyrings/")
  "Files that must never be sent to a hosted AI model (matched case-insensitively).
Local models are unaffected.  Extend it in user/ with `add-to-list'."
  :type '(repeat regexp))

(defcustom ide-ai-confirm-whole-buffer t
  "Ask before sending a whole buffer (no active region) to a hosted model." :type 'boolean)

(defvar-local ide-ai-private-buffer nil
  "Non-nil marks this buffer private, whatever its file name is (see `ide-ai-mark-private').")

(defun ide-ai-private-file-p (file)
  "Non-nil when FILE looks like it holds secrets (see `ide-ai-private-file-regexps')."
  (when (stringp file)
    (let ((f (expand-file-name file)) (case-fold-search t))
      (or (cl-some (lambda (re) (string-match-p re f)) ide-ai-private-file-regexps)
          (string-prefix-p (expand-file-name "var/" user-emacs-directory) f)))))

(defun ide-ai-private-buffer-p (&optional buffer)
  "Non-nil when BUFFER (default current) is marked private or visits a private file.
An indirect buffer is as private as its base buffer."
  (with-current-buffer (or buffer (current-buffer))
    (or ide-ai-private-buffer
        (ide-ai-private-file-p buffer-file-name)
        (and buffer-file-truename (ide-ai-private-file-p (expand-file-name buffer-file-truename)))
        (and (buffer-base-buffer) (ide-ai-private-buffer-p (buffer-base-buffer))))))

(defun ide-ai--local-host-p ()
  (string-match-p "\\`https?://\\(localhost\\|127\\.[0-9.]+\\|\\[::1\\]\\)\\([:/]\\|\\'\\)" ide-ai-ollama-host))

(defun ide-ai--provider-hosted-p (provider)
  "Non-nil when PROVIDER runs on someone else's machine.  Unknown providers count as hosted."
  (if (plist-member (cdr provider) :hosted)
      (plist-get (cdr provider) :hosted)
    (or (null provider)
        (and (ide-ai--pget provider :env) t)
        (if (eq (car provider) 'ollama) (not (ide-ai--local-host-p)) t))))

(defun ide-ai--backend-hosted-p (backend)
  "Non-nil when gptel BACKEND runs on someone else's machine (or is unknown)."
  (ide-ai--provider-hosted-p (ide-ai--backend-provider backend)))

(defun ide-ai-chat-hosted-p ()
  "Non-nil when chat and agent requests from this buffer go to a hosted service."
  (ide-ai--backend-hosted-p (bound-and-true-p gptel-backend)))

(defun ide-ai-fim-hosted-p ()
  "Non-nil when inline completion currently goes to a hosted service."
  (if (eq ide-ai-fim-backend 'ollama) (not (ide-ai--local-host-p)) t))

(defun ide-ai--guard-send (whole-buffer)
  "Refuse private buffers and confirm whole-buffer sends while a hosted model is current.
WHOLE-BUFFER non-nil means the entire buffer is about to be sent."
  (when (ide-ai-chat-hosted-p)
    (when (ide-ai-private-buffer-p)
      (user-error "%s looks private (secrets); not sending it to a hosted model. Switch to a local model with C-c a M, or M-x ide-ai-mark-private to change it"
                  (buffer-name)))
    (when (and whole-buffer ide-ai-confirm-whole-buffer
               (not (y-or-n-p (format "Send the whole buffer %s to %s? " (buffer-name)
                                      (let ((p (ide-ai--current-provider))) (if p (ide-ai--pget p :name) "a hosted model"))))))
      (user-error "Cancelled"))))

(defun ide-ai--inline-mode ()
  "The minor mode that shows inline suggestions for `ide-ai-fim-backend'."
  (if (eq ide-ai-fim-backend 'copilot) 'copilot-mode 'minuet-auto-suggestion-mode))

(defun ide-ai--inline-off ()
  "Turn off inline suggestions (any backend) in this buffer."
  (dolist (m '(copilot-mode minuet-auto-suggestion-mode))
    (when (and (boundp m) (symbol-value m)) (funcall m -1))))

(defun ide-ai--maybe-enable-minuet ()
  "Turn on inline suggestions when wanted, except in private buffers with a hosted model.
Only in interactive sessions and in buffers visiting a file (never in batch
runs, package compilation or scratch buffers); a backend that cannot start
(Copilot without its server or sign-in) only leaves a message."
  (when (and ide-ai-inline-completion
             (not noninteractive)
             buffer-file-name
             (not (and (ide-ai-fim-hosted-p) (ide-ai-private-buffer-p))))
    (condition-case err (funcall (ide-ai--inline-mode) 1)
      (error (message "Inline completion unavailable: %s" (error-message-string err))))))

(defun ide-ai--refresh-inline ()
  "Apply `ide-ai-inline-completion' and the current backend to every programming buffer."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (derived-mode-p 'prog-mode)
        (ide-ai--inline-off)
        (ide-ai--maybe-enable-minuet)))))

(add-hook 'prog-mode-hook #'ide-ai--maybe-enable-minuet)

;;;###autoload
(defun ide-ai-toggle-inline-completion ()
  "Switch ghost-text inline completion on or off, in this and every programming buffer."
  (interactive)
  (setq ide-ai-inline-completion (not ide-ai-inline-completion))
  (ide-ai--refresh-inline)
  (message "Inline completion %s" (if ide-ai-inline-completion "on" "off")))

(defun ide-ai--disable-minuet-in-private-buffers ()
  "After switching to a hosted completion backend, stop suggestions in private buffers."
  (when (ide-ai-fim-hosted-p)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (ide-ai-private-buffer-p) (ide-ai--inline-off))))))

;;;###autoload
(defun ide-ai-mark-private (&optional unmark)
  "Mark this buffer private: it is never sent to a hosted AI model.  With UNMARK, undo that."
  (interactive "P")
  (setq ide-ai-private-buffer (not unmark))
  (when (and (not unmark) (ide-ai-fim-hosted-p)) (ide-ai--inline-off))
  (message "%s is %s" (buffer-name) (if unmark "no longer private" "private: hosted models will not see it")))

;; Two layers.  Adding private data to gptel's context is refused early while
;; a hosted model is current.  The real guard sits where every request is
;; built (`gptel--realize-query': gptel-send, gptel-rewrite, agents, plain
;; `gptel-request'): it checks the backend that request actually uses, after
;; presets, against the sending buffer and every context entry.

(declare-function gptel-fsm-info "gptel-request")
(defvar gptel-context)
(defvar gptel-use-context)

(defvar ide-ai--local-spills)

(defun ide-ai--private-path-p (file)
  "Non-nil when FILE, its true name, or a buffer visiting it is private."
  (and (stringp file)
       (or (ide-ai-private-file-p file)
           (member (expand-file-name file) ide-ai--local-spills)
           (member (file-truename file) ide-ai--local-spills)
           (ide-ai-private-file-p (file-truename file))
           (and (file-directory-p file) (ide-ai-private-file-p (file-name-as-directory file)))
           (when-let* ((b (get-file-buffer file))) (ide-ai-private-buffer-p b)))))

;; Kills made in a private buffer are remembered (the strings themselves,
;; weakly, so they go when the kill ring drops them).  gptel's commands that
;; send a kill directly -- `gptel-context-add-current-kill' and the menu's
;; kill-ring prompt -- refuse those for hosted models.  Pasting a secret into
;; another buffer is an explicit act this cannot follow; that text is sent.

(defvar ide-ai--private-kills (make-hash-table :test 'equal :weakness 'key)
  "Strings killed in private buffers.")

(defun ide-ai--record-private-kill (&rest _)
  "After `kill-new': remember the new kill when it came from a private buffer."
  (when (and kill-ring (stringp (car kill-ring)) (ide-ai-private-buffer-p))
    (puthash (car kill-ring) t ide-ai--private-kills)))

(advice-add 'kill-new :after #'ide-ai--record-private-kill)

(defun ide-ai--private-kill-p (text)
  (and (stringp text) (gethash text ide-ai--private-kills)))

(defvar ide-ai--private-context-kills (make-hash-table :test 'equal)
  "Private kills added to gptel's context, kept for as long as the context may hold them.")

(defun ide-ai--holds-private-kill-p (buffer)
  "Non-nil when BUFFER is gptel's kill-ring context and a segment of it is a private kill.
`gptel-context-add-current-kill' separates accumulated kills with \"\\n----\\n\"."
  (and (equal (buffer-name buffer) " *gptel-kill-ring-context*")
       (cl-some (lambda (seg) (or (ide-ai--private-kill-p seg) (gethash seg ide-ai--private-context-kills)))
                (split-string (with-current-buffer buffer
                                (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
                              "\n----\n"))))

(defun ide-ai--private-context (context)
  "Names of the private buffers and files among gptel CONTEXT entries."
  (let (found)
    (dolist (entry context (nreverse found))
      (let ((src (if (consp entry) (car entry) entry)))
        (cond ((bufferp src)
               (when (and (buffer-live-p src)
                          (or (ide-ai-private-buffer-p src) (ide-ai--holds-private-kill-p src)))
                 (push (buffer-name src) found)))
              ((stringp src)
               (dolist (f (if (file-directory-p src) (directory-files-recursively src "") (list src)))
                 (when (ide-ai--private-path-p f) (push (file-name-nondirectory f) found)))))))))

(defvar ide-ai--origin-buffer nil
  "Buffer a prompt was taken from when gptel sends it from another (its menu's chat redirect).")

(defun ide-ai--check-request (backend buffer context)
  "Signal `user-error' if a request from BUFFER with CONTEXT would take private data to BACKEND.
The prompt being built is the current buffer."
  (when (ide-ai--backend-hosted-p backend)
    (when (and (buffer-live-p ide-ai--origin-buffer) (ide-ai-private-buffer-p ide-ai--origin-buffer))
      (setq buffer ide-ai--origin-buffer))
    (when (and (buffer-live-p buffer) (ide-ai-private-buffer-p buffer))
      (user-error "%s looks private (secrets); not sending it to a hosted model. Switch to a local model with C-c a M, or M-x ide-ai-mark-private to change it"
                  (buffer-name buffer)))
    (when-let* ((private (ide-ai--private-context context)))
      (user-error "The AI context holds private data (%s); not sending it to a hosted model. Remove it with M-x gptel-context-remove-all, or switch to a local model with C-c a M"
                  (string-join private ", ")))))

(defun ide-ai--guard-request (orig fsm)
  "Around `gptel--realize-query': refuse FSM's request if it would leak private data."
  (let* ((info (gptel-fsm-info fsm)) (data (plist-get info :data)))
    (when (buffer-live-p data)
      (condition-case err
          (with-current-buffer data
            (ide-ai--check-request gptel-backend (plist-get info :buffer)
                                   (and gptel-use-context gptel-context)))
        (user-error
         (kill-buffer data)
         (ide-ai--reset-status (plist-get info :buffer))
         (signal (car err) (cdr err)))))
    (funcall orig fsm)))

(declare-function gptel--update-status "gptel")

(defun ide-ai--reset-status (buffer)
  "Put BUFFER's gptel status back to Ready after a refused request."
  (when (and (buffer-live-p buffer) (fboundp 'gptel--update-status))
    (with-current-buffer buffer
      (when (bound-and-true-p gptel-mode) (gptel--update-status " Ready" 'success)))))

(defun ide-ai--guard-menu-send (orig &rest args)
  "Around gptel's menu send: it can copy the prompt into a chat buffer and send
from there, so refuse a private source buffer up front if either side is hosted."
  (let* ((targets (cl-loop for a in (car args)
                           when (and (stringp a) (string-prefix-p "g" a))
                           collect (get-buffer (substring a 1))))
         (hosted (or (ide-ai-chat-hosted-p)
                     (cl-some (lambda (b) (and (buffer-live-p b)
                                               (with-current-buffer b (ide-ai-chat-hosted-p))))
                              targets))))
    (when (and hosted (ide-ai-private-buffer-p))
      (user-error "%s looks private (secrets); not sending it to a hosted model. Switch to a local model with C-c a M, or M-x ide-ai-mark-private to change it"
                  (buffer-name)))
    ;; The kill-ring prompt ("y", or C-u for an older kill) is checked on
    ;; the string actually chosen.
    (let ((ide-ai--origin-buffer (current-buffer))
          (ide-ai--checking-kills hosted))
      (apply orig args))))

(defvar ide-ai--checking-kills nil "Non-nil while a hosted menu send may read the kill ring.")

(defun ide-ai--check-chosen-kill (text)
  "Filter the kill read by gptel's menu: refuse a private one while sending to a hosted model."
  (when (and ide-ai--checking-kills (ide-ai--private-kill-p text))
    (user-error "That kill was copied from a private buffer; not sending it to a hosted model"))
  text)

(advice-add 'current-kill :filter-return #'ide-ai--check-chosen-kill)
(advice-add 'read-from-kill-ring :filter-return #'ide-ai--check-chosen-kill)

(defun ide-ai--guard-current-kill (&rest _)
  "Before `gptel-context-add-current-kill': refuse text killed in a private buffer."
  (let ((kill (and kill-ring (current-kill 0 t))))
    (when (ide-ai--private-kill-p kill)
      (when (ide-ai-chat-hosted-p)
        (user-error "The last kill was copied from a private buffer; not adding it to a hosted model's context"))
      ;; Allowed for a local model; the send guard refuses it if a hosted one follows.
      (puthash (substring-no-properties kill) t ide-ai--private-context-kills))))

(defvar ide-ai--adding-directory nil "Non-nil while gptel adds a directory to its context.")

(defun ide-ai--guard-gptel-file (orig path &rest args)
  "Around `gptel-context-add-file': keep private files out of a hosted model's context.
A private file inside an added directory is skipped; one named directly is refused."
  (if (and (ide-ai-chat-hosted-p) (not (file-directory-p path)) (ide-ai--private-path-p path))
      (if ide-ai--adding-directory
          (message "Skipped private file %s" path)
        (user-error "%s looks private; not adding it to a hosted model's context" (file-name-nondirectory path)))
    (apply orig path args)))

(defun ide-ai--guard-gptel-directory (orig &rest args)
  (let ((ide-ai--adding-directory t)) (apply orig args)))

(defun ide-ai--guard-gptel-region (buffer &rest _)
  "Before `gptel-context--add-region' (regions, buffers, ibuffer): refuse private BUFFER."
  (when (and (ide-ai-chat-hosted-p) (ide-ai-private-buffer-p buffer))
    (user-error "%s looks private (secrets); not adding it to a hosted model's context" (buffer-name buffer))))

(with-eval-after-load 'gptel
  (define-key gptel-mode-map (kbd "C-c C-k") #'gptel-abort))

(with-eval-after-load 'gptel-request
  (advice-add 'gptel--realize-query :around #'ide-ai--guard-request)
  (advice-add 'gptel--handle-tool-use :around #'ide-ai--with-request-backend))

;; `gptel-add' and `gptel-add-file' are aliases: advise the functions they
;; name, which dired, ibuffer and directory adds call directly.
(with-eval-after-load 'gptel-context
  (advice-add 'gptel-context-add-file :around #'ide-ai--guard-gptel-file)
  (advice-add 'gptel-context--add-directory :around #'ide-ai--guard-gptel-directory)
  (advice-add 'gptel-context--add-region :before #'ide-ai--guard-gptel-region)
  (advice-add 'gptel-context-add-current-kill :before #'ide-ai--guard-current-kill))

(with-eval-after-load 'gptel-transient
  (advice-add 'gptel--suffix-send :around #'ide-ai--guard-menu-send))

;;;; gptel-agent -----------------------------------------------------------

(declare-function gptel-agent "gptel-agent")
(defvar gptel-agent-skill-dirs)

(defun ide-ai-claude-plugin-skill-dirs ()
  "Skill directories of installed Claude Code plugins, newest versions first.
Plugins live in ~/.claude/plugins/cache/MARKETPLACE/PLUGIN/VERSION/skills."
  (sort (cl-remove-if-not #'file-directory-p
                          (file-expand-wildcards (expand-file-name "~/.claude/plugins/cache/*/*/*/skills")))
        #'string>))

(defun ide-ai--refresh-skill-dirs (&rest _)
  "Make `gptel-agent' see skills of installed Claude Code plugins.
`gptel-agent-skill-dirs' already covers ~/.claude/skills and .claude/skills."
  (setq gptel-agent-skill-dirs
        (delete-dups (append gptel-agent-skill-dirs (ide-ai-claude-plugin-skill-dirs)))))

(use-package gptel-agent
  :commands (gptel-agent gptel-agent-update)
  :config
  (advice-add 'gptel-agent :before #'ide-ai--refresh-skill-dirs))

;; agent-shell: ACP coding agents in a shell buffer.  Pi is the default agent
;; (`ide-ai-pi', C-c a p); point `agent-shell-pi-acp-command' at your Pi ACP
;; adapter (for a sandboxed Pi: ("agent-box" "acp")).
(declare-function agent-shell-pi-start-agent "agent-shell-pi")

(use-package agent-shell
  :ensure t
  :commands (agent-shell agent-shell-pi-start-agent))

;;;###autoload
(defun ide-ai-pi ()
  "Start a Pi coding agent session (agent-shell) for the current project."
  (interactive)
  (let ((default-directory (if-let* ((p (project-current))) (project-root p) default-directory)))
    (agent-shell-pi-start-agent)))

;; The agent's Read and Grep tools run without asking, so they get the same
;; rule: private files stay away from hosted models.  Tools run inside
;; `gptel--handle-tool-use', which knows the backend of the request.

(defvar ide-ai--request-backend nil "Backend of the request whose tools are running.")
(defvar ide-ai--grep-root nil "Directory of a hosted agent's Grep, while it runs.")

(defun ide-ai--with-request-backend (orig fsm &rest args)
  (let ((ide-ai--request-backend (plist-get (gptel-fsm-info fsm) :backend)))
    (apply orig fsm args)))

(defun ide-ai--tool-hosted-p ()
  (ide-ai--backend-hosted-p (or ide-ai--request-backend (bound-and-true-p gptel-backend))))

(defun ide-ai--guard-agent-read (file &rest _)
  "Before the agent's Read tool: refuse private FILE for hosted models."
  (when (and (stringp file) (ide-ai--tool-hosted-p) (ide-ai--private-path-p (expand-file-name file)))
    (error "Error: %s is private (secrets) and is not shared with a hosted model" file)))

(defun ide-ai--guard-agent-grep (orig regex path &rest args)
  "Around the agent's Grep tool: refuse a private PATH and drop matches in private files."
  (if (not (ide-ai--tool-hosted-p))
      (apply orig regex path args)
    (let ((full (expand-file-name (substitute-in-file-name path))))
      (when (ide-ai--private-path-p full)
        (error "Error: %s is private (secrets) and is not searched for a hosted model" path))
      ;; A single (non-private) file needs no filtering: its lines may not name it.
      (let ((ide-ai--grep-root (and (file-directory-p full)
                                    (or (locate-dominating-file full ".git") full))))
        (apply orig regex path args)))))

(defun ide-ai--grep-unquote (path)
  "PATH as git prints it: C-quoted (\"caf\\303\\251.py\") when not plain ASCII."
  (if (and (string-prefix-p "\"" path) (string-suffix-p "\"" path) (> (length path) 1))
      (decode-coding-string
       (replace-regexp-in-string
        "\\\\\\([0-7]\\{3\\}\\|.\\)"
        (lambda (m)
          (let ((c (substring m 1)))
            (if (= (length c) 3) (unibyte-string (string-to-number c 8))
              (pcase c ("n" "\n") ("t" "\t") (_ c)))))
        (substring path 1 -1) t t)
       'utf-8)
    path))

(defvar ide-ai--grep-files nil "Cache of `file-regular-p' answers during one filter run.")

(defun ide-ai--grep-file-p (file)
  (if (not ide-ai--grep-files) (file-regular-p file)
    (let ((v (gethash file ide-ai--grep-files 'unknown)))
      (if (eq v 'unknown) (puthash file (file-regular-p file) ide-ai--grep-files) v))))

(defun ide-ai--grep-line-file (line root)
  "The existing file a PATH:N: or PATH-N- grep LINE names, relative to ROOT, or nil.
The longest prefix that is a file and is followed by such a separator wins, so
paths holding digits and dashes resolve correctly."
  (let ((start 0) file)
    (while (string-match "[:-][0-9]+[:-]" line start)
      (let ((cand (expand-file-name (ide-ai--grep-unquote (substring line 0 (match-beginning 0))) root)))
        (when (and (> (match-beginning 0) 0) (ide-ai--grep-file-p cand)) (setq file cand)))
      (setq start (1+ (match-beginning 0))))
    file))

(defun ide-ai--grep-drop-private ()
  "Remove lines of private files from the grep output in this buffer.
Handles git grep and grep (PATH:N:TEXT, PATH-N-TEXT) and ripgrep's
--heading blocks.  Fails closed: a line whose file cannot be told is dropped."
  (let* ((root ide-ai--grep-root) (heading 'none) (dropped 0) kept
         (ide-ai--grep-files (make-hash-table :test 'equal))
         (verdicts (make-hash-table :test 'equal))
         (private (lambda (f) (let ((v (gethash f verdicts 'unknown)))
                                (if (eq v 'unknown) (puthash f (ide-ai--private-path-p f) verdicts) v)))))
    (dolist (line (split-string (buffer-string) "\n"))
      (let* ((whole (and (not (string-empty-p line)) (not (string-match-p "\\`[0-9]+[:-]" line))
                         (expand-file-name (ide-ai--grep-unquote line) root)))
             (whole (and whole (ide-ai--grep-file-p whole) whole))
             (file (and (not (string-empty-p line)) (not whole) (ide-ai--grep-line-file line root))))
        (cond ((string-empty-p line) (setq heading 'none) (push line kept))
              ;; A line naming a whole file is an rg heading, before any split.
              (whole (setq heading (if (funcall private whole) 'private 'public))
                     (if (eq heading 'private) (cl-incf dropped) (push line kept)))
              (file (if (funcall private file) (cl-incf dropped) (push line kept)))
              ((equal line "--") (if (eq heading 'private) (cl-incf dropped) (push line kept)))
              ((string-prefix-p "Error: search failed" line) (push line kept))
              ((and (not (string-match-p "\\`[0-9]+[:-]" line))
                    (ide-ai--grep-file-p (expand-file-name (ide-ai--grep-unquote line) root)))
               (setq heading (if (funcall private (expand-file-name (ide-ai--grep-unquote line) root)) 'private 'public))
               (if (eq heading 'private) (cl-incf dropped) (push line kept)))
              ((and (eq heading 'public) (string-match-p "\\`[0-9]+[:-]" line)) (push line kept))
              (t (cl-incf dropped)))))
    (when (> dropped 0)
      ;; Separators and blank lines left around dropped groups collapse.
      (let (out)
        (dolist (l (nreverse kept))
          (unless (and (member l '("--" "")) (or (null out) (member (car out) '("--" "")))) (push l out)))
        (while (member (car out) '("--" "")) (pop out))
        (setq kept out))
      (erase-buffer)
      (insert (string-join (nreverse kept) "\n")
              (format "\n[%d lines from private files withheld from this hosted model]" dropped)))))

;; gptel-agent spills large tool output to a temp file the model may Read
;; later.  Spills made for a local model are unfiltered, so they count as
;; private; a hosted model's own spills are filtered already.
(defvar ide-ai--local-spills nil "Spill files gptel-agent wrote for local-model requests.")
(defvar ide-ai--recording-spills nil)

(defun ide-ai--record-spill (file)
  (when ide-ai--recording-spills (push (expand-file-name file) ide-ai--local-spills))
  file)

(advice-add 'make-nearby-temp-file :filter-return #'ide-ai--record-spill)

(defun ide-ai--agent-truncate (orig &rest args)
  "Around gptel-agent's output truncation: filter a hosted Grep first; record local spills."
  (if (ide-ai--tool-hosted-p)
      (progn (when ide-ai--grep-root (ide-ai--grep-drop-private))
             (apply orig args))
    (let ((ide-ai--recording-spills t)) (apply orig args))))

(with-eval-after-load 'gptel-agent-tools
  (advice-add 'gptel-agent--read-file-lines :before #'ide-ai--guard-agent-read)
  (advice-add 'gptel-agent--grep :around #'ide-ai--guard-agent-grep)
  (advice-add 'gptel-agent--truncate-buffer :around #'ide-ai--agent-truncate))

(defun ide-ai-agent ()
  "Start a `gptel-agent' session for the current project.
The agent can read and edit files, run shell commands and Elisp, search the
web and use Agent Skills; everything except reads and web access asks first."
  (interactive)
  (call-interactively #'gptel-agent))

(defun ide-ai-chat ()
  "Open a gptel chat buffer."
  (interactive) (call-interactively #'gptel))

;;;; MCP servers: let outside agents ask this Emacs ---------------------------------
;; Two servers expose the running McGeEmacs to agents such as Claude Code:
;; emcp (HTTP, inside Emacs) for symbols, definitions, references, Info and,
;; with richer profiles, variables, screenshots, eval and keys; elisp-dev-mcp
;; (stdio through emacsclient) for docs, definitions, Info lookup and reading
;; installed Elisp source.  Neither starts unless asked: `ide-ai-mcp-start'.

(declare-function emcp-start "emcp")
(declare-function emcp-stop "emcp")
(declare-function emcp-server-url "emcp")
(declare-function mcp-server-lib-start "mcp-server-lib-commands")
(declare-function elisp-dev-mcp-enable "elisp-dev-mcp")
(declare-function server-start "server")
(declare-function emcp--server-transport "emcp-core")
(declare-function mcp-server-lib-tool-throw "mcp-server-lib")
(defvar emcp-http-port)
(defvar emcp--servers)
(defvar server-process)
(defvar elisp-dev-mcp-additional-allowed-dirs)
(defvar ide-modules-directory)
(defvar package-user-dir)
(defvar server-name)

(defcustom ide-ai-mcp-autostart nil
  "Start the MCP servers when McGeEmacs starts (not in batch runs or daemons).
Otherwise start them with `ide-ai-mcp-start'."
  :type 'boolean)

(defcustom ide-ai-mcp-server-name "mcgeemacs"
  "Emacs server name used for emacsclient, and so by elisp-dev-mcp.
Distinct from the default \"server\" so another Emacs can run its own.
Only applied when this Emacs is not already running a server."
  :type 'string)

(defcustom ide-ai-emcp-profile 'inspect
  "Profile emcp serves: `inspect' (read-only lookups), `develop' (adds
reading and setting variables and screenshots) or `full-control' (adds eval
and sending keys, each confirmed by you first)."
  :type '(choice (const inspect) (const develop) (const full-control)))

(defcustom ide-ai-emcp-port 38913
  "Fixed localhost port for emcp, so MCP clients can be registered once."
  :type 'integer)

(use-package emcp
  :commands (emcp-start emcp-stop emcp-session-manager)
  :config (setq emcp-http-port ide-ai-emcp-port))

(use-package elisp-dev-mcp
  :commands (elisp-dev-mcp-enable elisp-dev-mcp-disable)
  :config
  ;; Installed packages (package.el and package-vc checkouts) and McGeEmacs's
  ;; own modules are readable; user/ is not, as it may hold personal settings
  ;; (see below for definitions).
  (setq elisp-dev-mcp-additional-allowed-dirs
        (list package-user-dir
              ide-modules-directory (expand-file-name "lisp/" user-emacs-directory))))

;; user/ stays private to outside agents: both servers can return the source
;; of any loaded definition, so lookups that resolve into user/ are withheld.
;; (emcp's eval in the full-control profile can still read anything; it asks.)

(defun ide-ai--user-config-file-p (file)
  "Non-nil when FILE is under the user/ directory of this config."
  (and (stringp file)
       (let ((user (file-name-as-directory (expand-file-name "user/" user-emacs-directory)))
             (f (expand-file-name file)))
         (or (string-prefix-p user f)
             (string-prefix-p (file-truename user) (file-truename f))))))

(defun ide-ai--elisp-dev-hide-user (_fn-name _sym file &rest _)
  "Before elisp-dev-mcp reads a definition from FILE: refuse files in user/."
  (when (ide-ai--user-config-file-p file)
    (mcp-server-lib-tool-throw "Definition withheld: it is in the user's private user/ configuration")))

(defun ide-ai--emcp-hide-user (def)
  "Filter emcp's definition lookup DEF: withhold source from user/."
  (if (ide-ai--user-config-file-p (plist-get def :file))
      (list :file "user/ (private)" :source "Definition withheld: it is in the user's private user/ configuration.")
    def))

(defvar ide-ai--emcp-hiding-user nil "Non-nil while an emcp tool reads loaded files.")

(defun ide-ai--emcp-references-hide-user (orig &rest args)
  (let ((ide-ai--emcp-hiding-user t)) (apply orig args)))

(defun ide-ai--elisp-refs-hide-user (paths)
  (if ide-ai--emcp-hiding-user (cl-remove-if #'ide-ai--user-config-file-p paths) paths))

(defun ide-ai--commentary-hide-user (orig &optional file)
  "Around `lm-commentary': emcp's describe of a feature gets no commentary from user/."
  (if (and ide-ai--emcp-hiding-user (ide-ai--user-config-file-p (or file buffer-file-name)))
      "Commentary withheld: the file is in the user's private user/ configuration."
    (funcall orig file)))

(with-eval-after-load 'elisp-dev-mcp
  (advice-add 'elisp-dev-mcp--get-function-definition-from-file :before #'ide-ai--elisp-dev-hide-user))

(with-eval-after-load 'emcp-tools
  (advice-add 'emcp-tools--find-definition :filter-return #'ide-ai--emcp-hide-user)
  (advice-add 'emcp-tools-find-references :around #'ide-ai--emcp-references-hide-user)
  (advice-add 'emcp-tools-describe :around #'ide-ai--emcp-references-hide-user))

(with-eval-after-load 'lisp-mnt
  (advice-add 'lm-commentary :around #'ide-ai--commentary-hide-user))

(with-eval-after-load 'elisp-refs
  (advice-add 'elisp-refs--loaded-paths :filter-return #'ide-ai--elisp-refs-hide-user))

;; emcp listens on a fixed localhost port; a web page could reach it through
;; DNS rebinding.  Only requests addressed to localhost, and not sent from
;; another site's page, are answered.

(defconst ide-ai--localhost-re "\\(127\\.0\\.0\\.1\\|localhost\\|\\[::1\\]\\)")

(defun ide-ai--emcp-request-allowed-p (headers port)
  "Non-nil when HEADERS address localhost on PORT and carry no foreign Origin."
  (let ((host (alist-get 'Host headers)) (origin (alist-get 'Origin headers)))
    (and (stringp host)
         (string-match-p (format "\\`%s:%d\\'" ide-ai--localhost-re port) (downcase (string-trim host)))
         (or (null origin)
             (string-match-p (format "\\`https?://%s\\(:[0-9]+\\)?\\'" ide-ai--localhost-re)
                             (downcase (string-trim origin)))))))

(defun ide-ai--emcp-guard-request (orig server request send-response)
  "Around emcp's HTTP handler: answer 403 to requests not addressed to localhost."
  (if (ide-ai--emcp-request-allowed-p (plist-get request :headers)
                                      (process-contact (emcp--server-transport server) :service))
      (funcall orig server request send-response)
    (funcall send-response '(:status Forbidden))))

(with-eval-after-load 'emcp-http
  (advice-add 'emcp--http-on-request :around #'ide-ai--emcp-guard-request))

(defun ide-ai-mcp-elisp-dev-enable ()
  "Make elisp-dev-mcp answer requests; the stdio script calls this per client.
Registration is reference-counted, so each connecting client enables once and
`elisp-dev-mcp-disable' undoes it when that client leaves."
  (require 'mcp-server-lib-commands)
  (require 'elisp-dev-mcp)
  (ignore-errors (mcp-server-lib-start))  ; errors only when already running
  (elisp-dev-mcp-enable))

(defun ide-ai--server-live-p ()
  "Non-nil when THIS Emacs runs a server.
`server-running-p' is also true when another Emacs owns the socket."
  (and (boundp 'server-process) (processp server-process) (process-live-p server-process)))

(defun ide-ai--mcp-ensure-server ()
  "Run an Emacs server for emacsclient, named `ide-ai-mcp-server-name' if new.
Return the server name clients should use."
  (require 'server)
  (unless (ide-ai--server-live-p)
    (let ((old server-name))
      (setq server-name ide-ai-mcp-server-name)
      (server-start)
      (unless (ide-ai--server-live-p)
        (setq server-name old)
        (error "Could not start the Emacs server %S; another Emacs probably uses that name. Set `ide-ai-mcp-server-name' to another name"
               ide-ai-mcp-server-name))))
  server-name)

(defvar ide-ai--emcp-profile nil "The emcp profile `ide-ai-mcp-start' started, if running.")

(defun ide-ai--emcp-url ()
  "URL of the emcp server McGeEmacs started, or nil."
  (when-let* ((server (and ide-ai--emcp-profile (boundp 'emcp--servers)
                           (alist-get ide-ai--emcp-profile emcp--servers))))
    (emcp-server-url server)))

(defun ide-ai-mcp-stdio-script ()
  "The stdio bridge elisp-dev-mcp clients run, from the installed mcp-server-lib."
  (when-let* ((lib (locate-library "mcp-server-lib")))
    (expand-file-name "emacs-mcp-stdio.sh" (file-name-directory lib))))

;;;###autoload
(defun ide-ai-mcp-start ()
  "Start the Emacs server (for elisp-dev-mcp) and emcp on `ide-ai-emcp-port'."
  (interactive)
  (let ((name (ide-ai--mcp-ensure-server)))
    (require 'emcp)
    ;; A different profile started earlier holds the port: replace it.
    (when (and (ide-ai--emcp-url) (not (eq ide-ai--emcp-profile ide-ai-emcp-profile)))
      (ide-ai-mcp-stop))
    (setq emcp-http-port ide-ai-emcp-port)
    (emcp-start ide-ai-emcp-profile)
    (setq ide-ai--emcp-profile ide-ai-emcp-profile)
    (message "MCP: emcp (%s) at %s · elisp-dev-mcp via emacsclient -s %s"
             ide-ai-emcp-profile (ide-ai--emcp-url) name)))

;;;###autoload
(defun ide-ai-mcp-stop ()
  "Stop emcp.  The Emacs server keeps running for emacsclient."
  (interactive)
  (if (not (ide-ai--emcp-url))
      (message "MCP: emcp is not running")
    (emcp-stop ide-ai--emcp-profile)
    (message "MCP: emcp (%s) stopped" ide-ai--emcp-profile))
  (setq ide-ai--emcp-profile nil))

(defun ide-ai-mcp-claude-commands ()
  "The `claude mcp add' argument lists registering both servers for this Emacs."
  (require 'server)
  (let ((script (or (ide-ai-mcp-stdio-script) (user-error "The mcp-server-lib package is not installed"))))
    (list (list "mcp" "add" "--scope" "user" "--transport" "http" "emcp"
                (format "http://127.0.0.1:%d/mcp" ide-ai-emcp-port))
          (list "mcp" "add" "--scope" "user" "elisp-dev" "--" script
                (concat "--socket=" (if (ide-ai--server-live-p) server-name ide-ai-mcp-server-name))
                "--init-function=ide-ai-mcp-elisp-dev-enable"
                "--stop-function=elisp-dev-mcp-disable"
                "--server-id=elisp-dev-mcp"))))

;;;###autoload
(defun ide-ai-mcp-register-claude ()
  "Register emcp and elisp-dev-mcp with Claude Code (user scope).
Existing entries named emcp and elisp-dev are replaced."
  (interactive)
  (unless (executable-find "claude") (user-error "The claude CLI is not on PATH"))
  (let ((commands (ide-ai-mcp-claude-commands)))
    (when (y-or-n-p "Register emcp and elisp-dev with Claude Code, replacing any existing entries? ")
      (dolist (args commands)
        (let ((name (if (member "--transport" args) "emcp" "elisp-dev")))
          (call-process "claude" nil nil nil "mcp" "remove" "--scope" "user" name)
          (with-temp-buffer
            (unless (zerop (apply #'call-process "claude" nil t nil args))
              (user-error "Command claude mcp add %s failed: %s" name (string-trim (buffer-string)))))))
      (message "Registered emcp and elisp-dev with Claude Code; start them with %s"
               (substitute-command-keys "\\[ide-ai-mcp-start]")))))

(defun ide-ai--mcp-autostart ()
  "Start the MCP servers at startup when `ide-ai-mcp-autostart' asks for it."
  (when (and ide-ai-mcp-autostart (not noninteractive) (not (daemonp)))
    (condition-case err (ide-ai-mcp-start)
      (error (display-warning 'ide (format "MCP servers did not start: %s" (error-message-string err)))))))

(add-hook 'emacs-startup-hook #'ide-ai--mcp-autostart)

;;;; MCP servers gptel can use (the other direction: tools for the chat) -----------

(defvar mcp-hub-servers)
(defvar mcp-server-wait-initial-time)
(declare-function gptel-mcp-connect "gptel-integrations")

(defcustom ide-ai-mcp-client-servers nil
  "MCP servers whose tools gptel may use, as (NAME . PLIST) entries.
PLIST is what `mcp-hub-servers' takes: (:url URL) for HTTP servers or
(:command CMD :args ARGS :env (:VAR \"value\" ...)) for stdio ones, plus
:connect, either `startup' (connect in the background soon after Emacs starts)
or `first-use' (the default: connect when gptel sends its first request).
An entry whose :command is not installed is skipped.  Only list servers whose
tools are safe for an unsandboxed chat: lookups and notes, not code edits."
  :type '(alist :key-type string :value-type plist))

(use-package mcp :defer t)              ; provides mcp-hub, which gptel's MCP support uses

(defun ide-ai--mcp-client-command (cmd)
  "CMD with a leading ~ expanded; bare names stay for PATH lookup."
  (if (string-prefix-p "~" cmd) (expand-file-name cmd) cmd))

(defun ide-ai--mcp-client-available-p (plist)
  "Non-nil when the server described by PLIST can run on this machine."
  (let ((cmd (plist-get plist :command)))
    (or (null cmd) (and (executable-find (ide-ai--mcp-client-command cmd)) t))))

(defun ide-ai--mcp-client-names (connect)
  "Names of usable servers in `ide-ai-mcp-client-servers' to connect at CONNECT."
  (cl-loop for (name . plist) in ide-ai-mcp-client-servers
           when (and (eq (or (plist-get plist :connect) 'first-use) connect)
                     (ide-ai--mcp-client-available-p plist))
           collect name))

(defun ide-ai--mcp-client-register ()
  "Copy `ide-ai-mcp-client-servers' into `mcp-hub-servers'."
  (dolist (entry ide-ai-mcp-client-servers)
    (let ((plist (copy-sequence (cdr entry))))
      (cl-remf plist :connect)
      (when (plist-get plist :command)
        (setq plist (plist-put plist :command (ide-ai--mcp-client-command (plist-get plist :command)))))
      (setf (alist-get (car entry) mcp-hub-servers nil nil #'equal) plist))))

(defun ide-ai-mcp-client-connect (&optional connect)
  "Connect the gptel MCP servers marked CONNECT (`startup' or `first-use'; both if nil).
This waits for the servers (a second or two), so their tools are ready for the
request that follows; it runs when Emacs is idle or just before gptel's first
request, never during startup itself."
  (interactive)
  (let ((names (if connect (ide-ai--mcp-client-names connect)
                 (append (ide-ai--mcp-client-names 'startup) (ide-ai--mcp-client-names 'first-use)))))
    (when names
      (require 'mcp-hub)
      (require 'gptel-integrations)
      (ide-ai--mcp-client-register)
      ;; mcp.el pauses 2 s after each server's handshake; these answer at once.
      (let ((mcp-server-wait-initial-time 0.2))
        (gptel-mcp-connect names t)))))

(defun ide-ai--mcp-client-first-use (&rest _)
  "Before gptel's first request: connect the `first-use' MCP servers, once."
  (advice-remove 'gptel-request #'ide-ai--mcp-client-first-use)
  (condition-case err (ide-ai-mcp-client-connect 'first-use)
    (error (message "MCP servers for gptel did not connect: %s" (error-message-string err)))))

(defun ide-ai--mcp-client-startup ()
  "Shortly after startup: connect the `startup' MCP servers, and arm the rest."
  (when (and ide-ai-mcp-client-servers (not noninteractive))
    (advice-add 'gptel-request :before #'ide-ai--mcp-client-first-use)
    (when (ide-ai--mcp-client-names 'startup)
      (run-with-idle-timer
       2 nil (lambda () (condition-case err (ide-ai-mcp-client-connect 'startup)
                          (error (message "MCP servers for gptel did not connect: %s"
                                          (error-message-string err)))))))))

(add-hook 'emacs-startup-hook #'ide-ai--mcp-client-startup)

(provide '13-ai)
;;; 13-ai.el ends here
