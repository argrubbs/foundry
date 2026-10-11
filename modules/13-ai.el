;;; 13-ai.el --- Inline FIM completion and agentic chat -*- lexical-binding: t; -*-

;;; Commentary:
;; minuet for ghost-text FIM completion (local Ollama or cloud), gptel for
;; conversational / region-based work, gptel-agent for autonomous tool-using
;; sessions (file edits, shell, Elisp, web, sub-agents and Agent Skills).

;;; Code:

(require 'cl-lib)
(require 'project)
(eval-when-compile (require 'gptel nil t))  ; struct types, for the byte-compiler

(defgroup ide-ai nil "AI assistant settings." :group 'tools)

(defcustom ide-ai-fim-backend 'ollama
  "Backend for inline completion: `ollama', `gemini' or `openai-fim'."
  :type '(choice (const ollama) (const gemini) (const openai-fim)))

(defcustom ide-ai-ollama-model "qwen2.5-coder:7b" "Ollama FIM model." :type 'string)
(defcustom ide-ai-ollama-host "http://localhost:11434" "Ollama base URL." :type 'string)

(declare-function minuet-auto-suggestion-mode "minuet")
(declare-function minuet-accept-suggestion "minuet")
(declare-function minuet-accept-suggestion-word "minuet")
(declare-function minuet-accept-suggestion-line "minuet")
(declare-function minuet-next-suggestion "minuet")
(declare-function minuet-previous-suggestion "minuet")
(declare-function minuet-dismiss-suggestion "minuet")
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
           (plist-put (plist-put (plist-put (plist-put (plist-put (copy-sequence minuet-openai-fim-compatible-options)
                                                                 :end-point (concat ide-ai-ollama-host "/v1/completions"))
                                                           :api-key (lambda () "ollama"))
                                             :name "Ollama")
                                 :optional '(:max_tokens 56))
                      :model ide-ai-ollama-model)))
    ('gemini
     (setq minuet-provider 'gemini
           minuet-gemini-options (plist-put (copy-sequence minuet-gemini-options)
                                            :model "gemini-2.0-flash")))
    ('openai-fim (setq minuet-provider 'openai-fim-compatible)))
  (ide-ai--disable-minuet-in-private-buffers)
  (message "Inline completion backend: %s" ide-ai-fim-backend))

(defun ide-ai-switch-backend (backend)
  "Switch the inline completion BACKEND."
  (interactive (list (intern (completing-read "FIM backend: " '("ollama" "gemini" "openai-fim") nil t))))
  (setq ide-ai-fim-backend backend)
  (ide-ai-apply-fim-backend))

(use-package minuet
  :ensure (:host github :repo "milanglacier/minuet-ai.el")
  :hook (prog-mode . ide-ai--maybe-enable-minuet)
  :init
  (setq minuet-auto-suggestion-debounce-delay 0.4
        minuet-auto-suggestion-throttle-delay 1.0
        minuet-n-completions 1
        minuet-request-timeout 6
        minuet-context-window 4096
        minuet-show-error-message-on-minibuffer t)
  :config
  (ide-ai-apply-fim-backend)
  (add-to-list 'minuet-auto-suggestion-block-predicates
               (lambda () (and (bound-and-true-p corfu--frame) (frame-visible-p corfu--frame))))
  (define-key minuet-active-mode-map (kbd "TAB") #'minuet-accept-suggestion)
  (define-key minuet-active-mode-map (kbd "<tab>") #'minuet-accept-suggestion)
  (define-key minuet-active-mode-map (kbd "C-<return>") #'minuet-accept-suggestion-word)
  (define-key minuet-active-mode-map (kbd "S-<return>") #'minuet-accept-suggestion-line)
  (define-key minuet-active-mode-map (kbd "M-n") #'minuet-next-suggestion)
  (define-key minuet-active-mode-map (kbd "M-p") #'minuet-previous-suggestion)
  (define-key minuet-active-mode-map (kbd "C-g") #'minuet-dismiss-suggestion))

;;;; gptel ------------------------------------------------------------------

(declare-function gptel-make-ollama "gptel-ollama")
(declare-function gptel-make-anthropic "gptel-anthropic")
(declare-function gptel-make-gemini "gptel-gemini")
(declare-function ide-run-with-timeout "00-core")
(declare-function gptel-request "gptel")
(declare-function gptel-abort "gptel-request")
(declare-function gptel-add-file "gptel-context")
(declare-function gptel "gptel")
(declare-function gptel-mode "gptel")
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
  :config (ide-ai--apply-startup-model))

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
            :make ide-ai--make-ollama :models-fn ide-ai--ollama-models)
    (claude :name "Claude (Anthropic)" :backend-name "Claude"
            :make ide-ai--make-claude :env "ANTHROPIC_API_KEY" :host "api.anthropic.com"
            :models (claude-fable-5-1 claude-opus-5-5 claude-sonnet-5-5 claude-haiku-5-5))
    (gemini :name "Gemini (Google)" :backend-name "Gemini"
            :make ide-ai--make-gemini :env "GEMINI_API_KEY" :host "generativelanguage.googleapis.com"
            :models (gemini-2.5-pro gemini-2.5-flash)))
  "Providers offered by `ide-ai-set-model': (ID . PLIST).
PLIST keys: :name (shown), :backend-name (the gptel backend's name), :make
(function (MODELS KEY-FUNCTION) returning a gptel backend), :models or
:models-fn (suggested models; any other model id can still be typed), and for
hosted services :env (API key variable) and :host (auth-source host).
Add your own entry in user/ to offer another endpoint.")

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

(defun ide-ai--ready-p (provider)
  "Non-nil when PROVIDER can be used right now."
  (if (ide-ai--pget provider :env) (and (ide-ai--api-key provider) t)
    (car (ide-ai--ollama-probe))))

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

(defvar ide-ai--backends nil "Alist (PROVIDER-ID . gptel backend) created so far.")

(defun ide-ai--backend (provider models)
  "Return PROVIDER's gptel backend, creating it if needed, offering MODELS."
  (let* ((id (car provider)) (existing (alist-get id ide-ai--backends)))
    (if existing
        (progn (setf (cl-struct-slot-value 'gptel-backend 'models existing)
                     (delete-dups (append models (cl-struct-slot-value 'gptel-backend 'models existing))))
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

(defun ide-ai--current-provider ()
  "The provider whose backend is current, or nil."
  (when (and (boundp 'gptel-backend) gptel-backend)
    (cl-find (gptel-backend-name gptel-backend) ide-ai-providers
             :key (lambda (p) (ide-ai--pget p :backend-name)) :test #'equal)))

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
      (ignore-errors (ide-ai--backend p (and (null (ide-ai--pget p :env)) (list (intern ide-ai-ollama-model)))))))
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
    "\\.\\(tfvars\\|auto\\.tfvars\\)\\(\\.json\\)?\\'" "\\.tfstate\\(\\.[^/]*\\)?\\'"
    "\\.\\(pem\\|key\\|p12\\|pfx\\|jks\\|kdbx\\|gpg\\)\\'"
    "/id_\\(rsa\\|dsa\\|ecdsa\\|ed25519\\)\\'"
    "\\(\\`\\|/\\)[._]?\\(netrc\\|authinfo\\|npmrc\\|pypirc\\|git-credentials\\|pgpass\\)\\(\\.gpg\\)?\\'"
    "/\\.\\(ssh\\|aws\\|azure\\|gnupg\\|kube\\)/" "/kubeconfig\\'"
    "/[^/]*\\(secret\\|credential\\|password\\|token\\)s?[^/]*\\.\\(json\\|ya?ml\\|env\\|txt\\|ini\\|conf\\|cfg\\|toml\\|properties\\|xml\\|csv\\)\\'"
    "/\\(secrets?\\|credentials?\\)\\'")
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
  "Non-nil when BUFFER (default current) is marked private or visits a private file."
  (with-current-buffer (or buffer (current-buffer))
    (or ide-ai-private-buffer (ide-ai-private-file-p buffer-file-name))))

(defun ide-ai--local-host-p ()
  (string-match-p "\\`https?://\\(localhost\\|127\\.[0-9.]+\\|\\[::1\\]\\)\\([:/]\\|\\'\\)" ide-ai-ollama-host))

(defun ide-ai--provider-hosted-p (provider)
  "Non-nil when PROVIDER runs on someone else's machine.  Unknown providers count as hosted."
  (cond ((null provider) t)
        ((plist-member (cdr provider) :hosted) (plist-get (cdr provider) :hosted))
        ((ide-ai--pget provider :env) t)
        ((eq (car provider) 'ollama) (not (ide-ai--local-host-p)))
        (t t)))

(defun ide-ai-chat-hosted-p ()
  "Non-nil when chat and agent requests currently go to a hosted service."
  (ide-ai--provider-hosted-p (ide-ai--current-provider)))

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

(defun ide-ai--maybe-enable-minuet ()
  "Turn on inline suggestions, except in private buffers while a hosted model is used."
  (unless (and (ide-ai-fim-hosted-p) (ide-ai-private-buffer-p))
    (minuet-auto-suggestion-mode 1)))

(defun ide-ai--disable-minuet-in-private-buffers ()
  "After switching to a hosted completion backend, stop suggestions in private buffers."
  (when (ide-ai-fim-hosted-p)
    (dolist (b (buffer-list))
      (with-current-buffer b
        (when (and (bound-and-true-p minuet-auto-suggestion-mode) (ide-ai-private-buffer-p))
          (minuet-auto-suggestion-mode -1))))))

;;;###autoload
(defun ide-ai-mark-private (&optional unmark)
  "Mark this buffer private: it is never sent to a hosted AI model.  With UNMARK, undo that."
  (interactive "P")
  (setq ide-ai-private-buffer (not unmark))
  (when (and (not unmark) (ide-ai-fim-hosted-p) (bound-and-true-p minuet-auto-suggestion-mode))
    (minuet-auto-suggestion-mode -1))
  (message "%s is %s" (buffer-name) (if unmark "no longer private" "private: hosted models will not see it")))

(defun ide-ai--guard-gptel-context (&rest _)
  "Advice for gptel's own context commands: keep private data away from hosted models."
  (ide-ai--guard-send nil))

(defun ide-ai--guard-gptel-file (file &rest _)
  (when (and (ide-ai-chat-hosted-p) (ide-ai-private-file-p file))
    (user-error "%s looks private; not adding it to a hosted model's context" (file-name-nondirectory file))))

(with-eval-after-load 'gptel
  (define-key gptel-mode-map (kbd "C-c C-k") #'gptel-abort)
  (advice-add 'gptel-add :before #'ide-ai--guard-gptel-context)
  (advice-add 'gptel-add-file :before #'ide-ai--guard-gptel-file))

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
  (ide-ai--refresh-skill-dirs)
  (advice-add 'gptel-agent :before #'ide-ai--refresh-skill-dirs))

(defun ide-ai-agent ()
  "Start a `gptel-agent' session for the current project.
The agent can read and edit files, run shell commands and Elisp, search the
web and use Agent Skills; everything except reads and web access asks first."
  (interactive)
  (call-interactively #'gptel-agent))

(defun ide-ai-chat ()
  "Open a gptel chat buffer."
  (interactive) (call-interactively #'gptel))

(provide '13-ai)
;;; 13-ai.el ends here
