;;; 10-keybindings.el --- IDE muscle-memory keymap -*- lexical-binding: t; -*-

;;; Commentary:
;; Tool-window toggles on C-c w <digit>, editor actions under C-c l and M-s.
;; Nothing here shadows a standard Emacs editing key: only C-c <letter> (reserved
;; for users), M-s (the search prefix, where `consult' puts its commands), C-=,
;; and function keys are used.  Everything lives in `ide-keys-mode-map' so it can
;; be turned off as a unit.

;;; Code:

(require 'transient)

(declare-function ide-complete-snippet "04-corfu")

;;;; Commands ----------------------------------------------------------------

(defun ide-toggle-terminal ()
  "Toggle the terminal drawer."
  (interactive)
  (let ((win (seq-find (lambda (w) (string-prefix-p "*ghostel" (buffer-name (window-buffer w)))) (window-list))))
    (cond (win (delete-window win))
          ((fboundp 'ghostel) (ide-project-terminal))
          (t (eshell)))))

(defun ide-toggle-run-window ()
  "Toggle the Run / compilation drawer."
  (interactive)
  (let ((win (get-buffer-window "*compilation*")))
    (cond (win (delete-window win))
          ((get-buffer "*compilation*") (display-buffer "*compilation*"))
          (t (popper-toggle)))))

(defun ide-toggle-problems ()
  "Toggle the Problems (diagnostics) drawer."
  (interactive)
  (let ((win (get-buffer-window (format "*Flymake diagnostics for `%s'*" (buffer-name)))))
    (if win (delete-window win) (flymake-show-buffer-diagnostics))))

(defun ide-search-everywhere ()
  "Search files in the project (or buffers when outside one)."
  (interactive)
  (cond ((and (project-current) (executable-find "fd")) (consult-fd))
        ((project-current) (consult-find))
        (t (consult-buffer))))

(defun ide-smart-expand ()
  "Expand the selection structurally."
  (interactive)
  (er/expand-region 1))

(defun ide-reformat-buffer ()
  "Reformat the buffer with the LSP server or apheleia."
  (interactive)
  (cond ((and (bound-and-true-p eglot--managed-mode)
              (eglot-server-capable :documentFormattingProvider))
         (eglot-format-buffer))
        ((fboundp 'apheleia-format-buffer) (call-interactively #'apheleia-format-buffer))
        (t (indent-region (point-min) (point-max)))))

(defvar ide-run-command-functions nil
  "Abnormal hook: each function returns the shell command that runs the current
buffer, or nil.  Language modules add theirs; the first non-nil answer wins.")

(defvar ide-debug-functions nil
  "Abnormal hook: a function starts a debug session for the current buffer and
returns non-nil, or returns nil to leave it to the next one.")

(defun ide-run-current ()
  "Run the current file/target in the bottom drawer."
  (interactive)
  (let* ((file (and buffer-file-name (shell-quote-argument buffer-file-name)))
         (default-directory (if-let* ((p (project-current))) (project-root p) default-directory))
         (cmd (or
               (run-hook-with-args-until-success 'ide-run-command-functions)
               (cond
               ((derived-mode-p 'python-base-mode)
                (if (string-match-p "test_\\|_test\\.py" (or buffer-file-name ""))
                    (format "python -m pytest %s" file) (format "python %s" file)))
               ((derived-mode-p 'go-ts-mode)
                (if (string-suffix-p "_test.go" (or buffer-file-name "")) "go test ./..." "go run ."))
               ((derived-mode-p 'bash-ts-mode 'sh-mode) (format "bash %s" file))
               ((derived-mode-p 'terraform-mode) "terraform validate")))))
    (compile (or cmd (read-shell-command "Run: " compile-command)))))

(defun ide-debug-current ()
  "Start a debug session."
  (interactive)
  (require 'dap-mode)
  (unless (run-hook-with-args-until-success 'ide-debug-functions)
    (call-interactively #'dap-debug)))

(defun ide-escape ()
  "IDE-style Escape: quit prompts, dismiss popups, focus the editor."
  (interactive)
  (cond ((minibuffer-window-active-p (selected-window)) (abort-recursive-edit))
        ((region-active-p) (deactivate-mark))
        (t (ide-focus-code-window))))

;;;###autoload (autoload 'ide-services-dispatch "10-keybindings" nil t)
(transient-define-prefix ide-services-dispatch ()
  "Services & DevOps tool window."
  [["Containers" ("d" "Docker" docker)
    ("s" "Shell in container" ide-docker-shell) ("l" "Container logs" ide-docker-logs)]
   ["Cluster" ("k" "Kubernetes (kubel)" kubel) ("p" "Pod logs" ide-kubectl-logs)]
   ["Cloud" ("a" "Azure" azure-dispatch) ("o" "Azure DevOps" azdo-dispatch)
    ("i" "IaC (terraform/packer)" iac-dispatch)]
   ["Tools (mise)" ("m" "Install / update tools" ide-mise-setup)
    ("M" "Tool doctor" ide-mise-doctor)]
   ["Extend" ("A" "Add a package" ide-user-add-package)
    ("E" "Edit user config" ide-user-open)
    ("R" "Reload user config" ide-user-reload)]
   ["Help" ("h" "Open documentation" ide-docs-open)
    ("H" "Update documentation" ide-docs-update)]])

;;;; Keymap ------------------------------------------------------------------

(defvar ide-keys-mode-map
  (let ((m (make-sparse-keymap)))
    ;; Tool windows
    (define-key m (kbd "C-c w 1") #'ide-toggle-project-tree)
    (define-key m (kbd "C-c w 4") #'ide-toggle-run-window)
    (define-key m (kbd "C-c w 5") #'ide-toggle-debug-window)
    (define-key m (kbd "C-c w 6") #'ide-toggle-problems)
    (define-key m (kbd "C-c w 7") #'ide-toggle-structure)
    (define-key m (kbd "C-c w 8") #'ide-services-dispatch)
    (define-key m (kbd "C-c w 9") #'magit-status)
    (define-key m (kbd "C-c w 0") #'ide-toggle-terminal)
    (define-key m (kbd "M-<f12>") #'ide-toggle-terminal)
    (define-key m (kbd "C-c w e") #'ide-escape)
    (define-key m (kbd "C-`") #'popper-cycle)
    (define-key m (kbd "C-c w t") #'popper-toggle-type)
    ;; Editor.  Go to definition / usages are the standard M-. and M-?.
    (define-key m (kbd "<f12>") #'xref-find-definitions)
    (define-key m (kbd "C-c l f") #'ide-reformat-buffer)
    (define-key m (kbd "C-c l h") #'eldoc-box-help-at-point)
    (define-key m (kbd "C-=") #'ide-smart-expand)
    (define-key m (kbd "S-<f10>") #'ide-run-current)
    (define-key m (kbd "S-<f9>") #'ide-debug-current)
    (define-key m (kbd "M-s f") #'ide-search-everywhere)
    (define-key m (kbd "M-s l") #'consult-line)
    (define-key m (kbd "M-s b") #'consult-buffer)
    (define-key m (kbd "M-s s") #'ide-complete-snippet)
    (define-key m (kbd "C-S-f") #'consult-ripgrep)
    ;; DevOps / AI / Org prefixes
    (define-key m (kbd "C-c d d") #'docker)
    (define-key m (kbd "C-c d k") #'kubel)
    (define-key m (kbd "C-c d a") #'azure-dispatch)
    (define-key m (kbd "C-c d o") #'azdo-dispatch)
    (define-key m (kbd "C-c d i") #'iac-dispatch)
    (define-key m (kbd "C-c a i") #'gptel)
    (define-key m (kbd "C-c a m") #'gptel-menu)
    (define-key m (kbd "C-c a M") #'ide-ai-set-model)
    (define-key m (kbd "C-c a r") #'ide-ai-region-action)
    (define-key m (kbd "C-c a a") #'ide-ai-agent)
    (define-key m (kbd "C-c a c") #'ide-ai-add-project-context)
    (define-key m (kbd "C-c o r") #'org-roam-node-find)
    (define-key m (kbd "C-c o i") #'org-roam-node-insert)
    (define-key m (kbd "C-c o b") #'org-roam-buffer-toggle)
    m)
  "Keymap of `ide-keys-mode'.")

;; Code actions live on M-RET in programming modes only, so Org, Markdown and
;; comint keep their own M-RET.
(define-key prog-mode-map (kbd "M-RET") #'eglot-code-actions)

(defgroup ide-keys nil "IDE-style keybindings." :group 'convenience)

(define-minor-mode ide-keys-mode
  "IDE-style global keybindings."
  :group 'ide-keys
  :global t :keymap ide-keys-mode-map :lighter nil)
(ide-keys-mode 1)

(with-eval-after-load 'popper
  (define-key popper-mode-map (kbd "C-`") nil))

(provide '10-keybindings)
;;; 10-keybindings.el ends here
