;;; ide-align.el --- Smart indentation, auto-alignment and delimiter splitting -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides IDE-grade auto-alignment, smart delimiter splitting on Return,
;; intelligent YAML/Ansible indentation & electric list alignment, Ansible module
;; post-completion newline-and-indent, and snippet exit point preservation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function yas--field-snippet "yasnippet")
(declare-function yas--field-number "yasnippet")
(declare-function yas--snippet-fields "yasnippet")
(declare-function yas-exit-all-snippets "yasnippet")
(declare-function yas-next-field "yasnippet")
(declare-function yas-exit-snippet "yasnippet")
(declare-function corfu-insert "corfu")
(declare-function corfu-complete "corfu")
(declare-function eglot-completion-at-point "eglot")
(declare-function markdown-enter-key "markdown-mode")
(declare-function org-return "org")
(declare-function ide--ansible-file-p "ansible-intelligence")

(defvar yas--active-field-overlay)
(defvar yas-keymap)
(defvar yas-after-exit-snippet-hook)
(defvar ansible-mode)
(defvar ansible-intelligence-mode)
(defvar yaml-indent-offset)
(defvar terraform-indent-level)
(defvar hcl-indent-level)

(defgroup ide-align nil
  "Smart alignment and indentation."
  :group 'convenience)

(defcustom ide-align-yaml-indent-offset 2
  "Number of spaces per indentation level in YAML."
  :type 'integer
  :group 'ide-align)

(defcustom ide-align-terraform-indent-offset 2
  "Number of spaces per indentation level in Terraform / HCL."
  :type 'integer
  :group 'ide-align)

;;;; Delimiter & Return Intelligence -----------------------------------------

(defun ide-align--between-pair-p ()
  "Return non-nil if point is between matching delimiters: {}, [] or ()."
  (let ((before (char-before))
        (after (char-after)))
    (or (and (eq before ?{) (eq after ?}))
        (and (eq before ?\[) (eq after ?\]))
        (and (eq before ?\() (eq after ?\)))
        ;; Also allow whitespace between delimiters
        (save-excursion
          (let ((open-char (progn (skip-chars-backward " \t") (char-before))))
            (and (memq open-char '(?{ ?\[ ?\())
                 (progn
                   (skip-chars-forward " \t")
                   (let ((close-char (char-after)))
                     (or (and (eq open-char ?{) (eq close-char ?}))
                         (and (eq open-char ?\[) (eq close-char ?\]))
                         (and (eq open-char ?\() (eq close-char ?\))))))))))))

(defun ide-align--split-delimiters ()
  "Split matching delimiters onto separate lines with proper indentation."
  (let ((delims-start (save-excursion (skip-chars-backward " \t") (point)))
        (delims-end (save-excursion (skip-chars-forward " \t") (point))))
    (delete-region delims-start delims-end))
  (let* ((orig-indent (current-indentation))
         (mode-offset (ide-align--indent-offset-for-mode))
         (indent-tabs-mode nil))
    (newline)
    (indent-line-to (+ orig-indent mode-offset))
    (save-excursion
      (newline)
      (indent-line-to orig-indent))))

(defconst ide-align--offsets
  '(((yaml-ts-mode yaml-mode) yaml-indent-offset 2)
    ((terraform-mode hcl-mode) terraform-indent-level 2)
    ((python-ts-mode python-mode) python-indent-offset 4)
    ((go-ts-mode go-mode) go-ts-mode-indent-offset 4)
    ((rust-ts-mode rust-mode) rust-ts-mode-indent-offset 4)
    ((typescript-ts-mode tsx-ts-mode) typescript-ts-mode-indent-offset 2)
    ((js-ts-mode js-mode) js-indent-level 2)
    ((json-ts-mode json-mode) json-ts-mode-indent-offset 2)
    ((lua-ts-mode lua-mode) lua-ts-mode-indent-offset 2)
    ((bash-ts-mode sh-mode) sh-basic-offset 2)
    ((dockerfile-ts-mode) nil 2))
  "Alist mapping major modes to (VAR FALLBACK-OFFSET).")

(defun ide-align--indent-offset-for-mode ()
  "Return the indent offset for the current major mode."
  (or (cl-some (pcase-lambda (`(,modes ,var ,fallback))
                 (when (apply #'derived-mode-p modes)
                   (or (and var (boundp var) (symbol-value var)) fallback)))
               ide-align--offsets)
      (bound-and-true-p standard-indent)
      2))

;;;; YAML / Ansible Smart Indentation ---------------------------------------

(defconst ide-align-yaml-bare-key-re
  "^[ \t]*\\(?:- +\\)?\\(?:[a-zA-Z0-9_.-]+\\|\"[^\"]+\"\\|'[^']+'\\):[ \t]*$"
  "Regexp matching a YAML line ending in a mapping key without scalar value.")

(defun ide-align-yaml--line-ends-with-key-p ()
  "Return non-nil if current line ends with a mapping key colon."
  (save-excursion
    (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (string-match-p ide-align-yaml-bare-key-re line))))

(defun ide-align-yaml-compute-indentation ()
  "Calculate sensible maximum indentation for current line in YAML."
  (save-excursion
    (beginning-of-line)
    (if (looking-at-p "^---") 0
      (forward-line -1)
      ;; Skip blank lines
      (while (and (looking-at-p "^[ \t]*$") (> (point) (point-min)))
        (forward-line -1))
      (let* ((prev-indent (current-indentation))
             (prev-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (offset (or (bound-and-true-p yaml-indent-offset) ide-align-yaml-indent-offset 2)))
        (cond
         ;; Previous line was document start
         ((string-prefix-p "---" prev-line) 0)
         ;; Previous line was a bare mapping key: "foo:" or "- foo:"
         ((string-match-p ide-align-yaml-bare-key-re prev-line)
          (+ prev-indent offset))
         ;; Previous line was a list item with key-value: "- name: foo"
         ((string-match-p "^[ \t]*- [a-zA-Z0-9_.-]+:[ \t]+[^ \t]" prev-line)
          (+ prev-indent offset))
         ;; Previous line was a list scalar: "- foo"
         ((string-match-p "^[ \t]*- " prev-line)
          (+ prev-indent offset))
         ;; Default: preserve indentation of previous line
         (t prev-indent))))))

(defun ide-align-yaml-indent-line ()
  "Indent current line smartly in YAML, cycling on repeated Tab."
  (interactive "*")
  (let ((indent-tabs-mode nil)
        (ci (current-indentation))
        (target (ide-align-yaml-compute-indentation))
        (offset (or (bound-and-true-p yaml-indent-offset) ide-align-yaml-indent-offset 2)))
    (save-excursion
      (if (and (memq last-command '(ide-align-yaml-indent-line indent-for-tab-command))
               (/= ci 0))
          ;; Cycle back by offset spaces
          (let ((next (- ci offset)))
            (indent-line-to (if (< next 0) target next)))
        (indent-line-to target)))
    (when (< (current-column) (current-indentation))
      (back-to-indentation))))

(defun ide-align-yaml--prev-nonblank-line-info ()
  "Return (LINE . INDENT) of the previous non-blank, non-comment line, or nil."
  (save-excursion
    (beginning-of-line)
    (let (info)
      (while (and (not info) (> (point) (point-min)))
        (forward-line -1)
        (unless (looking-at-p "^[ \t]*\\(?:#.*\\)?$")
          (setq info (cons (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position))
                           (current-indentation)))))
      info)))

(defun ide-align--enclosing-dash-col (cmp-fn target-col)
  "Find column of nearest preceding list dash satisfying (CMP-FN col TARGET-COL)."
  (save-excursion
    (let (found)
      (while (and (not found) (not (bobp)))
        (forward-line -1)
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (if (string-prefix-p "---" line)
              (goto-char (point-min))
            (when (string-match "^\\([ \t]*\\)- " line)
              (let ((col (length (match-string 1 line))))
                (when (funcall cmp-fn col target-col)
                  (setq found col)))))))
      found)))

(defun ide-align-yaml-electric-dash ()
  "Insert `-' and align to the enclosing list item if at line beginning."
  (interactive "*")
  (let ((indent-tabs-mode nil)
        (line-prefix (buffer-substring-no-properties (line-beginning-position) (point))))
    (cond
     ;; Repeated press when line already has "- "
     ((and (eq last-command 'ide-align-yaml-electric-dash)
           (string-match-p "^[ \t]*- \\'" line-prefix))
      (let ((outer-col (ide-align--enclosing-dash-col #'< (current-indentation))))
        (delete-region (line-beginning-position) (point))
        (if outer-col
            (progn
              (indent-to outer-col)
              (insert "- "))
          (insert "---"))))
     ;; First press when only whitespace on line before point
     ((string-match-p "^[ \t]*\\'" line-prefix)
      (let* ((prev-info (ide-align-yaml--prev-nonblank-line-info))
             (prev-line (car prev-info))
             (prev-indent (cdr prev-info))
             (offset (or (bound-and-true-p yaml-indent-offset) ide-align-yaml-indent-offset 2))
             (col (current-column)))
        (delete-horizontal-space)
        (cond
         ;; Case 1: Preceding line was a bare mapping key (e.g. "tasks:", "block:", "ports:").
         ;; Current line starts a new list under that key, so indent below it.
         ((and prev-line
               (string-match-p ide-align-yaml-bare-key-re prev-line))
          (let ((target-indent (if (> col prev-indent) col (+ prev-indent offset))))
            (indent-to target-indent)
            (insert "- ")))
         ;; Case 2: Preceding list item exists at or above current column.
         (t
          (let ((list-col (ide-align--enclosing-dash-col #'<= col)))
            (indent-to (or list-col (if (> col 0) col 0)))
            (insert "- "))))))
     ;; Normal press inside text or words
     (t
      (insert "-")))))

(defun ide-align-yaml-electric-backspace (arg)
  "Delete characters or back-dent by `ide-align-yaml-indent-offset' spaces."
  (interactive "*p")
  (if (and (looking-back "^[ \t]+" (line-beginning-position)) (not (bolp)))
      (let* ((indent-tabs-mode nil)
             (offset (or (bound-and-true-p yaml-indent-offset) ide-align-yaml-indent-offset 2))
             (ci (current-column))
             (target (* (/ (- ci 1) offset) offset)))
        (delete-region (line-beginning-position) (point))
        (indent-to (max 0 target)))
    (backward-delete-char-untabify arg)))

;;;; Post-Completion Auto-Alignment (Eglot LSP, Corfu, Cape, Schemas) --------

(defconst ide-align-ansible-modules-re
  (concat
   "\\`"
   "\\(?:"
   ;; FQCN format: ansible.builtin.foo, community.general.bar, etc.
   "ansible\\.[a-zA-Z0-9_.]+\\|"
   ;; Common classic / short module names
   "apt\\|apt_key\\|apt_repository\\|assemble\\|assert\\|async_status\\|"
   "command\\|copy\\|cron\\|debconf\\|debug\\|dnf\\|dpkg_selections\\|"
   "fail\\|fetch\\|file\\|find\\|gather_facts\\|get_url\\|getent\\|git\\|"
   "group\\|hostname\\|import_playbook\\|import_role\\|import_tasks\\|"
   "include_role\\|include_tasks\\|include_vars\\|iptables\\|known_hosts\\|"
   "lineinfile\\|meta\\|package\\|pause\\|ping\\|pip\\|raw\\|reboot\\|"
   "replace\\|service\\|set_fact\\|set_stats\\|setup\\|shell\\|slurp\\|"
   "stat\\|subversion\\|systemd\\|systemd_service\\|tempfile\\|template\\|"
   "unarchive\\|uri\\|user\\|wait_for\\|wait_for_connection\\|yum\\|yum_repository"
   "\\)"
   "\\'")
  "Regexp matching Ansible module names.")

(defconst ide-align-yaml-block-keys-re
  (concat
   "\\`"
   "\\(?:"
   ;; Ansible block keys
   "tasks\\|vars\\|environment\\|handlers\\|block\\|rescue\\|always\\|roles\\|"
   "pre_tasks\\|post_tasks\\|when\\|with_items\\|loop\\|"
   ;; Kubernetes / Compose / CI block keys
   "spec\\|metadata\\|containers\\|services\\|volumes\\|volumeMounts\\|"
   "ports\\|env\\|steps\\|jobs\\|on\\|defaults\\|labels\\|annotations"
   "\\)"
   ":?[ \t]*\\'")
  "Regexp matching common YAML schema keys that expect child blocks.")

(defun ide-align--ansible-buffer-p ()
  "Return non-nil if current buffer is an Ansible buffer or symbol is Ansible."
  (or (and (bound-and-true-p ansible-mode) t)
      (and (bound-and-true-p ansible-intelligence-mode) t)
      (and (fboundp 'ide--ansible-file-p) (ide--ansible-file-p))
      (and buffer-file-name
           (string-match-p "\\(playbook\\|roles/\\|tasks/\\|handlers/\\|vars/\\|\\.ansible\\)" buffer-file-name))
      (save-excursion
        (skip-chars-backward " \t")
        (let* ((end (point))
               (beg (save-excursion (skip-chars-backward "a-zA-Z0-9_.") (point)))
               (sym (buffer-substring-no-properties beg end)))
          (string-match-p "\\`\\(ansible\\|community\\)\\." sym)))))

(defun ide-align--handle-yaml-key (re chars)
  "If symbol before point matches RE, ensure `:` without inserting a newline."
  (when (derived-mode-p 'yaml-ts-mode 'yaml-mode)
    (let ((indent-tabs-mode nil)
          target-pos)
      (save-excursion
        (skip-chars-backward " \t")
        (let* ((end (point))
               (beg (save-excursion (skip-chars-backward chars) (point)))
               (sym (buffer-substring-no-properties beg end)))
          (when (string-match-p re sym)
            (let ((prefix (buffer-substring-no-properties (line-beginning-position) beg)))
              (when (string-match-p "^[ \t]*\\(?:- +\\)?$" prefix)
                (goto-char end)
                (unless (or (looking-at-p "[ \t]*:")
                            (looking-back ":" (max (point-min) (1- (point)))))
                  (insert ":"))
                (skip-chars-forward " \t:")
                (setq target-pos (point)))))))
      (when target-pos
        (goto-char target-pos)
        t))))

(defun ide-align--handle-ansible-module-completion ()
  "Ensure completed Ansible module is followed by `:` without extra newline."
  (when (ide-align--ansible-buffer-p)
    (ide-align--handle-yaml-key ide-align-ansible-modules-re "a-zA-Z0-9_.")))

(defun ide-align--handle-yaml-mapping-completion ()
  "After completing a YAML block mapping key, ensure `:` without extra newline."
  (ide-align--handle-yaml-key ide-align-yaml-block-keys-re "a-zA-Z0-9_.-:"))

(defun ide-align--handle-general-block-completion ()
  "Expand empty brace blocks into multi-line blocks with indented point."
  (when (derived-mode-p 'prog-mode 'terraform-mode 'hcl-mode)
    (let ((indent-tabs-mode nil)
          target-pos)
      (save-excursion
        (skip-chars-backward " \t\n")
        (cond
         ((or (eq (char-before) ?})
              (eq (char-after) ?}))
          (when (eq (char-after) ?}) (forward-char 1))
          (let ((close-pos (point)))
            (backward-char 1)
            (condition-case nil
                (let ((open-pos (scan-lists (point) -1 1)))
                  (when (and open-pos (eq (char-after open-pos) ?{))
                    (let ((between (buffer-substring-no-properties (1+ open-pos) (1- close-pos))))
                      (cond
                       ((string-match-p "\\`[ \t\n]*\\'" between)
                        (delete-region (1+ open-pos) (1- close-pos))
                        (goto-char (1+ open-pos))
                        (let* ((base-indent (save-excursion (goto-char open-pos) (current-indentation)))
                               (offset (ide-align--indent-offset-for-mode)))
                          (insert "\n")
                          (indent-line-to (+ base-indent offset))
                          (setq target-pos (point))
                          (insert "\n")
                          (indent-line-to base-indent)))
                       (t
                        (goto-char (1+ open-pos))
                        (forward-line 1)
                        (if (looking-at-p "^[ \t]*$")
                            (indent-line-to (ide-align--indent-offset-for-mode))
                          (back-to-indentation))
                        (setq target-pos (point)))))))
              (error nil))))
         ((or (and (eq (char-before) ?\]) (eq (char-before (1- (point))) ?\[))
              (and (eq (char-before) ?\)) (eq (char-before (1- (point))) ?\()))
          (backward-char 1)
          (setq target-pos (point)))))
      (when target-pos
        (goto-char target-pos)
        t))))

(defalias 'ide-align--handle-terraform-block-completion #'ide-align--handle-general-block-completion)

(defun ide-align--handle-python-colon-completion ()
  "After completing a Python block header ending in `:`, newline and indent."
  (when (derived-mode-p 'python-ts-mode 'python-mode)
    (let ((indent-tabs-mode nil)
          target-pos)
      (save-excursion
        (skip-chars-backward " \t")
        (when (eq (char-before) ?:)
          (let ((line (buffer-substring-no-properties (line-beginning-position) (point))))
            (when (string-match-p "^[ \t]*\\(?:def\\|class\\|if\\|elif\\|else\\|for\\|while\\|try\\|except\\|finally\\|with\\|async\\)[ \t]" line)
              (when (eolp)
                (let ((child-indent (+ (current-indentation) (ide-align--indent-offset-for-mode))))
                  (insert "\n")
                  (indent-line-to child-indent)
                  (setq target-pos (point))))))))
      (when target-pos
        (goto-char target-pos)
        t))))

(defun ide-align--post-completion-alignment (&optional _proxy)
  "Perform context-aware auto-alignment after completion is accepted.
Covers Eglot LSP completions, Corfu, Cape, and schemas across all languages."
  (let ((indent-tabs-mode nil))
    (save-excursion
      (untabify (line-beginning-position) (line-end-position)))
    (cond
     ((derived-mode-p 'yaml-ts-mode 'yaml-mode)
      (or (ide-align--handle-ansible-module-completion)
          (ide-align--handle-yaml-mapping-completion)))
     ((derived-mode-p 'python-ts-mode 'python-mode)
      (ide-align--handle-python-colon-completion))
     ((derived-mode-p 'prog-mode 'terraform-mode 'hcl-mode)
      (ide-align--handle-general-block-completion)))))

(defun ide-align--eglot-capf-advice (orig-fun &rest args)
  "Wrap `eglot-completion-at-point' so its :exit-function triggers auto-alignment."
  (let ((res (apply orig-fun args)))
    (if (consp res)
        (let* ((plist (nthcdr 3 res))
               (orig-exit (plist-get plist :exit-function)))
          (plist-put plist :exit-function
                     (lambda (proxy status)
                       (when orig-exit
                         (let ((indent-tabs-mode nil))
                           (funcall orig-exit proxy status)))
                       (when (memq status '(finished exact))
                         (ide-align--post-completion-alignment proxy))))
          res)
      res)))

(defun ide-align--corfu-insert-advice (orig-fun &rest args)
  "Advise `corfu-insert' and `corfu-complete' to trigger post-completion auto-alignment."
  (let ((beg (line-beginning-position))
        (indent-tabs-mode nil))
    (let ((res (apply orig-fun args)))
      (save-excursion
        (untabify beg (line-end-position)))
      (ignore-errors (ide-align--post-completion-alignment))
      res)))

;;;; Yasnippet Auto-Alignment & Exit Point Preservation ----------------------

(defconst ide-align--special-modes
  '(special-mode dired-mode magit-mode comint-mode eshell-mode)
  "Modes where ide-align-mode should never enable or intercept Return.")

(defun ide-align--call-original-ret (&optional fallback)
  "Execute the underlying RET command with `ide-align-mode' disabled."
  (let ((ide-align-mode nil))
    (let ((cmd (key-binding (kbd "RET"))))
      (if (and cmd (not (memq cmd '(ide-align-smart-return ide-align-yas-ret))))
          (if (commandp cmd) (call-interactively cmd) (funcall cmd))
        (when fallback (funcall fallback))))))

(defun ide-align-yas-ret ()
  "Smart Return inside an active Yasnippet template.
If on a snippet field, advances to the next field or commits to `$0`
inside the block body rather than inserting a literal newline."
  (interactive)
  (if (and (bound-and-true-p ide-align-mode) (bound-and-true-p yas--active-field-overlay))
      (let* ((field (overlay-get yas--active-field-overlay 'yas--field))
             (snippet (and field (yas--field-snippet field))))
        (if (or (null snippet)
                (= (yas--field-number field) 0)
                (null (yas--snippet-fields snippet)))
            (yas-exit-all-snippets)
          (condition-case nil
              (yas-next-field)
            (error (yas-exit-all-snippets)))))
    (ide-align--call-original-ret #'ide-align-smart-return)))

(defun ide-align--fix-snippet-exit-point (&rest _)
  "Ensure point lands inside the block body when a snippet exits."
  (when (and (bound-and-true-p ide-align-mode)
             (derived-mode-p 'terraform-mode 'hcl-mode))
    (ide-align--handle-general-block-completion)))

;;;; Smart Return Command -----------------------------------------------------

;;;###autoload
(defun ide-align-smart-return ()
  "Smart IDE Return: splits paired delimiters, indents YAML blocks, or passes through."
  (interactive)
  (let ((indent-tabs-mode nil))
    (cond
     ;; 1. Minibuffer safeguard: pass through to the minibuffer's own RET command!
     ((minibufferp)
      (ide-align--call-original-ret #'exit-minibuffer))

     ;; 2. Read-only or special modes: pass through!
     ((or buffer-read-only
          (apply #'derived-mode-p ide-align--special-modes))
      (ide-align--call-original-ret))

     ;; 3. Active Yasnippet field: use smart snippet commit
     ((bound-and-true-p yas--active-field-overlay)
      (ide-align-yas-ret))

     ;; 4. Point is between matching delimiters: split delimiters onto 3 lines
     ((ide-align--between-pair-p)
      (ide-align--split-delimiters))

     ;; 5. YAML / Ansible: line ends with a bare mapping key colon
     ((and (derived-mode-p 'yaml-ts-mode 'yaml-mode)
           (ide-align-yaml--line-ends-with-key-p))
      (let ((child-indent (+ (current-indentation)
                             (ide-align--indent-offset-for-mode))))
        (newline)
        (indent-line-to child-indent)))

     ;; 6. YAML / Ansible: general smart newline
     ((derived-mode-p 'yaml-ts-mode 'yaml-mode)
      (let ((ci (current-indentation)))
        (newline)
        (indent-line-to ci)))

     ;; 7. Markdown: preserve markdown-enter-key for list continuation
     ((and (derived-mode-p 'markdown-mode) (fboundp 'markdown-enter-key))
      (call-interactively #'markdown-enter-key))

     ;; 8. Org mode: preserve org-return
     ((and (derived-mode-p 'org-mode) (fboundp 'org-return))
      (call-interactively #'org-return))

     ;; 9. Default for programming modes: pass through to mode-specific RET or newline-and-indent
     (t
      (ide-align--call-original-ret #'newline-and-indent)))))

;;;; Minor Modes & Hooks ------------------------------------------------------

(defvar ide-align-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ide-align-smart-return)
    (define-key map (kbd "<return>") #'ide-align-smart-return)
    map)
  "Keymap for `ide-align-mode'.")

(defvar ide-align-yaml-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "-") #'ide-align-yaml-electric-dash)
    (define-key map (kbd "DEL") #'ide-align-yaml-electric-backspace)
    (define-key map (kbd "<backspace>") #'ide-align-yaml-electric-backspace)
    (define-key map (kbd "TAB") #'ide-align-yaml-indent-line)
    map)
  "Keymap for YAML-specific auto-alignment.")

(define-minor-mode ide-align-yaml-mode
  "YAML auto-alignment minor mode for indentation and electric lists."
  :lighter nil
  :keymap ide-align-yaml-mode-map
  (if ide-align-yaml-mode
      (progn
        (setq-local indent-tabs-mode nil)
        (setq-local indent-line-function #'ide-align-yaml-indent-line)
        (setq-local yaml-indent-offset ide-align-yaml-indent-offset))
    (setq-local indent-line-function #'indent-relative)))

(defun ide-align--setup-yaml ()
  "Enable YAML smart indentation and electric keys."
  (setq-local indent-tabs-mode nil)
  (ide-align-yaml-mode 1))

(defun ide-align--setup-terraform ()
  "Setup Terraform buffer indentation and snippet alignment."
  (setq-local tab-width ide-align-terraform-indent-offset
              terraform-indent-level ide-align-terraform-indent-offset
              hcl-indent-level ide-align-terraform-indent-offset
              indent-tabs-mode nil))

;;;###autoload
(define-minor-mode ide-align-mode
  "Buffer-local minor mode providing IDE parity for auto-alignment and delimiter splitting."
  :lighter nil
  :keymap ide-align-mode-map
  (if ide-align-mode
      (progn
        (setq-local indent-tabs-mode nil)
        (when (derived-mode-p 'yaml-ts-mode 'yaml-mode)
          (ide-align--setup-yaml))
        (when (derived-mode-p 'terraform-mode 'hcl-mode)
          (ide-align--setup-terraform)))
    (when (derived-mode-p 'yaml-ts-mode 'yaml-mode)
      (ide-align-yaml-mode -1))))

(defun ide-align--turn-on ()
  "Turn on `ide-align-mode' in programming and config buffers.
Never turn on in minibuffers, Dired, Magit, or special modes."
  (unless (or (minibufferp)
              (apply #'derived-mode-p ide-align--special-modes))
    (when (derived-mode-p 'prog-mode 'conf-mode 'yaml-ts-mode 'yaml-mode 'terraform-mode 'hcl-mode)
      (ide-align-mode 1))))

(defun ide-align--yas-exit-snippet-advice (orig-fun &rest args)
  "Advise `yas-exit-snippet' to ensure point is properly placed inside blocks."
  (let ((res (apply orig-fun args)))
    (when (bound-and-true-p ide-align-mode)
      (ignore-errors (ide-align--fix-snippet-exit-point)))
    res))

;; Top-level integrations (loaded once, avoid re-registering on mode toggling)
(with-eval-after-load 'eglot
  (advice-add 'eglot-completion-at-point :around #'ide-align--eglot-capf-advice))

(with-eval-after-load 'corfu
  (advice-add 'corfu-insert :around #'ide-align--corfu-insert-advice)
  (advice-add 'corfu-complete :around #'ide-align--corfu-insert-advice))

(with-eval-after-load 'yasnippet
  (define-key yas-keymap (kbd "RET") #'ide-align-yas-ret)
  (define-key yas-keymap (kbd "<return>") #'ide-align-yas-ret)
  (advice-add 'yas-exit-snippet :around #'ide-align--yas-exit-snippet-advice))

;;;###autoload
(define-globalized-minor-mode global-ide-align-mode ide-align-mode ide-align--turn-on
  :group 'ide-align)

(defalias 'ide-align-global-mode #'global-ide-align-mode)

(provide 'ide-align)
;;; ide-align.el ends here
