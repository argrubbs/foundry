;;; ansible-intelligence-parser.el --- Bounded Ansible buffer parser -*- lexical-binding: t; -*-

;;; Code:

(require 'ansible-intelligence-model)
(require 'ansible-intelligence-jinja)
(require 'ansible-intelligence-yaml)
(require 'ansible-intelligence-bash)
(require 'subr-x)
(require 'treesit nil t)

(defconst ansible-intelligence--identifier-re "[[:alpha:]_][[:alnum:]_]*")

(defun ansible-intelligence-detect-document-kind (&optional file)
  "Classify FILE, defaulting to the current buffer file."
  (let ((path (or file buffer-file-name "")))
    (cond
     ((string-match-p "/roles/[^/]+/defaults/" path) 'role-default)
     ((string-match-p "/roles/[^/]+/vars/" path) 'role-var)
     ((string-match-p "/roles/[^/]+/tasks/" path) 'role-task)
     ((string-match-p "/\\(?:group_vars\\|host_vars\\)/" path) 'inventory-var)
     ((string-match-p "/\\(?:vars\\|variables\\)/" path) 'vars-file)
     (t 'unknown))))

(defun ansible-intelligence--tree ()
  "Return the YAML syntax tree when tree-sitter is available."
  (when (and (fboundp 'treesit-buffer-root-node)
             (seq-find (lambda (parser)
                         (eq (treesit-parser-language parser) 'yaml))
                       (treesit-parser-list)))
    (treesit-buffer-root-node 'yaml)))

(defun ansible-intelligence--line-indentation ()
  "Return indentation of the current line."
  (current-indentation))

(defun ansible-intelligence--mapping-value (key)
  "Return scalar value for KEY on the current line, or nil."
  (when (looking-at (concat "^[[:space:]]*" (regexp-quote key)
                            ":[[:space:]]*\\([^#\n]+\\)"))
    (string-trim (match-string-no-properties 1))))

(defun ansible-intelligence--inferred-field-metadata
    (documentation &optional fields)
  "Build metadata for an inferred field with DOCUMENTATION and FIELDS."
  (list :documentation documentation
        :annotation (if fields "  [inferred mapping]" "  [inferred field]")
        :fields fields))

(defun ansible-intelligence--inferred-variable-fields ()
  "Infer conservative nested mapping fields from the current YAML buffer.

Only block mappings whose keys are simple identifiers are included.  Scalars,
sequences, flow mappings, and other YAML constructs are intentionally left
opaque so completion never presents guesses as facts."
  (let (roots current-root stack)
    (cl-labels
        ((new-node (name)
           (list name nil nil))
         (node-name (node) (nth 0 node))
         (node-children (node) (nth 2 node))
         (set-node-children (node children)
           (setf (nth 2 node) children))
         (add-child (parent child)
           (set-node-children parent
                              (append (node-children parent) (list child))))
         (node-fields (node)
           (mapcar
            (lambda (child)
              (cons (node-name child)
                    (ansible-intelligence--inferred-field-metadata
                     "Inferred from a nested YAML mapping."
                     (node-fields child))))
            (node-children node))))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (cond
           ((looking-at "^[ \t]*-")
            ;; Sequence contents are deliberately opaque in this shape model.
            (setq stack nil))
           ((looking-at
             "^\\([ \t]*\\)\\([[:alnum:]_]+\\):[ \t]*\\([^#\n]*\\)")
            (let* ((indent (length (match-string-no-properties 1)))
                   (name (match-string-no-properties 2))
                   (value (string-trim (match-string-no-properties 3))))
              (if (= indent 0)
                  (let ((node (new-node name)))
                    (push node roots)
                    (setq current-root node
                          stack (if (string-empty-p value)
                                    (list (cons indent node))
                                  nil)))
                (when current-root
                  (while (and stack (<= indent (caar stack)))
                    (pop stack))
                  (let ((parent (cdar stack)))
                    (when parent
                      (let ((node (new-node name)))
                        (add-child parent node)
                        (when (string-empty-p value)
                          (push (cons indent node) stack)))))))))
           ((and (not (looking-at "^[[:space:]]*$"))
                 (looking-at "^[^ \t\n]"))
            (setq current-root nil stack nil)))
          (forward-line 1)))
      (mapcar (lambda (node)
                (cons (node-name node) (node-fields node)))
              (nreverse roots)))))

(defun ansible-intelligence--symbol (name kind start visibility-start source &optional fields)
  "Create a symbol NAME with its precise source and visibility positions."
  (ansible-intelligence-symbol-create
   :name name :kind kind
   :range (ansible-intelligence-range-create :start start :end (+ start (length name)))
   :visibility-start visibility-start :visibility-end (point-max)
   :source source :confidence 'static :fields fields :file buffer-file-name
   :line (line-number-at-pos start)))

(defun ansible-intelligence-parse-buffer (&optional forced-kind jinja-cache)
  "Extract bounded, source-ordered Ansible facts from the current buffer.

Tree-sitter supplies the YAML root when installed; line scanning is restricted
to semantic Ansible constructs so malformed YAML still has useful partial data.
FORCED-KIND classifies files reached through a typed project relationship.
JINJA-CACHE is a buffer-local cache of region-relative analyses."
  (let* ((kind (or forced-kind (ansible-intelligence-detect-document-kind)))
         (variable-file-p (memq kind '(role-default role-var inventory-var vars-file)))
  (inferred-fields (and variable-file-p
                        (ansible-intelligence--inferred-variable-fields)))
  (symbols nil) (links nil) (errors nil) (task-start nil)
         (tasks-indent nil) (vars-indent nil)
         (vars-child-indent nil) (vars-visibility-start nil)
         (set-fact-indent nil) (set-fact-child-indent nil)
         (loop-control-indent nil) (task-has-loop nil)
         (loop-definition nil) (task-local-symbols nil)
         (list-link-kind nil) (list-link-indent nil)
         (loop-var "item") (tree (ansible-intelligence--tree))
         (structural (ansible-intelligence-yaml-query-extract tree kind))
         (structural-query-p (plist-get structural :source))
         (outline (plist-get structural :outline))
         (shell-ranges
          (ansible-intelligence-bash-materialize-ranges
           (plist-get structural :shell-ranges)))
         (jinja (ansible-intelligence-jinja-analyze-buffer tree jinja-cache)))
    (setq links (copy-sequence (plist-get structural :links)))
    (cl-labels
        ((close-task
          (end)
          (when (and task-start task-has-loop loop-definition)
            (let ((symbol (ansible-intelligence--symbol
                           loop-var 'loop-var loop-definition task-start
                           "loop variable")))
              (setf (ansible-intelligence-symbol-visibility-end symbol) end)
              (push symbol symbols)))
          (dolist (symbol task-local-symbols)
            (setf (ansible-intelligence-symbol-visibility-end symbol) end))
          (setq task-has-loop nil loop-definition nil loop-var "item"
                task-local-symbols nil vars-indent nil vars-child-indent nil
                vars-visibility-start nil set-fact-indent nil
                set-fact-child-indent nil loop-control-indent nil)))
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line-start (point)) (indent (ansible-intelligence--line-indentation)))
          (when (and vars-indent (not (looking-at "^[[:space:]]*$"))
                     (<= indent vars-indent))
            (setq vars-indent nil vars-child-indent nil vars-visibility-start nil))
          (when (and set-fact-indent (not (looking-at "^[[:space:]]*$"))
                     (<= indent set-fact-indent))
            (setq set-fact-indent nil set-fact-child-indent nil))
          (when (and loop-control-indent (not (looking-at "^[[:space:]]*$"))
                     (<= indent loop-control-indent))
            (setq loop-control-indent nil))
          (when (and list-link-kind
                     (not (looking-at "^[[:space:]]*$"))
                     (<= indent list-link-indent)
                     (not (looking-at
                           "^[[:space:]]*\\(?:vars_files\\|roles\\|import_role\\|include_role\\|include_vars\\):")))
            (setq list-link-kind nil list-link-indent nil))
          (cond
           ((and variable-file-p (= indent 0)
                 (looking-at (concat "^\\(" ansible-intelligence--identifier-re "\\):")))
            (let ((name (match-string-no-properties 1)))
              (push (ansible-intelligence--symbol
                     name kind (match-beginning 1) (point-min)
                     (format "%s variable" (symbol-name kind))
                     (cdr (assoc name inferred-fields)))
                    symbols)))
           ((and (not structural-query-p)
                 (looking-at "^[[:space:]]*\\(vars_files\\|roles\\):[[:space:]]*$"))
            (setq list-link-kind
                  (if (string= (match-string-no-properties 1) "roles")
                      'import-role
                    'vars-files)
                  list-link-indent indent))
           ((and (not structural-query-p)
                 (looking-at
                  "^[[:space:]]*\\(?:-[[:space:]]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_role\\|include_role\\|include_vars\\):[[:space:]]*$"))
            (setq list-link-kind
                  (intern (replace-regexp-in-string
                           "_" "-" (match-string-no-properties 1)))
                  list-link-indent indent))
           ((and (not structural-query-p)
                 (looking-at
                  "^[[:space:]]*\\(?:-[[:space:]]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_tasks\\|include_tasks\\|vars_files\\|include_vars\\):[[:space:]]*\\([^#\n][^#\n]*\\)"))
            (let* ((key (match-string-no-properties 1))
                   (target (string-trim (match-string-no-properties 2) "[\"']" "[\"']"))
                   (link-kind (intern (replace-regexp-in-string "_" "-" key))))
              (unless (or (string-empty-p target)
                          (string-match-p "{{\\|{%" target))
                (push (ansible-intelligence-link-create
                       :kind link-kind :target target
                       :range (ansible-intelligence-range-create
                               :start (match-beginning 2) :end (match-end 2))
                       :confidence 'static)
                      links)))
            nil)
           ((and (not structural-query-p)
                 list-link-kind (> indent list-link-indent)
                 (looking-at "^[[:space:]]*\\(?:-[[:space:]]*\\|\\(?:name\\|role\\|file\\):[[:space:]]*\\)\\([^#\n]+\\)"))
            (let* ((raw (match-string-no-properties 1))
                   (target
                    (string-trim
                     (replace-regexp-in-string
                      "\\`\\(?:name\\|role\\|file\\):[[:space:]]*" "" raw)
                     "[\"']" "[\"']")))
              (unless (string-match-p "{{\\|{%" target)
                (push (ansible-intelligence-link-create
                       :kind list-link-kind :target target
                       :range (ansible-intelligence-range-create
                               :start (match-beginning 1) :end (match-end 1))
                       :confidence 'static)
                      links))))
           ((looking-at "^[[:space:]]*\\(?:tasks\\|handlers\\|block\\|rescue\\|always\\):[[:space:]]*$")
            (setq tasks-indent indent))
           ((and (looking-at
                  "^[[:space:]]*-[[:space:]]+\\([[:alnum:]_.]+\\):")
                 (not (and vars-indent (> indent vars-indent)))
                 (not (member (match-string-no-properties 1)
                              '("hosts" "roles" "vars_files")))
                 (or (eq kind 'role-task)
                     (null tasks-indent)
                     (> indent tasks-indent)))
            (let ((action (match-string-no-properties 1))
                  (action-column
                   (save-excursion
                     (goto-char (match-beginning 1))
                     (current-column))))
              (close-task line-start)
              (setq task-start line-start)
              (when (string= action "set_fact")
                (setq set-fact-indent action-column))))
           ((ansible-intelligence--mapping-value "register")
            (let ((name (match-string-no-properties 1))
                  (name-start (match-beginning 1)))
              (when (string-match-p (concat "\\`" ansible-intelligence--identifier-re "\\'") name)
                (push (ansible-intelligence--symbol
                       name 'register name-start
                       (line-end-position) "registered by this task"
                       ansible-intelligence-generic-result-fields)
                      symbols))))
           ((looking-at "^[[:space:]]*set_fact:[[:space:]]*$")
            (setq set-fact-indent indent))
           ((and set-fact-indent (> indent set-fact-indent)
                 (looking-at (concat "^[[:space:]]*\\(" ansible-intelligence--identifier-re "\\):")))
            (unless set-fact-child-indent
              (setq set-fact-child-indent indent))
            (when (= indent set-fact-child-indent)
              (let ((name (match-string-no-properties 1)))
                (push (ansible-intelligence--symbol
                       name 'set-fact (match-beginning 1)
                       (line-end-position) "created by set_fact")
                      symbols))))
           ((looking-at "^[[:space:]]*vars:[[:space:]]*$")
            (setq vars-indent indent vars-child-indent nil
                  vars-visibility-start (or task-start (point-min))))
           ((and loop-control-indent (> indent loop-control-indent)
                 (looking-at "^[[:space:]]*loop_var:[[:space:]]*\\([[:alnum:]_]+\\)"))
            (setq loop-var (match-string-no-properties 1)
                  loop-definition (match-beginning 1)))
           ((and vars-indent (> indent vars-indent)
                 (looking-at (concat "^[[:space:]]*\\(" ansible-intelligence--identifier-re "\\):")))
            (unless vars-child-indent
              (setq vars-child-indent indent))
            (when (= indent vars-child-indent)
              (let ((symbol
                     (ansible-intelligence--symbol
                      (match-string-no-properties 1)
                      (if task-start 'task-var 'play-var)
                      (match-beginning 1) vars-visibility-start
                      (if task-start "task-local variable" "play variable"))))
                (push symbol symbols)
                (when task-start (push symbol task-local-symbols)))))
           ((looking-at "^[[:space:]]*loop_control:[[:space:]]*$")
            (setq loop-control-indent indent))
           ((and task-start (looking-at "^[[:space:]]*\\(?:loop\\|with_[[:alnum:]_]+\\):"))
            (setq task-has-loop t loop-definition line-start)))
          (forward-line 1)))
      (close-task (point-max))))
    (when (and tree (eq (treesit-node-type tree) 'ERROR))
      (push "YAML syntax is incomplete; semantic results are partial." errors))
    (setq symbols
          (sort (append (nreverse symbols)
                        (ansible-intelligence-jinja-snapshot-definitions jinja))
                (lambda (left right)
                  (< (ansible-intelligence-range-start
                      (ansible-intelligence-symbol-range left))
                     (ansible-intelligence-range-start
                      (ansible-intelligence-symbol-range right))))))
    (ansible-intelligence-document-create
     :path buffer-file-name :kind kind :tick (buffer-chars-modified-tick)
     :parsed-at (float-time) :symbols symbols
     :references (ansible-intelligence-jinja-snapshot-references jinja)
     :links links
     :errors (nreverse (append (ansible-intelligence-jinja-snapshot-errors jinja)
                               errors))
     :tree tree :jinja jinja :outline outline :shell-ranges shell-ranges
     :source-identity (ansible-intelligence-source-identity-for-buffer))))

(provide 'ansible-intelligence-parser)

;;; ansible-intelligence-parser.el ends here
