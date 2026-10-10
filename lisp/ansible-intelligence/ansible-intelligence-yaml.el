;;; ansible-intelligence-yaml.el --- Verified YAML structural extraction -*- lexical-binding: t; -*-

;;; Commentary:
;; The installed YAML grammar is intentionally queried only through node
;; shapes verified against it.  The line scanner remains the recovery path for
;; absent or malformed trees; it is not used to second-guess successful query
;; captures.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ansible-intelligence-model)
(require 'treesit nil t)

(defconst ansible-intelligence-yaml--mapping-query
  '(((block_mapping_pair key: (_) @key value: (_) @value) @pair))
  "Verified mapping-pair query for `tree-sitter-yaml' v0.5.x.")

(defconst ansible-intelligence-yaml--scalar-types
  '("plain_scalar" "single_quote_scalar" "double_quote_scalar"
    "block_scalar" "literal_scalar" "folded_scalar")
  "YAML scalar nodes whose ranges can be mapped to source text.")

(defconst ansible-intelligence-yaml--link-keys
  '(("import_tasks" . import-tasks)
    ("include_tasks" . include-tasks)
    ("vars_files" . vars-files)
    ("include_vars" . include-vars)
    ("import_role" . import-role)
    ("include_role" . include-role)
    ("roles" . import-role))
  "Static relationship keys understood by the structural extractor.")

(defconst ansible-intelligence-yaml--section-keys
  '("tasks" "handlers" "block" "rescue" "always")
  "Mapping keys that establish outline/task sections.")

(defconst ansible-intelligence-yaml--action-keys
  '("name" "hosts" "roles" "vars" "vars_files" "register" "when"
    "failed_when" "changed_when" "until" "loop" "loop_control" "tags"
    "notify" "become" "become_user" "environment" "delegate_to"
    "check_mode" "changed_when" "ignore_errors" "ignore_unreachable" "role"
    "no_log" "run_once" "listen" "rescue" "always" "block" "tasks"
    "handlers" "gather_facts")
  "Task metadata keys, excluded when identifying an action pair.")

(defconst ansible-intelligence-yaml--shell-actions
  '("shell" "command" "raw")
  "Ansible actions that own optional Bash block scalars.")

(defun ansible-intelligence-yaml--root (&optional tree)
  "Return TREE or the current YAML tree, when available."
  (or tree
      (when (fboundp 'treesit-buffer-root-node)
        (condition-case nil
            (treesit-buffer-root-node 'yaml)
          (error nil)))))

(defun ansible-intelligence-yaml--node-text (node)
  "Return source text for NODE without requiring a second argument."
  (condition-case nil
      (treesit-node-text node)
    (error "")))

(defun ansible-intelligence-yaml--key-text (node)
  "Normalize a YAML key NODE to its identifier text."
  (let ((text (string-trim (ansible-intelligence-yaml--node-text node))))
    (if (and (> (length text) 1)
             (memq (aref text 0) '(?\" ?\'))
             (= (aref text 0) (aref text (1- (length text)))))
        (substring text 1 -1)
      text)))

(defun ansible-intelligence-yaml--target-text (text)
  "Normalize a static relationship TEXT while preserving dynamic markers."
  (let ((trimmed (string-trim text)))
    (if (and (> (length trimmed) 1)
             (memq (aref trimmed 0) '(?\" ?\'))
             (= (aref trimmed 0) (aref trimmed (1- (length trimmed)))))
        (substring trimmed 1 -1)
      trimmed)))

(defun ansible-intelligence-yaml--scalar-node-p (node)
  "Return non-nil when NODE is a verified YAML scalar node."
  (member (treesit-node-type node) ansible-intelligence-yaml--scalar-types))

(defun ansible-intelligence-yaml--scalar-leaves (node)
  "Return verified scalar leaves below NODE in source order."
  (let (result)
    (cl-labels
        ((walk (current)
           (if (ansible-intelligence-yaml--scalar-node-p current)
               (push current result)
             (dotimes (index (treesit-node-child-count current))
               (walk (treesit-node-child current index))))))
      (when node (walk node)))
    (nreverse result)))

(defun ansible-intelligence-yaml--direct-child (node types)
  "Return the first direct child of NODE whose type is in TYPES."
  (when node
    (let ((types (if (listp types) types (list types)))
          result)
      (dotimes (index (treesit-node-child-count node))
        (let ((child (treesit-node-child node index)))
          (when (and (null result)
                     (member (treesit-node-type child) types))
            (setq result child))))
      result)))

(defun ansible-intelligence-yaml--sequence-node (node)
  "Return the direct sequence node represented by NODE, or nil."
  (if (and node
           (member (treesit-node-type node)
                   '("block_sequence" "flow_sequence")))
      node
    (and node
         (member (treesit-node-type node)
                 '("block_node" "flow_node"))
         (ansible-intelligence-yaml--direct-child
          node '("block_sequence" "flow_sequence")))))

(defun ansible-intelligence-yaml--sequence-items (node)
  "Return direct sequence items represented by NODE."
  (when-let* ((sequence (ansible-intelligence-yaml--sequence-node node)))
    (let ((item-types
           (if (string= (treesit-node-type sequence) "block_sequence")
               '("block_sequence_item")
             '("flow_node")))
          items)
      (dotimes (index (treesit-node-child-count sequence))
        (let ((child (treesit-node-child sequence index)))
          (when (member (treesit-node-type child) item-types)
            (push child items))))
      (nreverse items))))

(defun ansible-intelligence-yaml--direct-mapping-node (node)
  "Return a direct mapping represented by NODE, or nil."
  (cond
   ((and node
         (member (treesit-node-type node)
                 '("block_mapping" "flow_mapping")))
    node)
   ((and node
         (member (treesit-node-type node)
                 '("block_node" "flow_node" "block_sequence_item")))
    (or (ansible-intelligence-yaml--direct-child
         node '("block_mapping" "flow_mapping"))
        (when-let* ((child
                    (ansible-intelligence-yaml--direct-child
                     node '("block_node" "flow_node"))))
          (ansible-intelligence-yaml--direct-mapping-node child))))))

(defun ansible-intelligence-yaml--direct-mapping-pairs (mapping)
  "Return direct block or flow mapping pairs below MAPPING."
  (when mapping
    (let ((pair-types
           (if (string= (treesit-node-type mapping) "block_mapping")
               '("block_mapping_pair")
             '("flow_pair")))
          pairs)
      (dotimes (index (treesit-node-child-count mapping))
        (let ((child (treesit-node-child mapping index)))
          (when (member (treesit-node-type child) pair-types)
            (push child pairs))))
      (nreverse pairs))))

(defun ansible-intelligence-yaml--direct-scalar-node (node)
  "Return a scalar directly represented by NODE, or nil."
  (cond
   ((ansible-intelligence-yaml--scalar-node-p node) node)
   ((and node
         (member (treesit-node-type node)
                 '("block_node" "flow_node" "block_sequence_item")))
    (let ((leaves (ansible-intelligence-yaml--scalar-leaves node)))
      (and (= (length leaves) 1) (car leaves))))))

(defun ansible-intelligence-yaml--mapping-target-nodes (mapping keys)
  "Return scalar values of direct MAPPING pairs whose keys are in KEYS."
  (let (targets)
    (dolist (pair (ansible-intelligence-yaml--direct-mapping-pairs mapping))
      (when (member
             (ansible-intelligence-yaml--key-text
              (ansible-intelligence-yaml--pair-key-node pair))
             keys)
        (when-let* ((value (ansible-intelligence-yaml--direct-scalar-node
                            (ansible-intelligence-yaml--pair-value-node
                             pair))))
          (push value targets))))
    (nreverse targets)))

(defun ansible-intelligence-yaml--relationship-target-nodes (value-node keys)
  "Return direct scalar relationship targets below VALUE-NODE.

Sequence scalar items are returned as-is.  Mapping sequence items contribute
only direct pair values whose keys are in KEYS; nested metadata is ignored."
  (let ((sequence (ansible-intelligence-yaml--sequence-node value-node)))
    (if sequence
        (let (targets)
          (dolist (item (ansible-intelligence-yaml--sequence-items sequence))
            (if-let* ((mapping
                       (ansible-intelligence-yaml--direct-mapping-node item)))
                (setq targets
                      (append targets
                              (ansible-intelligence-yaml--mapping-target-nodes
                               mapping keys)))
              (when-let* ((scalar
                           (ansible-intelligence-yaml--direct-scalar-node
                            item)))
                (setq targets (append targets (list scalar))))))
          targets)
      (if-let* ((mapping
                 (ansible-intelligence-yaml--direct-mapping-node value-node)))
          (ansible-intelligence-yaml--mapping-target-nodes mapping keys)
        (when-let* ((scalar
                     (ansible-intelligence-yaml--direct-scalar-node
                      value-node)))
          (list scalar))))))

(defun ansible-intelligence-yaml--ancestors (node)
  "Return NODE's parents, nearest first."
  (let (result)
    (while (setq node (treesit-node-parent node))
      (push node result))
    (nreverse result)))

(defun ansible-intelligence-yaml--mapping-node (pair)
  "Return the direct mapping parent for PAIR."
  (seq-find (lambda (node)
              (string= (treesit-node-type node) "block_mapping"))
            (ansible-intelligence-yaml--ancestors pair)))

(defun ansible-intelligence-yaml--mapping-pairs (mapping)
  "Return direct mapping pair nodes below MAPPING."
  (let (pairs)
    (when mapping
      (dotimes (index (treesit-node-child-count mapping))
        (let ((child (treesit-node-child mapping index)))
          (when (string= (treesit-node-type child) "block_mapping_pair")
            (push child pairs)))))
    (nreverse pairs)))

(defun ansible-intelligence-yaml--pair-key-node (pair)
  "Return PAIR's verified key node."
  (treesit-node-child-by-field-name pair "key"))

(defun ansible-intelligence-yaml--pair-value-node (pair)
  "Return PAIR's verified value node."
  (treesit-node-child-by-field-name pair "value"))

(defun ansible-intelligence-yaml--pair-record (pair)
  "Build a structural record for PAIR."
  (let* ((key-node (ansible-intelligence-yaml--pair-key-node pair))
         (value-node (ansible-intelligence-yaml--pair-value-node pair))
         (mapping (ansible-intelligence-yaml--mapping-node pair)))
    (list :pair-node pair
          :key-node key-node
          :value-node value-node
          :key (and key-node
                    (ansible-intelligence-yaml--key-text key-node))
          :value (and value-node
                      (ansible-intelligence-yaml--node-text value-node))
          :start (treesit-node-start pair)
          :end (treesit-node-end pair)
          :key-start (and key-node (treesit-node-start key-node))
          :key-end (and key-node (treesit-node-end key-node))
          :value-start (and value-node (treesit-node-start value-node))
          :value-end (and value-node (treesit-node-end value-node))
          :mapping mapping
          :mapping-pairs (ansible-intelligence-yaml--mapping-pairs mapping)
          :ancestors (ansible-intelligence-yaml--ancestors pair))))

(defun ansible-intelligence-yaml--within-p (node ancestor)
  "Return non-nil when NODE is ANCESTOR or below ANCESTOR."
  (or (eq node ancestor)
      (seq-some (lambda (parent) (eq parent ancestor))
                (ansible-intelligence-yaml--ancestors node))))

(defun ansible-intelligence-yaml--ancestor-key (record keys)
  "Return the nearest ancestor pair key in KEYS for RECORD."
  (seq-some
   (lambda (ancestor)
     (when (and (string= (treesit-node-type ancestor) "block_mapping_pair")
                (member
                 (ansible-intelligence-yaml--key-text
                  (ansible-intelligence-yaml--pair-key-node ancestor))
                 keys))
       (ansible-intelligence-yaml--key-text
        (ansible-intelligence-yaml--pair-key-node ancestor))))
   (plist-get record :ancestors)))

(defun ansible-intelligence-yaml--mapping-in-sequence-item-p (mapping)
  "Return non-nil when MAPPING is the direct map of a sequence item."
  (let* ((parents (ansible-intelligence-yaml--ancestors mapping))
         (block (car parents))
         (item (cadr parents)))
    (and block item
         (string= (treesit-node-type block) "block_node")
         (string= (treesit-node-type item) "block_sequence_item"))))

(defun ansible-intelligence-yaml--mapping-has-key-p (record key)
  "Return non-nil when RECORD's direct mapping contains KEY."
  (seq-some
   (lambda (pair)
     (string= key
             (ansible-intelligence-yaml--key-text
              (ansible-intelligence-yaml--pair-key-node pair))))
   (plist-get record :mapping-pairs)))

(defun ansible-intelligence-yaml--task-record-p (record)
  "Return non-nil when RECORD belongs to a direct task/play mapping."
  (ansible-intelligence-yaml--mapping-in-sequence-item-p
   (plist-get record :mapping)))

(defun ansible-intelligence-yaml--outline
    (name kind start end category &optional confidence)
  "Create a source-local outline entry."
  (ansible-intelligence-outline-entry-create
   :name name :kind kind
   :range (ansible-intelligence-range-create :start start :end end)
   :category category :source "YAML structural query"
   :confidence (or confidence 'static) :file buffer-file-name))

(defun ansible-intelligence-yaml--link
    (kind target start end &optional confidence)
  "Create a source-local relationship link."
  (ansible-intelligence-link-create
   :kind kind :target target
   :range (ansible-intelligence-range-create :start start :end end)
   :confidence (or confidence 'static)))

(defun ansible-intelligence-yaml--link-target-records (record kind)
  "Return target records for relationship RECORD of KIND."
  (let* ((value-node (plist-get record :value-node))
         (keys (cond
                ((memq kind '(include-role import-role))
                 '("name" "role"))
                ((memq kind '(include-vars include-tasks import-tasks vars-files))
                 '("file"))))
         (nodes (if keys
                    (ansible-intelligence-yaml--relationship-target-nodes
                     value-node keys)
                  (ansible-intelligence-yaml--scalar-leaves value-node))))
    (mapcar #'list nodes)))

(defun ansible-intelligence-yaml--extract-links (records)
  "Extract static and explicitly dynamic links from RECORDS."
  (let (links)
    (dolist (record records)
      (let* ((key (plist-get record :key))
             (short-key
              (if (string-match "\\(?:^\\|\\.\\)\\([[:alnum:]_]+\\)\\'" key)
                  (match-string 1 key)
                key)))
        (when-let* ((kind (cdr (assoc short-key
                                      ansible-intelligence-yaml--link-keys))))
        (dolist (target-record
                 (ansible-intelligence-yaml--link-target-records record kind))
          (let* ((node (car target-record))
                 (raw (ansible-intelligence-yaml--node-text node))
                 (target (ansible-intelligence-yaml--target-text raw))
                 (dynamic (string-match-p "{{\\|{%\\|{#" target)))
            (unless (string-empty-p target)
              (push (ansible-intelligence-yaml--link
                     kind target (treesit-node-start node)
                     (treesit-node-end node)
                     (if dynamic 'dynamic 'static))
                    links))))))
      )
    (sort links
          (lambda (left right)
            (< (ansible-intelligence-range-start
                (ansible-intelligence-link-range left))
               (ansible-intelligence-range-start
                (ansible-intelligence-link-range right)))))))

(defun ansible-intelligence-yaml--extract-shell-ranges (records)
  "Extract structural shell block ranges from action RECORDS."
  (let (ranges)
    (dolist (record records)
      (let* ((key (plist-get record :key))
             (action (if (string-match "\\(?:^\\|\\.\\)\\(shell\\|command\\|raw\\)\\'"
                                       key)
                         (match-string 1 key))))
        (when action
          (dolist (node
                   (ansible-intelligence-yaml--scalar-leaves
                    (plist-get record :value-node)))
            (when (string= (treesit-node-type node) "block_scalar")
              (push
               (ansible-intelligence-shell-range-create
                :kind 'block-scalar
                :range (ansible-intelligence-range-create
                        :start (treesit-node-start node)
                        :end (treesit-node-end node))
                :action action :parser nil)
               ranges))))))
    (nreverse ranges)))

(defun ansible-intelligence-yaml--extract-outline (records)
  "Extract plays, tasks, blocks, roles, and action outlines from RECORDS."
  (let (entries)
    (dolist (record records)
      (let* ((key (plist-get record :key))
             (mapping (plist-get record :mapping))
             (task-map-p (ansible-intelligence-yaml--task-record-p record))
             (direct-map-p
              (ansible-intelligence-yaml--mapping-in-sequence-item-p mapping))
             (section (ansible-intelligence-yaml--ancestor-key
                       record ansible-intelligence-yaml--section-keys)))
        (cond
         ((and (string= key "name") direct-map-p
               (not (ansible-intelligence-yaml--ancestor-key
                     record '("roles"))))
          (let* ((kind (if (ansible-intelligence-yaml--mapping-has-key-p
                           record "hosts")
                           'play
                         (if (string= section "handlers")
                             'handler
                           'task)))
                 (category (cond
                            ((eq kind 'play) "Plays")
                            ((eq kind 'handler) "Handlers")
                            (t "Tasks"))))
            (push
             (ansible-intelligence-yaml--outline
              (ansible-intelligence-yaml--target-text
               (plist-get record :value))
              kind
              (plist-get record :value-start)
              (plist-get record :value-end)
              category)
             entries)))
         ((and task-map-p (member key '("block" "rescue" "always")))
          (push
           (ansible-intelligence-yaml--outline
            key 'block (plist-get record :key-start)
            (or (plist-get record :value-end) (plist-get record :key-end))
            "Blocks")
           entries))
         ((and (string= key "roles")
               (or (ansible-intelligence-yaml--mapping-has-key-p record "hosts")
                   (not task-map-p)))
          (dolist (leaf
                   (ansible-intelligence-yaml--relationship-target-nodes
                    (plist-get record :value-node) '("role" "name")))
            (push
             (ansible-intelligence-yaml--outline
              (ansible-intelligence-yaml--target-text
               (ansible-intelligence-yaml--node-text leaf))
              'role (treesit-node-start leaf) (treesit-node-end leaf)
              "Roles")
             entries)))
         ((and task-map-p
               (not (member key ansible-intelligence-yaml--action-keys))
               (string-match-p "\\(?:^\\|\\.\\)\\([[:alnum:]_]+\\)\\'"
                               key))
          ;; A task without a human name still gets a useful action entry.
          (push
           (ansible-intelligence-yaml--outline
            key 'action (plist-get record :key-start)
            (plist-get record :key-end) "Tasks")
           entries)))))
    (sort entries
          (lambda (left right)
            (< (ansible-intelligence-range-start
                (ansible-intelligence-outline-entry-range left))
               (ansible-intelligence-range-start
                (ansible-intelligence-outline-entry-range right)))))))

(defun ansible-intelligence-yaml--query-records (tree)
  "Return verified mapping records from TREE, or nil on query failure."
  (when (and tree (fboundp 'treesit-query-capture))
    (condition-case nil
        (let (records)
          (dolist (capture
                   (treesit-query-capture
                    tree ansible-intelligence-yaml--mapping-query))
            (when (eq (car capture) 'pair)
              (push
               (ansible-intelligence-yaml--pair-record (cdr capture))
               records)))
          (nreverse records))
      (error nil))))

(defun ansible-intelligence-yaml-query-available-p (&optional tree)
  "Return non-nil when the verified YAML mapping query succeeds."
  (and (ansible-intelligence-yaml--query-records
        (ansible-intelligence-yaml--root tree))
       t))

(defun ansible-intelligence-yaml--fallback-role-item (line line-start)
  "Return a direct role target triple from sequence-item LINE.

The returned value is `(TARGET START END)'.  Mapping items contribute only
their direct `role' or `name' value; nested role variables are ignored."
  (when (string-match "^[ \t]*-[ \t]*\\(.+\\)[ \t]*\\'" line)
    (let* ((raw-start (match-beginning 1))
           (raw (match-string-no-properties 1 line))
           (content (string-trim raw))
           (content-start
            (+ raw-start (or (string-match (regexp-quote content) raw) 0))))
      (if (and (not (string-empty-p content))
               (not (string-match-p
                     "\\`[[:alnum:]_.-]+[ \t]*:" content)))
          (list (ansible-intelligence-yaml--target-text content)
                (+ line-start content-start)
                (+ line-start content-start (length content)))
        (when (string-match
               "\\`\\(?:role\\|name\\):[ \t]*\\(.+\\)\\'" content)
          (let* ((raw-target (match-string-no-properties 1 content))
                 (target (string-trim raw-target))
                 (target-start
                  (+ content-start (match-beginning 1)
                     (or (string-match (regexp-quote target) raw-target) 0))))
            (unless (string-empty-p target)
              (list (ansible-intelligence-yaml--target-text target)
                    (+ line-start target-start)
                    (+ line-start target-start (length target))))))))))

(defun ansible-intelligence-yaml--fallback-outline (kind)
  "Build a conservative outline from source lines when queries are absent."
  (ignore kind)
  (let (entries section list-kind list-indent list-item-indent)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (point))
               (indent (current-indentation))
               (raw-line
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
               (line (string-trim
                      raw-line)))
          (when (and list-kind list-indent
                     (not (string-empty-p line))
                     (<= indent list-indent)
                     (not (string-match-p
                           "\\`\\(?:- +\\)?roles:[ \t]*\\'" line)))
            (setq list-kind nil list-indent nil list-item-indent nil))
          (cond
           ((string-match
             "\\`\\(?:- +\\)?\\(tasks\\|handlers\\|block\\|rescue\\|always\\):"
             line)
            (setq section (match-string 1 line)))
           ((and (not (eq list-kind 'roles))
                 (string-match "\\`- +name:[ \t]*\\(.+\\)" line))
            (let* ((raw (string-trim (match-string 1 line)))
                   (name (ansible-intelligence-yaml--target-text raw))
                   (play-p
                    (and (zerop indent)
                         (save-excursion
                           (let ((limit (+ (point) 4096))
                                 found)
                             (forward-line 1)
                             (while (and (< (point) limit)
                                         (not (eobp))
                                         (not (looking-at "^- ")))
                               (when (looking-at "[ \t]*hosts:")
                                 (setq found t))
                               (forward-line 1))
                             found))))
                   (category (if play-p "Plays"
                               (if (string= section "handlers")
                                   "Handlers" "Tasks"))))
              (push (ansible-intelligence-yaml--outline
                     name (if play-p 'play
                           (if (string= section "handlers") 'handler 'task))
                     (+ line-start (string-match (regexp-quote raw)
                                                 (buffer-substring-no-properties
                                                  line-start (line-end-position))))
                     (line-end-position) category
                     'recovered)
                    entries)))
           ((string-match "\\`\\(?:- +\\)?\\(roles\\):[ \t]*\\(.+\\)" line)
            (let ((raw (string-trim (match-string 2 line))))
              (push (ansible-intelligence-yaml--outline
                     (ansible-intelligence-yaml--target-text raw) 'role
                     (+ line-start (string-match (regexp-quote raw)
                                                 (buffer-substring-no-properties
                                                  line-start (line-end-position))))
                     (line-end-position) "Roles" 'recovered)
                    entries)))
           ((string-match "\\`\\(?:- +\\)?\\(block\\|rescue\\|always\\):" line)
            (push (ansible-intelligence-yaml--outline
                   (match-string 1 line) 'block line-start (line-end-position)
                   "Blocks" 'recovered)
                  entries))
           ((string-match
             "\\`\\(?:- +\\)?\\(vars_files\\|import_tasks\\|include_tasks\\|include_vars\\|roles\\):[ \t]*\\(.+\\)"
             line)
            (setq list-kind (intern (replace-regexp-in-string
                                     "_" "-" (match-string 1 line)))))
           ((string-match
             "\\`\\(?:- +\\)?\\(roles\\):[ \t]*\\'" line)
            (setq list-kind 'roles list-indent indent list-item-indent nil))
           ((and (eq list-kind 'roles)
                 (> indent list-indent)
                 (string-match-p "^[ \t]*-" raw-line))
            (when-let* ((target
                        (ansible-intelligence-yaml--fallback-role-item
                         raw-line line-start)))
              (when (or (null list-item-indent)
                        (= indent list-item-indent))
                (push
                 (ansible-intelligence-yaml--outline
                  (nth 0 target) 'role (nth 1 target) (nth 2 target)
                  "Roles" 'recovered)
                 entries)
                (setq list-item-indent indent)))))
        (forward-line 1)))
    (nreverse entries))))

(defun ansible-intelligence-yaml--fallback-links ()
  "Build conservative static links from source lines."
  (let* ((links nil)
         (list-kind nil)
         (list-indent nil)
         (list-item-indent nil)
         (add-link
          (lambda (kind target start end)
            (unless (string-empty-p target)
              (push
               (ansible-intelligence-yaml--link
                kind target start end
                (if (string-match-p "{{\\|{%\\|{#" target)
                    'dynamic 'static))
               links)))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (point))
               (indent (current-indentation))
               (line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (blank-p (string-match-p "^[ \t]*$" line)))
          (when (and list-kind (not blank-p) (<= indent list-indent))
            (setq list-kind nil list-indent nil list-item-indent nil))
          (cond
           ((string-match
             "^[ \t]*\\(?:-[ \t]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_tasks\\|include_tasks\\|vars_files\\|include_vars\\):[ \t]*\\([^#\n]+\\)"
             line)
            (let* ((kind
                    (intern (replace-regexp-in-string
                            "_" "-" (match-string 1 line))))
                   (raw (string-trim (match-string 2 line)))
                   (value-start (+ line-start (match-beginning 2)))
                   (mapping-file
                    (and (string-match-p "\\`{" raw)
                         (string-match
                          "[{,][ \t]*file:[ \t]*\\([^,}#]+\\)" raw))))
              (if mapping-file
                  (let* ((raw-target (match-string 1 raw))
                         (target (string-trim raw-target))
                         (target-start
                          (+ value-start (match-beginning 1)
                             (or (string-match (regexp-quote target)
                                               raw-target)
                                 0))))
                    (funcall add-link kind target target-start
                             (+ target-start (length target))))
                (funcall add-link kind
                         (ansible-intelligence-yaml--target-text raw)
                         value-start (+ line-start (match-end 2))))))
           ((string-match
             "^[ \t]*\\(?:-[ \t]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_role\\|include_role\\|import_tasks\\|include_tasks\\|vars_files\\|include_vars\\|roles\\):[ \t]*$"
             line)
            (setq list-kind
                  (pcase (match-string 1 line)
                    ("roles" 'import-role)
                    ("vars_files" 'vars-files)
                    (_ (intern (replace-regexp-in-string
                                "_" "-" (match-string 1 line)))))
                  list-indent indent
                  list-item-indent nil))
           ((and list-kind (> indent list-indent) (not blank-p))
            (if (memq list-kind '(import-role include-role))
                (progn
                  (when (string-match-p "^[ \t]*-" line)
                    (when-let* ((target
                                (ansible-intelligence-yaml--fallback-role-item
                                 line line-start)))
                      (when (or (null list-item-indent)
                                (= indent list-item-indent))
                        (funcall add-link list-kind (nth 0 target)
                                 (nth 1 target) (nth 2 target))
                        (setq list-item-indent indent))))
                  (when (string-match
                         "^[ \t]*\\(?:name\\|role\\):[ \t]*\\([^#\n]+\\)"
                         line)
                    (when (or (null list-item-indent)
                              (= indent list-item-indent))
                      (funcall add-link list-kind
                               (ansible-intelligence-yaml--target-text
                                (string-trim (match-string 1 line)))
                               (+ line-start (match-beginning 1))
                               (+ line-start (match-end 1)))
                      (setq list-item-indent indent))))
              (progn
                (when (null list-item-indent)
                  (setq list-item-indent indent))
                (when (and (memq list-kind
                                 '(import-tasks include-tasks include-vars
                                   vars-files))
                           (= indent list-item-indent)
                           (string-match
                            "^[ \t]*\\(?:-[ \t]*\\)?file:[ \t]*\\([^#\n]+\\)"
                            line))
                  (let* ((target (string-trim (match-string 1 line)))
                         (target-start
                          (+ line-start (match-beginning 1)
                             (or (string-match (regexp-quote target)
                                               (match-string 1 line))
                                 0))))
                    (funcall add-link list-kind target target-start
                             (+ target-start (length target)))))
                (when (string-match
                       "^[ \t]*-[ \t]*\\([^#\n]+\\)"
                       line)
                  (let ((raw (string-trim (match-string 1 line))))
                    (unless (string-match-p
                             "\\`\\(?:file\\|name\\|role\\):[ \t]*"
                             raw)
                      (funcall add-link list-kind
                               (ansible-intelligence-yaml--target-text raw)
                               (+ line-start (match-beginning 1))
                               (+ line-start (match-end 1)))))))))))
          (forward-line 1))
    (nreverse links))))

(defun ansible-intelligence-yaml--fallback-shell-ranges ()
  "Return structural shell block ranges without a YAML tree."
  (let (ranges)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (point))
              (indent (current-indentation))
              (line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (when (string-match
                 "^[ \t-]*\\([[:alnum:]_.]+\\)[ \t]*:[ \t]*\\([|>]\\)"
                 line)
            (let* ((key (match-string 1 line))
                   (start (+ line-start (match-beginning 2)))
                   (action
                    (and (string-match
                          "\\(?:^\\|\\.\\)\\(shell\\|command\\|raw\\)\\'"
                          key)
                         (match-string 1 key)))
                   end)
              (when action
                (save-excursion
                  (forward-line 1)
                  (while (and (not (eobp))
                              (or (string-match-p "^[ \t]*$"
                                                  (buffer-substring-no-properties
                                                   (line-beginning-position)
                                                   (line-end-position)))
                                  (> (current-indentation) indent)))
                    (forward-line 1))
                  (setq end (line-beginning-position)))
                (push
                 (ansible-intelligence-shell-range-create
                  :kind 'block-scalar
                  :range (ansible-intelligence-range-create
                          :start start :end end)
                  :action action :parser nil)
                 ranges)))))
        (forward-line 1)))
    (nreverse ranges)))

(defun ansible-intelligence-yaml-fallback-extract (&optional kind)
  "Return defensive structural facts for KIND without a YAML tree."
  (list :available nil :source 'scanner :pairs nil
        :links (ansible-intelligence-yaml--fallback-links)
        :outline (ansible-intelligence-yaml--fallback-outline kind)
        :shell-ranges (ansible-intelligence-yaml--fallback-shell-ranges)))

(defun ansible-intelligence-yaml-query-extract (&optional tree kind)
  "Extract verified YAML facts from TREE, falling back conservatively.

The result is a plist containing `:available', `:pairs', `:links',
`:outline', and `:shell-ranges'.  Query captures are the primary path when
their node shapes are present; malformed or unavailable trees use the
defensive line scanner."
  (let* ((root (ansible-intelligence-yaml--root tree))
         (records (ansible-intelligence-yaml--query-records root)))
    (if records
        (list :available t :source 'query :pairs records
              :links (ansible-intelligence-yaml--extract-links records)
              :outline (ansible-intelligence-yaml--extract-outline records)
              :shell-ranges (ansible-intelligence-yaml--extract-shell-ranges
                             records))
      (ansible-intelligence-yaml-fallback-extract kind))))

(provide 'ansible-intelligence-yaml)

;;; ansible-intelligence-yaml.el ends here
