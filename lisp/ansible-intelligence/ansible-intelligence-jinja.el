;;; ansible-intelligence-jinja.el --- Incremental Jinja regions in YAML -*- lexical-binding: t; -*-

;;; Commentary:
;; Jinja is embedded in YAML scalar text.  This module deliberately parses
;; only the scalar regions that contain Jinja delimiters.  The tree-sitter
;; nodes are copied into buffer-independent model structs before temporary
;; parser buffers are killed.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ansible-intelligence-model)
(require 'ansible-intelligence-yaml)
(require 'treesit nil t)

(defgroup ansible-intelligence-jinja nil
  "Bounded Jinja analysis for Ansible Intelligence."
  :group 'ansible-intelligence)

(defcustom ansible-intelligence-jinja-language 'jinja
  "Tree-sitter language symbol used for Jinja regions."
  :type 'symbol
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-scalars 500
  "Maximum YAML scalar nodes inspected during one analysis."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-regions 1000
  "Maximum Jinja delimiter regions inspected during one analysis."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-scalar-bytes 65536
  "Maximum UTF-8 bytes inspected for one YAML scalar."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-total-bytes (* 1024 1024)
  "Maximum UTF-8 bytes inspected across YAML scalars."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-work 20000
  "Maximum approximate AST and lexical work units per analysis."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defcustom ansible-intelligence-jinja-max-node-depth 128
  "Maximum copied tree-sitter node depth for one Jinja region."
  :type 'integer
  :group 'ansible-intelligence-jinja)

(defconst ansible-intelligence-jinja-parser-version
  "jinja-region-v1/tree-sitter-jinja-v0.13.0"
  "Version included in Jinja cache keys.")

(defconst ansible-intelligence-jinja--scalar-node-types
  '("plain_scalar" "single_quote_scalar" "double_quote_scalar"
    "block_scalar" "literal_scalar" "folded_scalar")
  "YAML tree-sitter scalar node types that may contain Jinja.")

(defconst ansible-intelligence-jinja--keywords
  '("and" "as" "autoescape" "block" "break" "call" "continue"
    "debug" "do" "elif" "else" "endautoescape" "endblock" "endcall"
    "endfilter" "endfor" "endif" "endmacro" "endset" "endtrans"
    "endwith" "extends" "filter" "for" "from" "if" "import" "in"
    "include" "macro" "not" "pluralize" "raw" "recursive" "required"
    "set" "trans" "with" "true" "false" "none" "null" "True" "False" "is")
  "Jinja words that cannot be variable references.")

(defconst ansible-intelligence-jinja--bare-expression-keys
  '("when" "failed_when" "changed_when" "until" "loop")
  "High-confidence Ansible keys whose scalar values are expressions.")

(defun ansible-intelligence-jinja--language ()
  "Return the available configured Jinja tree-sitter language, or nil."
  (when (fboundp 'treesit-language-available-p)
    (seq-find
     (lambda (language)
       (condition-case nil
           (treesit-language-available-p language)
         (error nil)))
     (delete-dups
      (list ansible-intelligence-jinja-language 'jinja 'jinja2)))))

(defun ansible-intelligence-jinja-grammar-available-p ()
  "Return non-nil when the configured Jinja grammar can be loaded."
  (and (ansible-intelligence-jinja--language) t))

(defun ansible-intelligence-jinja--range (start end)
  "Create a model range from START through END."
  (ansible-intelligence-range-create :start start :end end))

(defun ansible-intelligence-jinja--range-key (range)
  "Return a comparable key for RANGE."
  (cons (ansible-intelligence-range-start range)
        (ansible-intelligence-range-end range)))

(defun ansible-intelligence-jinja--node-type (node)
  "Return the type of tree-sitter NODE."
  (treesit-node-type node))

(defun ansible-intelligence-jinja--node-text (node)
  "Return NODE text without relying on a live buffer after copying."
  (condition-case nil
      (treesit-node-text node)
    (error "")))

(defun ansible-intelligence-jinja--tree-scalar-ranges (tree)
  "Return scalar ranges from YAML TREE."
  (let (ranges)
    (cl-labels
        ((walk (node)
           (let ((type (ansible-intelligence-jinja--node-type node)))
             (if (member type ansible-intelligence-jinja--scalar-node-types)
                 (push (ansible-intelligence-jinja--range
                        (treesit-node-start node)
                        (treesit-node-end node))
                       ranges)
               (dotimes (index (treesit-node-child-count node))
                 (walk (treesit-node-child node index)))))))
      (when tree (walk tree)))
    (delete-dups
     (mapcar (lambda (range)
               (cons (ansible-intelligence-jinja--range-key range) range))
             ranges))))

(defun ansible-intelligence-jinja--comment-before-p (position)
  "Return non-nil when POSITION is after an unquoted YAML comment marker."
  (save-excursion
    (let ((line-start (line-beginning-position))
          (quote-char nil)
          (escaped nil)
          (comment nil))
      (goto-char line-start)
      (while (and (< (point) position) (not comment))
        (let ((char (char-after)))
          (cond
           (escaped (setq escaped nil))
           ((and quote-char (= char ?\\)) (setq escaped t))
           ((and quote-char (= char quote-char)) (setq quote-char nil))
           ((and (not quote-char) (= char ?\\))
            (setq escaped t))
           ((and (not quote-char) (memq char '(?\" ?\')))
            (setq quote-char char))
           ((and (not quote-char) (= char ?#)) (setq comment t)))
          (forward-char 1)))
      comment)))

(defun ansible-intelligence-jinja--fallback-scalar-ranges ()
  "Return conservative line-based scalar ranges when YAML nodes are absent."
  (let (ranges)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line-start (line-beginning-position))
              (line-end (line-end-position)))
          (when (and (re-search-forward "{{\\|{%\\|{#" line-end t)
                     (not (ansible-intelligence-jinja--comment-before-p
                           (match-beginning 0))))
            (let ((marker (match-beginning 0))
                  start)
              (save-excursion
                (goto-char line-start)
                (when (re-search-forward ":[ \t]*" marker t)
                  (setq start (point))))
              (unless start
                (save-excursion
                  (goto-char line-start)
                  (when (looking-at "[ \t]*-[ \t]*")
                    (setq start (match-end 0)))))
              (push (ansible-intelligence-jinja--range
                     (or start line-start) line-end)
                    ranges))))
        (forward-line 1)))
    (delete-dups
     (mapcar (lambda (range)
               (cons (ansible-intelligence-jinja--range-key range) range))
             ranges))))

(defun ansible-intelligence-jinja--scalar-ranges (&optional yaml-tree)
  "Return YAML scalar ranges from YAML-TREE or the defensive fallback."
  (or (ansible-intelligence-jinja--tree-scalar-ranges yaml-tree)
      (ansible-intelligence-jinja--fallback-scalar-ranges)))

(defun ansible-intelligence-jinja--semantic-scalar (text start end)
  "Return semantic scalar TEXT and exact source ranges.

The first element is the trimmed semantic text, the second is its source
scalar range, and the third excludes YAML quote delimiters when present."
  (let* ((left 0)
         (right (length text)))
    (while (and (< left right)
                (memq (aref text left) '(?\s ?\t ?\n ?\r)))
      (setq left (1+ left)))
    (while (and (> right left)
                (memq (aref text (1- right)) '(?\s ?\t ?\n ?\r)))
      (setq right (1- right)))
    (let* ((source-start (+ start left))
           (source-end (min end (+ start right)))
           (source-text (substring text left right))
           (quoted-p
            (and (> (length source-text) 1)
                 (memq (aref source-text 0) '(?\" ?\'))
                 (= (aref source-text 0)
                    (aref source-text (1- (length source-text)))))))
      (list (if quoted-p (substring source-text 1 -1) source-text)
            (ansible-intelligence-jinja--range source-start source-end)
            (if quoted-p
                (ansible-intelligence-jinja--range
                 (1+ source-start) (1- source-end))
              (ansible-intelligence-jinja--range source-start source-end))))))

(defun ansible-intelligence-jinja--bare-region
    (key scalar-range text &optional semantic-range)
  "Create a synthetic Jinja region for KEY and scalar TEXT.

The parser sees a wrapper beginning three characters before the source
expression.  `:analysis-base' records that offset so all copied ranges map
back to the original YAML scalar."
  (let* ((range (or semantic-range scalar-range))
         (analysis-text (format "{{ %s }}" text))
         (key-data (list ansible-intelligence-jinja-parser-version
                         (or (ansible-intelligence-jinja--language) 'fallback)
                         'bare-expression key
                         (secure-hash 'sha256 analysis-text))))
    (list :kind 'bare-expression
          :range range
          :content-range range
          :scalar-range scalar-range
          :text text
          :analysis-text analysis-text
          :analysis-base 3
          :key key-data
          :expression-key key
          :complete t)))

(defun ansible-intelligence-jinja--bare-region-from-scalar
    (key text start end)
  "Create a bare region for scalar TEXT from START through END."
  (pcase-let ((`(,semantic-text ,scalar-range ,semantic-range)
               (ansible-intelligence-jinja--semantic-scalar text start end)))
    (unless (string-empty-p semantic-text)
      (ansible-intelligence-jinja--bare-region
       key scalar-range semantic-text semantic-range))))

(defun ansible-intelligence-jinja--bare-region-data-from-query (&optional yaml-tree)
  "Return bare-expression regions from verified YAML structural records."
  (let* ((facts (ansible-intelligence-yaml-query-extract yaml-tree))
         (records (plist-get facts :pairs))
         result)
    (dolist (record records)
      (let ((key (plist-get record :key)))
        (when (member key ansible-intelligence-jinja--bare-expression-keys)
          ;; A condition or loop is only an Ansible expression when it belongs
          ;; to a task/play sequence mapping.  This prevents arbitrary YAML
          ;; values named `when' in unrelated documents from becoming Jinja.
          (when (ansible-intelligence-yaml--task-record-p record)
            (let* ((value-node (plist-get record :value-node))
                   (sequence-p
                    (ansible-intelligence-yaml--sequence-node value-node)))
              ;; A loop sequence is data, not a scalar loop expression.  Its
              ;; members still go through the normal delimiter scanner, so an
              ;; explicit `{{ ... }}` item remains semantic.
              (unless (and (string= key "loop") sequence-p)
                (dolist (node
                         (ansible-intelligence-yaml--scalar-leaves
                          value-node))
                  (let ((text (ansible-intelligence-yaml--node-text node)))
                    (unless (or (string-empty-p (string-trim text))
                                (string-match-p "{{\\|{%\\|{#" text)
                                (string= (treesit-node-type node) "block_scalar"))
                      (push
                       (ansible-intelligence-jinja--bare-region-from-scalar
                        key text (treesit-node-start node)
                        (treesit-node-end node))
                       result))))))))))
    (sort result
          (lambda (left right)
            (< (ansible-intelligence-range-start (plist-get left :range))
               (ansible-intelligence-range-start (plist-get right :range)))))))

(defun ansible-intelligence-jinja--bare-region-data-fallback ()
  "Return conservative bare-expression regions without a YAML tree."
  (let (result active-key active-indent task-indent)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (point))
               (line-end (line-end-position))
               (indent (current-indentation))
               (line (buffer-substring-no-properties line-start line-end)))
          (when (and (string-match-p "^[ \t]*-" line)
                     (or (null task-indent) (<= indent task-indent)))
            (setq task-indent indent))
          (cond
           ((string-match
             "^[ \t]*\\([[:alnum:]_]+\\):[ \t]*\\([^#\n]*\\)"
             line)
            (let ((key (match-string-no-properties 1 line))
                  (raw (string-trim (match-string-no-properties 2 line))))
              (if (and task-indent
                       (> indent task-indent)
                       (member key ansible-intelligence-jinja--bare-expression-keys))
                  (if (string-empty-p raw)
                      (unless (string= key "loop")
                        (setq active-key key active-indent indent))
                    (unless (and (string= key "loop")
                                 (string-match-p "\\`\\[.*\\]\\'" raw))
                      (unless (string-match-p "{{\\|{%\\|{#" raw)
                        (push
                         (ansible-intelligence-jinja--bare-region-from-scalar
                          key raw (+ line-start (match-beginning 2))
                          (+ line-start (match-end 2)))
                         result)))
                    (setq active-key nil active-indent nil))
                (when (or (null active-indent) (<= indent active-indent))
                  (setq active-key nil active-indent nil)))))
           ((and active-key
                 (string-match "^[ \t]*-[ \t]*\\(.+\\)" line))
            (let ((raw (match-string-no-properties 1 line)))
              (unless (or (string-empty-p (string-trim raw))
                          (string-match-p "{{\\|{%\\|{#" raw))
                (push
                 (ansible-intelligence-jinja--bare-region-from-scalar
                  active-key raw (+ line-start (match-beginning 1))
                  (+ line-start (match-end 1)))
                 result))))
           ((and active-indent
                 (not (string-match-p "^[ \t]*$" line))
                 (<= indent active-indent))
            (setq active-key nil active-indent nil))))
        (forward-line 1)))
    (nreverse result)))

(defun ansible-intelligence-jinja--bare-region-data (&optional yaml-tree)
  "Return high-confidence bare Ansible expression regions."
  (or (ansible-intelligence-jinja--bare-region-data-from-query yaml-tree)
      (ansible-intelligence-jinja--bare-region-data-fallback)))

(defun ansible-intelligence-jinja--delimiter-kind (text position)
  "Return the Jinja kind at POSITION in TEXT, or nil."
  (cond
   ((string-prefix-p "{{" (substring text position)) 'expression)
   ((string-prefix-p "{%" (substring text position)) 'statement)
   ((string-prefix-p "{#" (substring text position)) 'comment)))

(defun ansible-intelligence-jinja--closing-token (kind)
  "Return the closing token for Jinja KIND."
  (pcase kind
    ('expression "}}")
    ('statement "%}")
    ('comment "#}")))

(defun ansible-intelligence-jinja--find-closing-token (text start limit kind)
  "Find the next unquoted closing token in TEXT after START before LIMIT."
  (let ((token (ansible-intelligence-jinja--closing-token kind))
        (position start)
        (quote-char nil)
        (escaped nil)
        result)
    (while (and (< position limit) (not result))
      (let ((char (aref text position)))
        (cond
         (escaped (setq escaped nil))
         ((and quote-char (= char ?\\)
               (< (1+ position) limit)
               (= (aref text (1+ position)) quote-char)
               (< (+ position 2) limit)
               (memq (aref text (+ position 2)) '(?\] ?\) ?\} ?,)))
          (setq quote-char nil position (1+ position)))
         ((and quote-char (= char ?\\)) (setq escaped t))
         ((and quote-char (= char quote-char)) (setq quote-char nil))
         ((and (not quote-char) (= char ?\\)
               (< (1+ position) limit)
               (memq (aref text (1+ position)) '(?\" ?\')))
          (setq quote-char (aref text (1+ position))
                position (1+ position)))
         ((and (not quote-char) (memq char '(?\" ?\')))
          (setq quote-char char))
         ((and (not quote-char)
               (or (string-prefix-p token (substring text position))
                   (and (> position start)
                        (memq (aref text (1- position)) '(?- ?+))
                        (string-prefix-p token (substring text position)))))
          (let* ((trimmed (and (> position start)
                               (memq (aref text (1- position)) '(?- ?+))))
                 (content-end (if trimmed (1- position) position))
                 (outer-end (+ position (length token))))
            (setq result (cons content-end outer-end)))))
        (setq position (1+ position))))
    result))

(defun ansible-intelligence-jinja--region-list (scalar-range)
  "Extract Jinja regions from one SCALAR-RANGE."
  (let* ((scalar-start (ansible-intelligence-range-start scalar-range))
         (scalar-end (ansible-intelligence-range-end scalar-range))
         (scalar-text (buffer-substring-no-properties scalar-start scalar-end))
         (position 0)
         regions)
    (while (and (< position (length scalar-text))
                (string-match "{{\\|{%\\|{#" scalar-text position))
      (let* ((marker-start (match-beginning 0))
             (kind (ansible-intelligence-jinja--delimiter-kind
                    scalar-text marker-start))
             (open-end (+ marker-start 2))
             (content-start
              (if (and (< open-end (length scalar-text))
                       (memq (aref scalar-text open-end) '(?- ?+)))
                  (1+ open-end)
                open-end))
             (closing
              (ansible-intelligence-jinja--find-closing-token
               scalar-text content-start (length scalar-text) kind))
             (content-end (or (car closing) (length scalar-text)))
             (region-end (or (cdr closing) (length scalar-text)))
             (absolute-start (+ scalar-start marker-start))
             (absolute-end (+ scalar-start region-end))
             (region-text (substring scalar-text marker-start region-end))
             (key (list ansible-intelligence-jinja-parser-version
                        (or (ansible-intelligence-jinja--language) 'fallback)
                        kind
                        (secure-hash 'sha256 region-text))))
        (push (list :kind kind
                    :range (ansible-intelligence-jinja--range
                            absolute-start absolute-end)
                    :content-range
                    (ansible-intelligence-jinja--range
                     (+ scalar-start content-start)
                     (+ scalar-start content-end))
                    :scalar-range scalar-range
                    :text region-text
                    :key key
                    :complete (and closing t)
                    :error (unless closing "Jinja region is incomplete."))
              regions)
        ;; An unterminated region is bounded by this scalar.  It must not
        ;; consume an unrelated region in a later scalar.
        (setq position (if closing region-end (length scalar-text)))))
    (nreverse regions)))

(defun ansible-intelligence-jinja--identifier-start-p (char)
  "Return non-nil when CHAR can begin a Jinja identifier."
  (or (eq char ?_) (and char (or (and (>= char ?a) (<= char ?z))
                                 (and (>= char ?A) (<= char ?Z))))))

(defun ansible-intelligence-jinja--identifier-char-p (char)
  "Return non-nil when CHAR can continue a Jinja identifier."
  (or (ansible-intelligence-jinja--identifier-start-p char)
      (and char (>= char ?0) (<= char ?9))))

(defun ansible-intelligence-jinja--skip-space-backward (text position)
  "Return POSITION moved backward over whitespace in TEXT."
  (while (and (> position 0) (memq (aref text (1- position)) '(?\s ?\t ?\n ?\r)))
    (setq position (1- position)))
  position)

(defun ansible-intelligence-jinja--previous-word (text position)
  "Return the word immediately before POSITION in TEXT."
  (setq position (ansible-intelligence-jinja--skip-space-backward text position))
  (let ((end position))
    (while (and (> position 0)
                (ansible-intelligence-jinja--identifier-char-p
                 (aref text (1- position))))
      (setq position (1- position)))
    (and (< position end) (substring text position end))))

(defun ansible-intelligence-jinja--definition-range-p (ranges start end)
  "Return non-nil when START through END is in definition RANGES."
  (seq-some
   (lambda (item)
     (let ((range (if (ansible-intelligence-symbol-p item)
                      (ansible-intelligence-symbol-range item)
                    item)))
     (and (<= (ansible-intelligence-range-start range) start)
          (<= end (ansible-intelligence-range-end range)))))
   ranges))

(defun ansible-intelligence-jinja--path-after (text start)
  "Return dotted and quoted-bracket path segments after START in TEXT."
  (let ((position start)
        path
        ranges)
    (cl-labels
        ((space ()
           (setq position
                 (ansible-intelligence-jinja--skip-space-backward
                  text position)))
         (forward-space ()
           (while (and (< position (length text))
                       (memq (aref text position) '(?\s ?\t ?\n ?\r)))
             (setq position (1+ position)))))
      (while (< position (length text))
        (forward-space)
        (cond
         ((eq (aref text position) ?.)
          (setq position (1+ position))
          (forward-space)
          (let ((start-position position))
            (while (and (< position (length text))
                        (ansible-intelligence-jinja--identifier-char-p
                         (aref text position)))
              (setq position (1+ position)))
            (if (> position start-position)
                (progn
                  (push (substring text start-position position) path)
                  (push (ansible-intelligence-jinja--range
                         start-position position)
                        ranges))
              (setq position (length text)))))
         ((eq (aref text position) ?\[)
          (setq position (1+ position))
          (forward-space)
          (if (and (< position (length text))
                   (or (memq (aref text position) '(?\" ?\'))
                       (and (= (aref text position) ?\\)
                            (< (1+ position) (length text))
                            (memq (aref text (1+ position)) '(?\" ?\')))))
              (let* ((escaped-open (= (aref text position) ?\\))
                     (quote-position (if escaped-open (1+ position) position))
                     (quote-char (aref text quote-position))
                     (field-start (1+ quote-position))
                     (field-end field-start)
                     closed)
                (setq position (1+ quote-position))
                (while (and (< position (length text)) (not closed))
                  (cond
                   ((and escaped-open (= (aref text position) ?\\)
                         (< (1+ position) (length text))
                         (= (aref text (1+ position)) quote-char))
                    (setq field-end position
                          position (+ position 2)
                          closed t))
                   ((and (not escaped-open)
                         (= (aref text position) quote-char)
                         (/= (aref text (1- position)) ?\\))
                    (setq field-end position
                          position (1+ position)
                          closed t))
                   (t (setq position (1+ position)))))
                (when closed
                  (forward-space)
                  (when (and (< position (length text))
                             (= (aref text position) ?\]))
                    (setq position (1+ position))
                    (push (substring text field-start field-end) path)
                    (push (ansible-intelligence-jinja--range
                           field-start field-end)
                          ranges))))
            ;; Dynamic indexes are not facts about a nested field.
            (setq position (length text))))
         (t (setq position (length text)))))
      (list (nreverse path) (nreverse ranges)))))

(defun ansible-intelligence-jinja--statement-content (text)
  "Return the inner content bounds of a Jinja TEXT region."
  (let* ((open-end (if (and (> (length text) 2)
                            (memq (aref text 2) '(?- ?+)))
                       3 2))
         (close (or (string-match "\\(?:[-+]\\)?\\(?:%}\\|#}\\)\\'" text)
                    (string-match "\\(?:[-+]\\)?}}\\'" text)
                    (length text))))
    (cons open-end (max open-end close))))

(defun ansible-intelligence-jinja--definitions (text)
  "Extract relative Jinja set and for definitions from TEXT."
  (let* ((bounds (ansible-intelligence-jinja--statement-content text))
         (start (car bounds))
         (end (cdr bounds))
         (inner (substring text start end))
         definitions)
    (cl-labels
        ((add-identifiers (from to kind source)
           (let ((position from))
             (while (and (< position to)
                         (string-match
                          "[[:alpha:]_][[:alnum:]_]*" inner position))
               (let* ((relative-start (match-beginning 0))
                      (relative-end (match-end 0))
                      (name (match-string-no-properties 0 inner))
                      (absolute-start (+ start relative-start)))
                 (if (> relative-end to)
                     (setq position to)
                   (unless (member name ansible-intelligence-jinja--keywords)
                     (push
                      (ansible-intelligence-symbol-create
                       :name name :kind kind
                       :range (ansible-intelligence-jinja--range
                               absolute-start (+ start relative-end))
                       :visibility-start (length text)
                       :visibility-end nil
                       :source source :confidence 'static :file nil)
                      definitions))
                   (setq position relative-end)))))))
      (when (string-match "\\_<set\\_>[ \t\n]+\\([^=]+\\)=" inner)
        (add-identifiers (match-beginning 1) (match-end 1)
                         'jinja-set "Jinja set statement"))
      (when (string-match "\\_<for\\_>[ \t\n]+\\(.*?\\)[ \t\n]+\\_<in\\_>"
                          inner)
        (add-identifiers (match-beginning 1) (match-end 1)
                         'jinja-loop-var "Jinja for loop")))
    (nreverse definitions)))

(defun ansible-intelligence-jinja--lexical-references (text definitions)
  "Extract conservative variable references from TEXT.

DEFINITIONS contains relative ranges whose identifiers are not references."
  (let ((position 0)
        (quote-char nil)
        (escaped nil)
        references)
    (while (< position (length text))
      (let ((char (aref text position)))
        (cond
         (escaped (setq escaped nil position (1+ position)))
         ((and quote-char (= char ?\\)
               (< (1+ position) (length text))
               (= (aref text (1+ position)) quote-char)
               (< (+ position 2) (length text))
               (memq (aref text (+ position 2)) '(?\] ?\) ?\} ?,)))
          (setq quote-char nil position (+ position 2)))
         ((and quote-char (= char ?\\))
          (setq escaped t position (1+ position)))
         ((and quote-char (= char quote-char))
          (setq quote-char nil position (1+ position)))
         ((and (not quote-char) (= char ?\\)
               (< (1+ position) (length text))
               (memq (aref text (1+ position)) '(?\" ?\')))
          (setq quote-char (aref text (1+ position))
                position (+ position 2)))
         ((and (not quote-char) (memq char '(?\" ?\')))
          (setq quote-char char position (1+ position)))
         ((and (not quote-char)
               (ansible-intelligence-jinja--identifier-start-p char))
          (let ((start position))
            (setq position (1+ position))
            (while (and (< position (length text))
                        (ansible-intelligence-jinja--identifier-char-p
                         (aref text position)))
              (setq position (1+ position)))
            (let* ((end position)
                   (name (substring text start end))
                   (before (ansible-intelligence-jinja--skip-space-backward
                            text start))
                   (previous-char (and (> before 0) (aref text (1- before))))
                   (previous-word (ansible-intelligence-jinja--previous-word
                                   text start))
                   (after position))
              (while (and (< after (length text))
                          (memq (aref text after) '(?\s ?\t ?\n ?\r)))
                (setq after (1+ after)))
              (when (not
                    (or (member name ansible-intelligence-jinja--keywords)
                        (ansible-intelligence-jinja--definition-range-p
                         definitions start end)
                        (eq previous-char ?.)
                        (eq previous-char ?|)
                        (member previous-word '("is" "not"))
                        ;; A single equals denotes a keyword argument.  Do not
                        ;; confuse it with ==.
                        (and (< after (length text))
                             (= (aref text after) ?=)
                             (or (= (1+ after) (length text))
                                 (/= (aref text (1+ after)) ?=)))
                        ;; Calls name a function/filter, not a variable.
                        (and (< after (length text))
                             (= (aref text after) ?\())))
                (pcase-let ((`(,path ,path-ranges)
                              (ansible-intelligence-jinja--path-after text end)))
                  (push
                   (ansible-intelligence-reference-create
                    :name name
                    :range (ansible-intelligence-jinja--range start end)
                    :file nil :kind 'variable :path path
                    :path-ranges path-ranges)
                   references))))))
         (t (setq position (1+ position))))))
    (nreverse references)))

(defun ansible-intelligence-jinja--node-error-p (node)
  "Return non-nil when NODE or a descendant is an ERROR node."
  (or (string= (ansible-intelligence-jinja--node-type node) "ERROR")
      (cl-loop for index below (treesit-node-child-count node)
               thereis (ansible-intelligence-jinja--node-error-p
                        (treesit-node-child node index)))))

(defun ansible-intelligence-jinja--isolated-block-statement-p (text)
  "Return non-nil when TEXT is a block delimiter parsed without its pair."
  (string-match-p
   (concat
    "\\`{%[-+]?[\t\n ]*\\(?:if\\|elif\\|else\\|endif\\|for\\|endfor\\|"
    "block\\|endblock\\|with\\|endwith\\|filter\\|endfilter\\|macro\\|"
    "endmacro\\|call\\|endcall\\|set\\|endset\\|trans\\|endtrans\\|"
    "autoescape\\|endautoescape\\)\\_>")
   text))

(defun ansible-intelligence-jinja--copy-tree (node state depth)
  "Copy NODE into a model tree, updating mutable work STATE."
  (if (> depth ansible-intelligence-jinja-max-node-depth)
      (progn
        (setf (nth 1 state) t)
        nil)
    (setf (nth 0 state) (1+ (nth 0 state)))
    (if (> (nth 0 state) ansible-intelligence-jinja-max-work)
        (progn
          (setf (nth 1 state) t)
          nil)
      (let (children)
        (dotimes (index (treesit-node-child-count node))
          (when-let* ((child
                      (ansible-intelligence-jinja--copy-tree
                       (treesit-node-child node index) state (1+ depth))))
            (push child children)))
        (ansible-intelligence-jinja-node-create
         :type (ansible-intelligence-jinja--node-type node)
         :range (ansible-intelligence-jinja--range
                 (1- (treesit-node-start node))
                 (1- (treesit-node-end node)))
         :text (ansible-intelligence-jinja--node-text node)
         :children (nreverse children)
         :named (condition-case nil
                    (treesit-node-check node 'named)
                  (error t)))))))

(defun ansible-intelligence-jinja--copy-tree-rebase (node base)
  "Copy relative model NODE to absolute offsets at BASE."
  (when node
    (ansible-intelligence-jinja-node-create
     :type (ansible-intelligence-jinja-node-type node)
     :range
     (ansible-intelligence-jinja--range
      (+ base (ansible-intelligence-range-start
               (ansible-intelligence-jinja-node-range node)))
      (+ base (ansible-intelligence-range-end
               (ansible-intelligence-jinja-node-range node))))
     :text (ansible-intelligence-jinja-node-text node)
     :children
     (mapcar (lambda (child)
               (ansible-intelligence-jinja--copy-tree-rebase child base))
             (ansible-intelligence-jinja-node-children node))
     :named (ansible-intelligence-jinja-node-named node))))

(defun ansible-intelligence-jinja--rebase-reference (reference base)
  "Return REFERENCE with relative offsets rebased at BASE."
  (ansible-intelligence-reference-create
   :name (ansible-intelligence-reference-name reference)
   :range
   (let ((range (ansible-intelligence-reference-range reference)))
     (ansible-intelligence-jinja--range
      (+ base (ansible-intelligence-range-start range))
      (+ base (ansible-intelligence-range-end range))))
   :file buffer-file-name
   :kind (ansible-intelligence-reference-kind reference)
   :path (ansible-intelligence-reference-path reference)
   :path-ranges
   (mapcar
    (lambda (range)
      (ansible-intelligence-jinja--range
       (+ base (ansible-intelligence-range-start range))
       (+ base (ansible-intelligence-range-end range))))
    (ansible-intelligence-reference-path-ranges reference))))

(defun ansible-intelligence-jinja--rebase-symbol (symbol base)
  "Return Jinja SYMBOL with offsets rebased at BASE."
  (let ((range (ansible-intelligence-symbol-range symbol))
        (visibility-start (ansible-intelligence-symbol-visibility-start symbol))
        (visibility-end (ansible-intelligence-symbol-visibility-end symbol)))
    (ansible-intelligence-symbol-create
     :name (ansible-intelligence-symbol-name symbol)
     :kind (ansible-intelligence-symbol-kind symbol)
     :range
     (ansible-intelligence-jinja--range
      (+ base (ansible-intelligence-range-start range))
      (+ base (ansible-intelligence-range-end range)))
     :visibility-start (+ base visibility-start)
     :visibility-end (and visibility-end (+ base visibility-end))
     :source (ansible-intelligence-symbol-source symbol)
     :confidence (ansible-intelligence-symbol-confidence symbol)
     :fields (ansible-intelligence-symbol-fields symbol)
     :documentation (ansible-intelligence-symbol-documentation symbol)
     :file buffer-file-name
     :line (line-number-at-pos
            (+ base (ansible-intelligence-range-start range))))))

(defun ansible-intelligence-jinja--fallback-analysis (text key complete error)
  "Build a lexical analysis for TEXT with KEY and COMPLETENESS."
  (let* ((definitions (and complete
                           (ansible-intelligence-jinja--definitions text)))
         (references (and complete
                          (ansible-intelligence-jinja--lexical-references
                           text definitions)))
         (errors (delq nil (list error
                                 "Jinja tree-sitter grammar unavailable; lexical fallback is active."))))
    (ansible-intelligence-jinja-analysis-create
     :key key :parser 'fallback :tree nil :references references
     :definitions definitions :errors errors :complete complete
     :work (+ (length references) (length definitions)))))

(defun ansible-intelligence-jinja--parse-analysis (text key complete error)
  "Parse TEXT with tree-sitter when available, falling back safely."
  (let ((language (ansible-intelligence-jinja--language)))
    (if (not language)
        (ansible-intelligence-jinja--fallback-analysis text key complete error)
      (condition-case parse-error
          (with-temp-buffer
            (insert text)
            (treesit-parser-create language)
            (let* ((root (treesit-buffer-root-node language))
                   (state (list 0 nil))
                   (tree (ansible-intelligence-jinja--copy-tree root state 0))
                   (definitions
                    (and complete
                         (ansible-intelligence-jinja--definitions text)))
                   (references
                    (and complete
                         (ansible-intelligence-jinja--lexical-references
                          text definitions)))
                   (grammar-error
                    (and (ansible-intelligence-jinja--node-error-p root)
                         (not (ansible-intelligence-jinja--isolated-block-statement-p
                               text))
                         "Jinja grammar recovery produced a partial AST."))
                   (limit-error
                    (and (nth 1 state)
                         "Jinja AST work limit reached; lexical semantics are partial."))
                   (errors
                    (delq nil (list error grammar-error limit-error))))
              (ansible-intelligence-jinja-analysis-create
               :key key :parser language :tree tree :references references
               :definitions definitions :errors errors
               :complete (and complete (null errors))
               :work (+ (nth 0 state) (length references)
                        (length definitions)))))
        (error
         (ansible-intelligence-jinja--fallback-analysis
          text key complete
          (format "Jinja grammar parse failed (%s); lexical semantics are partial."
                  (error-message-string parse-error))))))))

(defun ansible-intelligence-jinja--analysis-for (region cache)
  "Return cached or newly parsed analysis for REGION using CACHE."
  (let* ((key (plist-get region :key))
         (analysis (and cache (gethash key cache))))
    (if analysis
        (cons analysis t)
      (let ((analysis
             (ansible-intelligence-jinja--parse-analysis
              (or (plist-get region :analysis-text)
                  (plist-get region :text))
              key
              (plist-get region :complete) (plist-get region :error))))
        (when cache (puthash key analysis cache))
        (cons analysis nil)))))

(defun ansible-intelligence-jinja--region-from (region analysis)
  "Materialize REGION from relative ANALYSIS data."
  (let* ((range (plist-get region :range))
        (base (- (ansible-intelligence-range-start range)
                 (or (plist-get region :analysis-base) 0)))
        (references
          (mapcar (lambda (reference)
                    (ansible-intelligence-jinja--rebase-reference
                     reference base))
                  (ansible-intelligence-jinja-analysis-references analysis)))
         (definitions
          (mapcar
           (lambda (symbol)
             (ansible-intelligence-jinja--rebase-symbol symbol base))
           (ansible-intelligence-jinja-analysis-definitions analysis))))
    (list :region
          (ansible-intelligence-jinja-region-create
           :kind (plist-get region :kind)
           :range range
           :content-range (plist-get region :content-range)
           :scalar-range (plist-get region :scalar-range)
           :text (plist-get region :text)
           :key (plist-get region :key)
           :analysis analysis
           :tree (ansible-intelligence-jinja--copy-tree-rebase
                  (ansible-intelligence-jinja-analysis-tree analysis) base)
           :references references :definitions definitions
           :complete (ansible-intelligence-jinja-analysis-complete analysis)
           :error (car (ansible-intelligence-jinja-analysis-errors analysis)))
          :references references
          :definitions definitions)))

(defun ansible-intelligence-jinja--region-kind (region)
  "Return the kind of REGION."
  (ansible-intelligence-jinja-region-kind region))

(defun ansible-intelligence-jinja--scope-definitions (regions)
  "Adjust Jinja definition visibility across REGIONS."
  (let ((stack nil))
    (dolist (region regions)
      (when (eq (ansible-intelligence-jinja-region-kind region) 'statement)
        (let ((text (ansible-intelligence-jinja-region-text region))
              (start (ansible-intelligence-range-start
                      (ansible-intelligence-jinja-region-range region)))
              (end (ansible-intelligence-range-end
                    (ansible-intelligence-jinja-region-range region))))
          (cond
           ((string-match "\\_<for\\_>" text)
            (push (cons 'for region) stack)
            (dolist (symbol (ansible-intelligence-jinja-region-definitions region))
              (setf (ansible-intelligence-symbol-visibility-start symbol) end)))
           ((string-match "\\_<endfor\\_>" text)
            (when-let* ((open (seq-find (lambda (entry) (eq (car entry) 'for))
                                       stack)))
              (setq stack (delq open stack))
              (let ((close-start start))
                (dolist (symbol
                         (ansible-intelligence-jinja-region-definitions
                          (cdr open)))
                  (setf (ansible-intelligence-symbol-visibility-end symbol)
                        close-start)))))
           ((string-match "\\_<set\\_>" text)
            (dolist (symbol (ansible-intelligence-jinja-region-definitions region))
              (setf (ansible-intelligence-symbol-visibility-start symbol) end)))))))
    regions))

(defun ansible-intelligence-jinja-analyze-buffer (&optional yaml-tree cache)
  "Analyze Jinja regions embedded in YAML scalars.

YAML-TREE is an existing YAML tree-sitter root when available.  CACHE is a
buffer-local hash table of region-relative analyses.  The returned snapshot
contains absolute buffer ranges and no live tree-sitter nodes."
(let ((scalar-ranges
       (sort (ansible-intelligence-jinja--scalar-ranges yaml-tree)
             (lambda (left right)
               (< (ansible-intelligence-range-start (cdr left))
                  (ansible-intelligence-range-start (cdr right))))))
        (scalar-count 0)
        (region-count 0)
        (bytes 0)
        (work 0)
        (cache-hits 0)
        (cache-misses 0)
        (incomplete nil)
        errors
        regions
        references
        definitions)
    (cl-labels
        ((consume-region
          (region-data)
          (when (>= region-count ansible-intelligence-jinja-max-regions)
            (setq incomplete t)
            (push "Jinja region limit reached; analysis is partial." errors)
            (throw 'limit nil))
          (pcase-let ((`(,analysis . ,hit)
                        (ansible-intelligence-jinja--analysis-for
                         region-data cache)))
            (if hit
                (setq cache-hits (1+ cache-hits))
              (setq cache-misses (1+ cache-misses)))
            (let ((materialized
                   (ansible-intelligence-jinja--region-from
                    region-data analysis)))
              (push (plist-get materialized :region) regions)
              (setq references
                    (append (plist-get materialized :references)
                            references)
                    definitions
                    (append (plist-get materialized :definitions)
                            definitions)
                    region-count (1+ region-count)
                    work (+ work
                            (ansible-intelligence-jinja-analysis-work
                             analysis)))
              (when (or
                     (not
                      (ansible-intelligence-jinja-analysis-complete
                       analysis))
                     (ansible-intelligence-jinja-analysis-errors
                      analysis))
                (setq incomplete t
                      errors
                      (append
                       (ansible-intelligence-jinja-analysis-errors
                        analysis)
                       errors)))))))
      (catch 'limit
        (dolist (scalar-entry scalar-ranges)
          (when (>= scalar-count ansible-intelligence-jinja-max-scalars)
            (setq incomplete t)
            (push "Jinja scalar limit reached; analysis is partial." errors)
            (throw 'limit nil))
          (let* ((scalar-range (cdr scalar-entry))
                 (scalar-text
                  (buffer-substring-no-properties
                   (ansible-intelligence-range-start scalar-range)
                   (ansible-intelligence-range-end scalar-range)))
                 (scalar-bytes (string-bytes scalar-text)))
            (setq scalar-count (1+ scalar-count)
                  bytes (+ bytes scalar-bytes))
            (when (> scalar-bytes ansible-intelligence-jinja-max-scalar-bytes)
              (setq incomplete t)
              (push "Jinja scalar byte limit reached; scalar skipped." errors))
            (when (> bytes ansible-intelligence-jinja-max-total-bytes)
              (setq incomplete t)
              (push "Jinja total byte limit reached; analysis is partial." errors)
              (throw 'limit nil))
            (when (<= scalar-bytes ansible-intelligence-jinja-max-scalar-bytes)
              (dolist (region-data
                       (ansible-intelligence-jinja--region-list scalar-range))
                (consume-region region-data)))))
        ;; Bare task expressions use synthetic wrappers but retain their YAML
        ;; scalar ranges.  They are deliberately considered after delimiter
        ;; regions so a normal `{{ ... }}` scalar is never double-counted.
        (dolist (region-data
                 (ansible-intelligence-jinja--bare-region-data yaml-tree))
          (consume-region region-data))))
    (setq regions
          (ansible-intelligence-jinja--scope-definitions
           (sort regions
                 (lambda (left right)
                   (<
                    (ansible-intelligence-range-start
                     (ansible-intelligence-jinja-region-range left))
                    (ansible-intelligence-range-start
                     (ansible-intelligence-jinja-region-range right)))))))
    (ansible-intelligence-jinja-snapshot-create
     :parser (or (ansible-intelligence-jinja--language) 'fallback)
     :regions regions
     :references
     (sort references
           (lambda (left right)
             (<
              (ansible-intelligence-range-start
               (ansible-intelligence-reference-range left))
              (ansible-intelligence-range-start
               (ansible-intelligence-reference-range right)))))
     :definitions
     (sort definitions
           (lambda (left right)
             (<
              (ansible-intelligence-range-start
               (ansible-intelligence-symbol-range left))
              (ansible-intelligence-range-start
               (ansible-intelligence-symbol-range right)))))
     :errors (delete-dups (nreverse errors))
     :incomplete incomplete
     :scalar-count scalar-count
     :region-count region-count
     :bytes bytes
     :work work
     :cache-hits cache-hits
     :cache-misses cache-misses)))

(defun ansible-intelligence-jinja--region-at (snapshot position)
  "Return the Jinja region in SNAPSHOT containing POSITION."
  (seq-find
   (lambda (region)
     (let ((range (ansible-intelligence-jinja-region-range region)))
       (and (<= (ansible-intelligence-range-start range) position)
            (<= position (ansible-intelligence-range-end range)))))
   (and snapshot (ansible-intelligence-jinja-snapshot-regions snapshot))))

(defun ansible-intelligence-jinja--identifier-backward (text position)
  "Return `(START . END)' for the identifier before POSITION in TEXT."
  (let ((end position))
    (while (and (> position 0)
                (ansible-intelligence-jinja--identifier-char-p
                 (aref text (1- position))))
      (setq position (1- position)))
    (and (< position end) (cons position end))))

(defun ansible-intelligence-jinja--bracket-backward (text position)
  "Return a quoted field before POSITION, or nil."
  (let ((cursor (ansible-intelligence-jinja--skip-space-backward text position)))
    (when (and (> cursor 0) (= (aref text (1- cursor)) ?\]))
      (let* ((close-position (1- cursor))
             (open (cl-position ?\[ text :from-end t :end close-position)))
        (when open
          (let ((quote-pos (1+ open)))
            (while (and (< quote-pos close-position)
                        (memq (aref text quote-pos) '(?\s ?\t ?\n ?\r)))
              (setq quote-pos (1+ quote-pos)))
            (when (and (< quote-pos close-position)
                       (memq (aref text quote-pos) '(?\" ?\')))
              (let* ((quote-char (aref text quote-pos))
                     (field-start (1+ quote-pos))
                     (field-end (cl-position quote-char text
                                              :start field-start)))
                (when (and field-end (< field-end close-position))
                  (list (substring text field-start field-end)
                        field-start field-end open))))))))))

(defun ansible-intelligence-jinja--incomplete-bracket (text)
  "Return `(PREFIX START OPEN)' for an unfinished quoted bracket field."
  (when-let* ((open (cl-position ?\[ text :from-end t)))
    (let ((cursor (1+ open)))
      (while (and (< cursor (length text))
                  (memq (aref text cursor) '(?\s ?\t ?\n ?\r)))
        (setq cursor (1+ cursor)))
      (when (and (< cursor (length text))
                 (memq (aref text cursor) '(?\" ?\')))
        (let* ((quote-char (aref text cursor))
               (start (1+ cursor))
               (close (cl-position quote-char text :start start)))
          (when (or (null close)
                    (not (string-match-p "\\]" (substring text close))))
            (list (substring text start (or close (length text)))
                  start open)))))))

(defun ansible-intelligence-jinja-completion-context (snapshot position)
  "Return a snapshot-only field context at POSITION, or nil.

The returned plist has `:base', `:path', `:start', `:end', and `:field-p'.
No tree-sitter parser is invoked here."
  (when-let* ((region (ansible-intelligence-jinja--region-at snapshot position))
              (content-range
               (ansible-intelligence-jinja-region-content-range region))
              (region-range (ansible-intelligence-jinja-region-range region)))
    (let* ((region-start (ansible-intelligence-range-start region-range))
           (text (ansible-intelligence-jinja-region-text region))
           (relative-position
            (max 0 (min (length text) (- position region-start))))
           (before (substring text 0 relative-position))
           (identifier (ansible-intelligence-jinja--identifier-backward
                        before (length before)))
           (empty-cursor
            (ansible-intelligence-jinja--skip-space-backward
             before (length before)))
           (bracket-prefix
            (ansible-intelligence-jinja--incomplete-bracket
             (substring before 0 empty-cursor)))
           (prefix-start
            (cond
             (bracket-prefix (nth 1 bracket-prefix))
             (identifier (car identifier))
             (t empty-cursor)))
           (prefix
            (cond
             (bracket-prefix (car bracket-prefix))
             (identifier (substring before (car identifier) (cdr identifier)))
             (t "")))
           (cursor (if bracket-prefix (nth 2 bracket-prefix) prefix-start))
           previous
           (field-p (and bracket-prefix t)))
      (when (or identifier bracket-prefix
                (and (> cursor 0)
                     (memq (aref before (1- cursor)) '(?. ?\] ?\[))))
        (when bracket-prefix
          (when-let* ((part
                      (ansible-intelligence-jinja--identifier-backward
                       before cursor)))
            (push (substring before (car part) (cdr part)) previous)
            (setq cursor (car part))))
        (let ((first-pass (not bracket-prefix)))
          (while (and (> cursor 0)
                      (or first-pass
                          (eq (aref before (1- cursor)) ?.)))
            (setq first-pass nil
                  cursor (ansible-intelligence-jinja--skip-space-backward
                          before cursor))
            (cond
             ((and (> cursor 0) (= (aref before (1- cursor)) ?.))
              (setq field-p t
                    cursor (1- cursor))
              (setq cursor (ansible-intelligence-jinja--skip-space-backward
                            before cursor))
              (if-let* ((bracket
                        (ansible-intelligence-jinja--bracket-backward
                         before cursor)))
                  (progn
                    (push (car bracket) previous)
                    (setq cursor (nth 3 bracket))
                    (when-let* ((part
                                (ansible-intelligence-jinja--identifier-backward
                                 before cursor)))
                      (push (substring before (car part) (cdr part)) previous)
                      (setq cursor (car part))))
                (when-let* ((part
                            (ansible-intelligence-jinja--identifier-backward
                             before cursor)))
                  (push (substring before (car part) (cdr part)) previous)
                  (setq cursor (car part)))))
             ((and (> cursor 0) (= (aref before (1- cursor)) ?\]))
              (setq field-p t)
              (when-let* ((bracket
                          (ansible-intelligence-jinja--bracket-backward
                           before cursor)))
                (push (car bracket) previous)
                 (setq cursor (nth 3 bracket))
                 (when-let* ((part
                             (ansible-intelligence-jinja--identifier-backward
                              before cursor)))
                   (push (substring before (car part) (cdr part)) previous)
                   (setq cursor (car part)))))
             (t (setq cursor 0))))
          (let* ((parts (append previous
                                 (unless (string-empty-p prefix)
                                   (list prefix))))
                 (base (car parts))
                 (path (and base
                            (if (string-empty-p prefix)
                                (cdr parts)
                              (butlast (cdr parts))))))
            (when base
              (list :base base :path path
                    :start (+ region-start prefix-start)
                    :end position :field-p field-p
                    :bracket (or bracket-prefix
                                 (and (> prefix-start 0)
                                      (= (aref before (1- prefix-start)) ?\[)))
                    :region region
                    :content-range content-range))))))))

(defun ansible-intelligence-jinja-root-name-at-point (snapshot position)
  "Return the Jinja root identifier at POSITION from SNAPSHOT."
  (plist-get (ansible-intelligence-jinja-completion-context
              snapshot position)
             :base))

(provide 'ansible-intelligence-jinja)

;;; ansible-intelligence-jinja.el ends here
