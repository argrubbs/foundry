;;; ide-sql-grid.el --- Editable data grids for SQL -*- lexical-binding: t; -*-

;;; Commentary:
;; An interactive, editable table UI built on Emacs 29's `vtable' for SQL results.
;; Intercepts `ide-sql--show' when the mode is `table'.
;;
;; Features:
;; - Consumes cleaned CSV from `ide-sql--csv-filter' and converts it into plists for `vtable'.
;; - Primary key inference heuristic (id, uuid, *_id) to anchor rows.
;; - AST table name extraction via `treesit' with robust heuristic fallback.
;; - Full grid navigation (h/j/k/l, arrows).
;; - Cell editing via RET or double-click with minibuffer prompt.
;; - Visual feedback: modified cells are highlighted with `font-lock-warning-face'.
;; - Mutation ledger tracking row PK, column name, original value, and new value.
;; - Sandbox-compliant caching of pending mutations in `var/sql-grid/'.
;; - C-c C-c commits pending mutations via UPDATE statements using `ide-sql--spawn'.
;; - C-c C-k rolls back pending mutations and restores the original table.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vtable)
(require 'lang-sql)

(defvar ide-var-dir)
(defvar ide-sql--result-conn)
(defvar ide-sql--result-sql)
(defvar ide-sql--result-mode)
(defvar ide-sql-result-buffer)
(defvar treesit-language-source-alist)

(declare-function ide-sql--product "lang-sql" (&optional conn))
(declare-function ide-sql--csv-filter "lang-sql" (conn text))
(declare-function ide-sql--spawn "lang-sql" (conn sql mode callback))
(declare-function ide-sql--history-add "lang-sql" (sql name))
(declare-function ide-sql-result-rerun "lang-sql" ())
(declare-function ide-sql-result-toggle-csv "lang-sql" ())
(declare-function ide-sql-copy-csv "lang-sql" ())
(declare-function ide-sql-export-csv "lang-sql" (file))
(declare-function ide-sql--ensure-connection "lang-sql" ())
(declare-function treesit-parser-create "treesit" (language &optional buffer-or-string))
(declare-function treesit-parser-root-node "treesit" (parser))
(declare-function treesit-node-type "treesit" (node))
(declare-function treesit-node-text "treesit" (node &optional no-property))
(declare-function treesit-node-children "treesit" (node &optional named))
(declare-function treesit-query-capture "treesit" (node query &optional beg end node-only))
(declare-function treesit-language-available-p "treesit" (language &optional detail))

;;;; Data Structures & State --------------------------------------------------------------------------

(cl-defstruct (ide-sql-mutation
               (:constructor ide-sql-mutation-create))
  "A pending cell modification in the data grid."
  row-pk-val
  column-name
  original-value
  new-value)

(defvar-local ide-sql-grid--headers nil
  "List of column header strings in the current grid.")

(defvar-local ide-sql-grid--table-name nil
  "Target table name inferred from the generating SQL query.")

(defvar-local ide-sql-grid--pk-column nil
  "Primary key column name inferred for the current grid.")

(defvar-local ide-sql-grid--original-objects nil
  "List of original row plists as received from the query.")

(defvar-local ide-sql-grid--mutations nil
  "Buffer-local list of `ide-sql-mutation' structs.")

(defvar-local ide-sql-grid--table nil
  "The current `vtable' instance.")

;;;; Sandbox Cache (var/sql-grid/) --------------------------------------------------------------------

(defun ide-sql-grid--cache-dir ()
  "Directory in the sandbox for caching pending mutations."
  (let ((dir (expand-file-name "sql-grid/" ide-var-dir)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun ide-sql-grid--cache-file ()
  "Cache file path for the current connection and query."
  (when (and (bound-and-true-p ide-sql--result-conn)
             (bound-and-true-p ide-sql--result-sql))
    (let ((hash (md5 (concat (car ide-sql--result-conn) ":" ide-sql--result-sql))))
      (expand-file-name (format "%s.eld" hash) (ide-sql-grid--cache-dir)))))

(defun ide-sql-grid--save-cache ()
  "Persist pending mutations to sandbox cache in `var/sql-grid/'."
  (when-let* ((file (ide-sql-grid--cache-file)))
    (if (null ide-sql-grid--mutations)
        (when (file-exists-p file)
          (delete-file file))
      (with-temp-file file
        (let ((records
               (mapcar (lambda (m)
                         (list :row-pk-val (ide-sql-mutation-row-pk-val m)
                               :column-name (ide-sql-mutation-column-name m)
                               :original-value (ide-sql-mutation-original-value m)
                               :new-value (ide-sql-mutation-new-value m)))
                       ide-sql-grid--mutations)))
          (prin1 (list :conn (car ide-sql--result-conn)
                       :sql ide-sql--result-sql
                       :table ide-sql-grid--table-name
                       :pk ide-sql-grid--pk-column
                       :mutations records
                       :timestamp (float-time))
                 (current-buffer)))))))

(defun ide-sql-grid--delete-cache ()
  "Delete sandbox cache file for the current query."
  (when-let* ((file (ide-sql-grid--cache-file)))
    (when (file-exists-p file)
      (delete-file file))))

;;;; CSV Parsing --------------------------------------------------------------------------------------

(defun ide-sql-grid-parse-csv (text)
  "Parse RFC 4180 CSV TEXT into (HEADERS . PLISTS).
HEADERS is a list of column name strings.
PLISTS is a list of plists, one per data row, with keyword keys matching HEADERS."
  (let ((i 0)
        (len (length text))
        (rows nil)
        (current-row nil)
        (current-field nil)
        (in-quotes nil))
    (while (< i len)
      (let ((c (aref text i)))
        (cond
         ((= c ?\")
          (if in-quotes
              (if (and (< (1+ i) len) (= (aref text (1+ i)) ?\"))
                  (progn
                    (push ?\" current-field)
                    (setq i (1+ i)))
                (setq in-quotes nil))
            (setq in-quotes t)))
         ((and (= c ?,) (not in-quotes))
          (push (concat (nreverse current-field)) current-row)
          (setq current-field nil))
         ((and (= c ?\n) (not in-quotes))
          (push (concat (nreverse current-field)) current-row)
          (setq current-field nil)
          (push (nreverse current-row) rows)
          (setq current-row nil))
         ((and (= c ?\r) (not in-quotes))
          ;; Ignore carriage returns outside quotes
          nil)
         (t
          (push c current-field))))
      (setq i (1+ i)))
    (when (or current-field current-row)
      (push (concat (nreverse current-field)) current-row)
      (push (nreverse current-row) rows))
    (let* ((all (nreverse rows))
           (headers (car all))
           (data (cdr all)))
      (if (null headers)
          (cons nil nil)
        (let ((plists
               (mapcar (lambda (row)
                         (let (pl)
                           (cl-mapc (lambda (h val)
                                      (setq pl (plist-put pl (intern (concat ":" h)) (or val ""))))
                                    headers row)
                           pl))
                       data)))
          (cons headers plists))))))

;;;; Primary Key Inference & AST Extraction -----------------------------------------------------------

(defun ide-sql-grid--infer-primary-key (columns &optional table-name)
  "Infer the primary key column name from COLUMNS and optional TABLE-NAME.
Checks for exact `id', exact `uuid'/`guid', `TABLE_id', or any column ending in `_id'."
  (or
   ;; 1. Exact match for "id"
   (cl-find-if (lambda (c) (string-equal-ignore-case c "id")) columns)
   ;; 2. Exact match for "uuid" or "guid"
   (cl-find-if (lambda (c) (or (string-equal-ignore-case c "uuid")
                               (string-equal-ignore-case c "guid")))
               columns)
   ;; 3. If table name given, match <table-name>_id or <singular>_id
   (when table-name
     (let* ((tbl (downcase table-name))
            (cand1 (concat tbl "_id"))
            (cand2 (when (string-suffix-p "s" tbl)
                     (concat (substring tbl 0 -1) "_id"))))
       (or (cl-find-if (lambda (c) (string-equal-ignore-case c cand1)) columns)
           (and cand2 (cl-find-if (lambda (c) (string-equal-ignore-case c cand2)) columns)))))
   ;; 4. Any column ending with "_id"
   (cl-find-if (lambda (c) (string-match-p "_id\\'" (downcase c))) columns)
   ;; 5. Fallback: first column
   (car columns)))

(defun ide-sql-grid--clean-identifier (ident)
  "Remove SQL quoting characters (double quotes, backticks, brackets) from IDENT."
  (if (stringp ident)
      (let ((s (string-trim ident)))
        (while (and (> (length s) 0)
                    (memq (aref s 0) (list ?\" ?\` ?\[ ?\')))
          (setq s (substring s 1)))
        (while (and (> (length s) 0)
                    (memq (aref s (1- (length s))) (list ?\" ?\` ?\] ?\')))
          (setq s (substring s 0 -1)))
        s)
    ident))

(defun ide-sql-grid--treesit-find-table (node)
  "Recursively search tree-sitter NODE for a table identifier within a FROM clause."
  (when node
    (let ((type (treesit-node-type node)))
      (cond
       ((string-match-p "\\`\\(?:from_clause\\|from\\)\\'" type)
        (or (ide-sql-grid--find-identifier-in-node node)
            (ide-sql-grid--clean-identifier (treesit-node-text node))))
       (t
        (cl-some #'ide-sql-grid--treesit-find-table
                 (treesit-node-children node)))))))

(defun ide-sql-grid--find-identifier-in-node (node)
  "Find the first identifier child in NODE."
  (when node
    (if (string-match-p "\\`\\(?:identifier\\|relation\\|table_reference\\|object_reference\\)\\'"
                        (treesit-node-type node))
        (ide-sql-grid--clean-identifier (treesit-node-text node))
      (cl-some #'ide-sql-grid--find-identifier-in-node
               (treesit-node-children node)))))

(defun ide-sql-grid--extract-table-treesit (sql)
  "Use `treesit' from `modules/05-treesitter.el' to extract target table name from SQL."
  (when (and (fboundp 'treesit-language-available-p)
             (treesit-language-available-p 'sql))
    (ignore-errors
      (with-temp-buffer
        (insert sql)
        (let* ((parser (treesit-parser-create 'sql))
               (root (treesit-parser-root-node parser))
               (query "((from_clause (relation (identifier) @table)))
                       ((from_clause (table_reference (identifier) @table)))
                       ((from_clause (identifier) @table))")
               (captures (ignore-errors (treesit-query-capture root query))))
          (if captures
              (ide-sql-grid--clean-identifier (treesit-node-text (cdr (car captures))))
            (ide-sql-grid--treesit-find-table root)))))))

(defun ide-sql-grid--extract-table-heuristic (sql)
  "Extract target table name from SQL statement using regex heuristics."
  (when (string-match "\\<from[ \t\n\r]+\\([]\"`[]?[a-zA-Z0-9_]+[]\"`[]?\\(?:\\.[*]?[]\"`[]?[a-zA-Z0-9_]+[]\"`[]?\\)?\\)"
                      (downcase sql))
    (let ((raw (substring sql (match-beginning 1) (match-end 1))))
      (ide-sql-grid--clean-identifier raw))))

(defun ide-sql-grid--extract-table-name (sql)
  "Extract the target table name from SQL query."
  (or (ide-sql-grid--extract-table-treesit sql)
      (ide-sql-grid--extract-table-heuristic sql)))

;;;; Value Quoting & Escaping -------------------------------------------------------------------------

(defun ide-sql-grid--quote-value (val)
  "Format VAL for SQL UPDATE statements."
  (cond
   ((null val) "NULL")
   ((string-empty-p val) "''")
   ((string-equal-ignore-case val "null") "NULL")
   ((string-match-p "\\`[-+]?[0-9]+\\(?:\\.[0-9]+\\)?\\'" val)
    val)
   (t
    (concat "'" (replace-regexp-in-string "'" "''" val) "'"))))

;;;; vtable Rendering & Formatting --------------------------------------------------------------------

(defun ide-sql-grid--getter (obj idx table)
  "Compute cell value for OBJ at column index IDX in TABLE.
If the cell has pending mutations, propertizes the string with `font-lock-warning-face'."
  (let* ((col (elt (vtable-columns table) idx))
         (col-name (vtable-column-name col))
         (val (or (plist-get obj (intern (concat ":" col-name))) ""))
         (is-modified (plist-get obj (intern (concat ":" col-name "--modified")))))
    (if is-modified
        (propertize (copy-sequence val) 'face 'font-lock-warning-face)
      val)))

(defun ide-sql-grid--render-vtable (headers plists)
  "Render an interactive vtable in the current buffer for HEADERS and PLISTS."
  (let* ((cols (seq-map-indexed
                (lambda (h idx)
                  (let ((maxw (apply #'max
                                     (length h)
                                     (mapcar (lambda (p)
                                               (length (or (plist-get p (intern (concat ":" h))) "")))
                                             plists))))
                    (list :name h
                          :min-width (+ maxw (if (zerop idx) 2 0))
                          :formatter (if (zerop idx)
                                         (lambda (v) (format "│ %s" v))
                                       nil))))
                headers))
         (table (make-vtable
                 :columns cols
                 :objects plists
                 :getter #'ide-sql-grid--getter
                 :divider "│ "
                 :separator-width 1
                 :use-header-line nil)))
    (setq ide-sql-grid--table table)
    table))

;;;; Editing & Mutation Commands ----------------------------------------------------------------------

(defun ide-sql-grid-edit-cell ()
  "Read the cell at point, prompt for new input, update the vtable object,
apply the modified face, and push to the mutation ledger."
  (interactive)
  (let* ((table (or (vtable-current-table) ide-sql-grid--table))
         (obj (vtable-current-object))
         (col-idx (vtable-current-column)))
    (unless (and table obj col-idx)
      (user-error "Not on a table cell"))
    (let* ((col (elt (vtable-columns table) col-idx))
           (col-name (vtable-column-name col))
           (current-val (or (plist-get obj (intern (concat ":" col-name))) ""))
           (new-val (read-string (format "Edit %s: " col-name) current-val)))
      (unless (equal new-val current-val)
        (let* ((pk-col (or ide-sql-grid--pk-column "id"))
               (pk-val (or (plist-get obj (intern (concat ":" pk-col)))
                           (plist-get obj :id)))
               ;; Retrieve the original unedited value for this cell
               (orig-obj (cl-find-if
                          (lambda (o)
                            (equal (or (plist-get o (intern (concat ":" pk-col)))
                                       (plist-get o :id))
                                   pk-val))
                          ide-sql-grid--original-objects))
               (true-orig-val (if orig-obj
                                  (or (plist-get orig-obj (intern (concat ":" col-name))) "")
                                current-val))
               (is-restored (equal new-val true-orig-val)))
          ;; Update or prune mutation ledger
          (if is-restored
              (setq ide-sql-grid--mutations
                    (cl-remove-if (lambda (m)
                                    (and (equal (ide-sql-mutation-row-pk-val m) pk-val)
                                         (equal (ide-sql-mutation-column-name m) col-name)))
                                  ide-sql-grid--mutations))
            (let ((existing (cl-find-if
                             (lambda (m)
                               (and (equal (ide-sql-mutation-row-pk-val m) pk-val)
                                    (equal (ide-sql-mutation-column-name m) col-name)))
                             ide-sql-grid--mutations)))
              (if existing
                  (setf (ide-sql-mutation-new-value existing) new-val)
                (push (ide-sql-mutation-create
                       :row-pk-val pk-val
                       :column-name col-name
                       :original-value true-orig-val
                       :new-value new-val)
                      ide-sql-grid--mutations))))
          ;; Sandbox compliance: cache ledger
          (ide-sql-grid--save-cache)
          ;; Update vtable object
          (let ((updated-obj (copy-sequence obj)))
            (setq updated-obj (plist-put updated-obj (intern (concat ":" col-name)) new-val))
            (setq updated-obj (plist-put updated-obj
                                         (intern (concat ":" col-name "--modified"))
                                         (not is-restored)))
            (vtable-update-object table updated-obj obj)
            ;; Expand column width if needed
            (let ((cur-width (elt (vtable--cache-widths (vtable--current-cache table)) col-idx))
                  (new-width (+ (length new-val) (if (zerop col-idx) 2 0) 1)))
              (when (> new-width cur-width)
                (vtable--alter-column-width table col-idx (- new-width cur-width))))
            ;; Restore point to edited cell
            (vtable-goto-object updated-obj)
            (vtable-goto-column col-idx)
            (message "Updated %s = %s (%d pending mutation%s - C-c C-c commit, C-c C-k rollback)"
                     col-name new-val
                     (length ide-sql-grid--mutations)
                     (if (= 1 (length ide-sql-grid--mutations)) "" "s"))))))))

(defun ide-sql-grid-mouse-edit-cell (event)
  "Edit the cell clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (ide-sql-grid-edit-cell))

(defun ide-sql-grid-commit (&optional callback)
  "Commit pending mutations: iterate over the mutation ledger, construct
`UPDATE <table> SET <col> = <new> WHERE id = <pk>;' strings, and send them
to the active connection via `ide-sql--spawn'.
Optional CALLBACK is called with EXIT-STATUS when the process completes."
  (interactive)
  (unless ide-sql-grid--mutations
    (user-error "No pending mutations to commit"))
  (let* ((conn (or (bound-and-true-p ide-sql--result-conn)
                   (ide-sql--ensure-connection)))
         (table (or ide-sql-grid--table-name
                    (read-string "Target table name: ")))
         (pk-col (or ide-sql-grid--pk-column "id"))
         (statements
          (mapcar
           (lambda (m)
             (format "UPDATE %s SET %s = %s WHERE %s = %s;"
                     table
                     (ide-sql-mutation-column-name m)
                     (ide-sql-grid--quote-value (ide-sql-mutation-new-value m))
                     pk-col
                     (ide-sql-grid--quote-value (ide-sql-mutation-row-pk-val m))))
           (reverse ide-sql-grid--mutations)))
         (sql-batch (string-join statements "\n"))
         (count (length ide-sql-grid--mutations))
         (buf (current-buffer)))
    (when (or (not (called-interactively-p 'any))
              (yes-or-no-p (format "Execute %d UPDATE statement%s on %s? "
                                   count (if (= 1 count) "" "s") (car conn))))
      (message "Executing %d update%s..." count (if (= 1 count) "" "s"))
      (ide-sql--spawn
       conn sql-batch 'table
       (lambda (text status secs)
         (if (zerop status)
             (progn
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (setq ide-sql-grid--mutations nil)
                   (ide-sql-grid--delete-cache)
                   (ide-sql-result-rerun)))
               (message "Committed %d mutation%s successfully (%.2fs)"
                        count (if (= 1 count) "" "s") secs)
               (when callback (funcall callback status)))
           (message "Commit failed (exit %d): %s" status (string-trim text))
           (when callback (funcall callback status))))))))

(defun ide-sql-grid-rollback ()
  "Discard all pending cell mutations and redraw the original vtable."
  (interactive)
  (if (null ide-sql-grid--mutations)
      (message "No pending mutations to roll back")
    (when (or (not (called-interactively-p 'any))
              (y-or-n-p (format "Discard %d pending mutation%s? "
                                (length ide-sql-grid--mutations)
                                (if (= 1 (length ide-sql-grid--mutations)) "" "s"))))
      (setq ide-sql-grid--mutations nil)
      (ide-sql-grid--delete-cache)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ide-sql-grid--render-vtable ide-sql-grid--headers ide-sql-grid--original-objects))
      (goto-char (point-min))
      (message "Rolled back all mutations"))))

;;;; Navigation Commands ------------------------------------------------------------------------------

(defun ide-sql-grid-next-row (&optional n)
  "Move down N rows in the SQL grid, preserving the current column."
  (interactive "p")
  (let ((col (or (vtable-current-column) 0)))
    (forward-line (or n 1))
    (when (vtable-current-object)
      (vtable-goto-column col))))

(defun ide-sql-grid-previous-row (&optional n)
  "Move up N rows in the SQL grid, preserving the current column."
  (interactive "p")
  (let ((col (or (vtable-current-column) 0)))
    (forward-line (- (or n 1)))
    (when (vtable-current-object)
      (vtable-goto-column col))))

(defun ide-sql-grid-next-column ()
  "Move to the next column in the grid."
  (interactive)
  (vtable-next-column))

(defun ide-sql-grid-previous-column ()
  "Move to the previous column in the grid."
  (interactive)
  (vtable-previous-column))

;;;; Major Mode & Keymap ------------------------------------------------------------------------------

(defvar-keymap ide-sql-grid-mode-map
  :doc "Keymap for interactive editable SQL data grids."
  :parent special-mode-map
  ;; Navigation
  "h" #'ide-sql-grid-previous-column
  "<left>" #'ide-sql-grid-previous-column
  "l" #'ide-sql-grid-next-column
  "<right>" #'ide-sql-grid-next-column
  "j" #'ide-sql-grid-next-row
  "<down>" #'ide-sql-grid-next-row
  "k" #'ide-sql-grid-previous-row
  "<up>" #'ide-sql-grid-previous-row
  ;; Editing & Transactions
  "RET" #'ide-sql-grid-edit-cell
  "<double-mouse-1>" #'ide-sql-grid-mouse-edit-cell
  "<mouse-2>" #'ide-sql-grid-mouse-edit-cell
  "C-c C-c" #'ide-sql-grid-commit
  "C-c C-k" #'ide-sql-grid-rollback
  ;; Foundry SQL actions
  "g" #'ide-sql-result-rerun
  "t" #'ide-sql-result-toggle-csv
  "w" #'ide-sql-copy-csv
  "e" #'ide-sql-export-csv)

(define-derived-mode ide-sql-grid-mode special-mode "SQL-Grid"
  "Interactive editable data grid for SQL results.
\\{ide-sql-grid-mode-map}"
  (setq-local truncate-lines t))

;;;; Integration: Intercept `ide-sql--show' -----------------------------------------------------------

(defun ide-sql-grid--handle-spawn-result (conn sql text status secs)
  "Handle result of `ide-sql--spawn' in current buffer and render the grid or error."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (/= 0 status)
        (progn
          (insert text)
          (goto-char (point-min))
          (setq header-line-format
                (format " %s [%s]  %.2fs  FAILED (exit %d)   g rerun  t table/csv"
                        (car conn) (ide-sql--product conn) secs status)))
      ;; Successful execution
      (let* ((csv (ide-sql--csv-filter conn text))
             (parsed (ide-sql-grid-parse-csv csv))
             (headers (car parsed))
             (plists (cdr parsed)))
        (if (null headers)
            (progn
              (insert (if (string-blank-p text)
                          "-- Statement executed successfully (no rows returned)\n"
                        text))
              (goto-char (point-min))
              (setq header-line-format
                    (format " %s [%s]  %.2fs   g rerun  t table/csv"
                            (car conn) (ide-sql--product conn) secs)))
          ;; Render interactive vtable grid
          (let* ((table-name (ide-sql-grid--extract-table-name sql))
                 (pk-col (ide-sql-grid--infer-primary-key headers table-name)))
            (setq ide-sql-grid--headers headers
                  ide-sql-grid--table-name table-name
                  ide-sql-grid--pk-column pk-col
                  ide-sql-grid--original-objects plists
                  ide-sql-grid--mutations nil)
            (ide-sql-grid--render-vtable headers plists)
            (goto-char (point-min))
            (setq header-line-format
                  (format " %s [%s]  %.2fs%s   RET edit  C-c C-c commit  C-c C-k rollback  g rerun  t table/csv"
                          (car conn) (ide-sql--product conn) secs
                          (if table-name (format "  [%s]" table-name) "")))))))))

(defun ide-sql-grid-show (conn sql)
  "Run SQL on CONN in CSV mode and display the interactive editable grid."
  (let ((buf (get-buffer-create ide-sql-result-buffer)))
    (with-current-buffer buf
      (ide-sql-grid-mode)
      (setq ide-sql--result-conn conn
            ide-sql--result-sql sql
            ide-sql--result-mode 'table)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-- %s (%s)\n" (car conn) (ide-sql--product conn))
                "-- running...\n")))
    (display-buffer buf)
    (ide-sql--history-add sql (car conn))
    (ide-sql--spawn
     conn sql 'csv
     (lambda (text status secs)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (ide-sql-grid--handle-spawn-result conn sql text status secs)))))))

(defun ide-sql-grid--show-around (orig-fun conn sql mode)
  "Around advice intercepting `ide-sql--show' when MODE is `table'."
  (if (eq mode 'table)
      (ide-sql-grid-show conn sql)
    (funcall orig-fun conn sql mode)))

(advice-add 'ide-sql--show :around #'ide-sql-grid--show-around)

(provide 'ide-sql-grid)
;;; ide-sql-grid.el ends here
