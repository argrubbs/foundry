;;; lang-sql-grid-test.el --- Tests for ide-sql-grid.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for `ide-sql-grid.el':
;; - CSV parsing to plists
;; - Primary key inference heuristics
;; - AST and heuristic target table extraction
;; - Grid navigation (h/j/k/l, arrows)
;; - Cell editing, mutation ledger, and visual warning face
;; - Sandbox caching in `var/sql-grid/'
;; - Rollback discarding changes
;; - Real SQLite update transaction via commit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ide-sql-grid)
(require 'lang-sql)

(defmacro ide-sql-grid-test--with-db (&rest body)
  "Execute BODY with a temporary SQLite database and project connection."
  `(let* ((dir (file-name-as-directory (make-temp-file "sqlgridtest" t)))
          (default-directory dir)
          (db (expand-file-name "test.db" dir))
          (ide-var-dir dir)
          (sql-connection-alist nil)
          (ide-sql--history nil))
     (unwind-protect
         (progn
           (when (executable-find "sqlite3")
             (call-process "sqlite3" nil nil nil db
                           (concat "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT, email TEXT);"
                                   "INSERT INTO users VALUES (1, 'alice', 'alice@test.org');"
                                   "INSERT INTO users VALUES (2, 'bob', 'bob@test.org');")))
           (with-temp-file (expand-file-name ".sql-connections.eld" dir)
             (insert (format "((\"testdb\" (sql-product 'sqlite) (sql-database %S)))\n" db)))
           ,@body)
       (delete-directory dir t))))

(defun ide-sql-grid-test--wait (pred)
  "Wait up to 10 seconds for PRED to become non-nil."
  (let ((n 0))
    (while (and (not (funcall pred)) (< (cl-incf n) 100))
      (accept-process-output nil 0.1))))

;;;; 1. CSV Parser Tests -------------------------------------------------------------------------------

(ert-deftest sql-grid-csv-parser-basic ()
  "Test parsing simple CSV lines into headers and plists."
  (let* ((csv "id,name,role\n1,alice,admin\n2,bob,editor\n")
         (res (ide-sql-grid-parse-csv csv))
         (headers (car res))
         (plists (cdr res)))
    (should (equal headers '("id" "name" "role")))
    (should (= (length plists) 2))
    (should (equal (plist-get (nth 0 plists) :id) "1"))
    (should (equal (plist-get (nth 0 plists) :name) "alice"))
    (should (equal (plist-get (nth 0 plists) :role) "admin"))
    (should (equal (plist-get (nth 1 plists) :id) "2"))
    (should (equal (plist-get (nth 1 plists) :name) "bob"))))

(ert-deftest sql-grid-csv-parser-rfc4180-quotes-and-newlines ()
  "Test RFC 4180 parsing with quotes, commas, escaped quotes, and newlines."
  (let* ((csv "id,description,status\n1,\"hello, world\",\"active\"\n2,\"quotes: \"\"escaped\"\"\",pending\n3,\"multi\nline\",done\n")
         (res (ide-sql-grid-parse-csv csv))
         (headers (car res))
         (plists (cdr res)))
    (should (equal headers '("id" "description" "status")))
    (should (= (length plists) 3))
    (should (equal (plist-get (nth 0 plists) :description) "hello, world"))
    (should (equal (plist-get (nth 1 plists) :description) "quotes: \"escaped\""))
    (should (equal (plist-get (nth 2 plists) :description) "multi\nline"))
    (should (equal (plist-get (nth 2 plists) :status) "done"))))

(ert-deftest sql-grid-csv-parser-empty-fields ()
  "Test that empty fields in CSV map to empty strings."
  (let* ((csv "id,note\n1,\n2,present\n")
         (res (ide-sql-grid-parse-csv csv))
         (plists (cdr res)))
    (should (equal (plist-get (nth 0 plists) :note) ""))
    (should (equal (plist-get (nth 1 plists) :note) "present"))))

;;;; 2. Primary Key Inference Tests -------------------------------------------------------------------

(ert-deftest sql-grid-pk-inference-id ()
  "Infer PK when id column is present."
  (should (equal (ide-sql-grid--infer-primary-key '("name" "id" "created_at")) "id"))
  (should (equal (ide-sql-grid--infer-primary-key '("ID" "NAME")) "ID")))

(ert-deftest sql-grid-pk-inference-uuid ()
  "Infer PK when uuid or guid column is present."
  (should (equal (ide-sql-grid--infer-primary-key '("title" "uuid" "score")) "uuid"))
  (should (equal (ide-sql-grid--infer-primary-key '("guid" "payload")) "guid")))

(ert-deftest sql-grid-pk-inference-suffix-and-table ()
  "Infer PK with _id suffix or matching table name."
  (should (equal (ide-sql-grid--infer-primary-key '("user_id" "email")) "user_id"))
  (should (equal (ide-sql-grid--infer-primary-key '("account_id" "amount") "accounts") "account_id"))
  (should (equal (ide-sql-grid--infer-primary-key '("other" "order_id" "total")) "order_id")))

(ert-deftest sql-grid-pk-inference-fallback ()
  "Fallback to first column when no id column pattern matches."
  (should (equal (ide-sql-grid--infer-primary-key '("sku" "description" "price")) "sku")))

;;;; 3. AST Table Extraction Tests --------------------------------------------------------------------

(ert-deftest sql-grid-extract-table-simple ()
  "Extract table name from standard SELECT statements."
  (should (equal (ide-sql-grid--extract-table-name "SELECT * FROM users;") "users"))
  (should (equal (ide-sql-grid--extract-table-name "select id, name from users where id = 10") "users"))
  (should (equal (ide-sql-grid--extract-table-name "SELECT * FROM public.orders ORDER BY id DESC") "public.orders")))

(ert-deftest sql-grid-extract-table-quoted-and-aliases ()
  "Extract table name from quoted identifiers and aliases."
  (should (equal (ide-sql-grid--extract-table-name "select a, b from \"customers\" as c") "customers"))
  (should (equal (ide-sql-grid--extract-table-name "SELECT * FROM [Products] WHERE active = 1") "Products"))
  (should (equal (ide-sql-grid--extract-table-name "SELECT u.id FROM `accounts` u JOIN logins l ON u.id = l.user_id") "accounts")))

(ert-deftest sql-grid-extract-table-none ()
  "Return nil when no table is in the query."
  (should-not (ide-sql-grid--extract-table-name "SELECT 1;"))
  (should-not (ide-sql-grid--extract-table-name "VALUES (1, 'test');")))

;;;; 4. Value Quoting Tests ---------------------------------------------------------------------------

(ert-deftest sql-grid-quote-values ()
  "Test SQL literal formatting for UPDATE statements."
  (should (equal (ide-sql-grid--quote-value "123") "123"))
  (should (equal (ide-sql-grid--quote-value "-45.67") "-45.67"))
  (should (equal (ide-sql-grid--quote-value "hello") "'hello'"))
  (should (equal (ide-sql-grid--quote-value "it's fine") "'it''s fine'"))
  (should (equal (ide-sql-grid--quote-value "NULL") "NULL"))
  (should (equal (ide-sql-grid--quote-value "null") "NULL"))
  (should (equal (ide-sql-grid--quote-value "") "''")))

;;;; 5. Grid Rendering & Navigation -------------------------------------------------------------------

(ert-deftest sql-grid-render-and-navigate ()
  "Test rendering a vtable and navigating with h/j/k/l."
  (with-temp-buffer
    (ide-sql-grid-mode)
    (let* ((headers '("id" "name" "email"))
           (plists '((:id "1" :name "alice" :email "a@t.org")
                     (:id "2" :name "bob" :email "b@t.org")
                     (:id "3" :name "charlie" :email "c@t.org")))
           (table (ide-sql-grid--render-vtable headers plists)))
      (should table)
      (should (equal (vtable-columns table) (vtable-columns ide-sql-grid--table)))
      ;; Position at row 1, col 0
      (goto-char (point-min))
      (search-forward "alice")
      (should (equal (vtable-current-column) 1))
      (should (equal (plist-get (vtable-current-object) :id) "1"))
      ;; Navigate down with j (ide-sql-grid-next-row)
      (ide-sql-grid-next-row 1)
      (should (equal (vtable-current-column) 1))
      (should (equal (plist-get (vtable-current-object) :id) "2"))
      ;; Navigate right with l (ide-sql-grid-next-column)
      (ide-sql-grid-next-column)
      (should (equal (vtable-current-column) 2))
      ;; Navigate left with h (ide-sql-grid-previous-column)
      (ide-sql-grid-previous-column)
      (should (equal (vtable-current-column) 1))
      ;; Navigate up with k (ide-sql-grid-previous-row)
      (ide-sql-grid-previous-row 1)
      (should (equal (vtable-current-column) 1))
      (should (equal (plist-get (vtable-current-object) :id) "1")))))

;;;; 6. Editing, Mutation Ledger & Visual Feedback ---------------------------------------------------

(ert-deftest sql-grid-cell-edit-and-visual-feedback ()
  "Test cell edit recording in ledger and visual face application."
  (let ((ide-var-dir (make-temp-file "sqlvargrid" t)))
    (unwind-protect
        (with-temp-buffer
          (ide-sql-grid-mode)
          (setq ide-sql--result-conn '("testdb" (product . sqlite))
                ide-sql--result-sql "select * from users;"
                ide-sql-grid--table-name "users"
                ide-sql-grid--pk-column "id"
                ide-sql-grid--mutations nil)
          (let* ((headers '("id" "name"))
                 (plists '((:id "1" :name "alice")
                           (:id "2" :name "bob")))
                 (table (ide-sql-grid--render-vtable headers plists)))
            (setq ide-sql-grid--headers headers
                  ide-sql-grid--original-objects plists)
            ;; Move to cell "alice"
            (goto-char (point-min))
            (search-forward "alice")
            ;; Simulate editing: mock read-string to supply "alicia"
            (cl-letf (((symbol-function 'read-string) (lambda (_prompt &optional _initial) "alicia")))
              (ide-sql-grid-edit-cell))
            ;; Ledger must contain the mutation
            (should (= (length ide-sql-grid--mutations) 1))
            (let ((m (car ide-sql-grid--mutations)))
              (should (equal (ide-sql-mutation-row-pk-val m) "1"))
              (should (equal (ide-sql-mutation-column-name m) "name"))
              (should (equal (ide-sql-mutation-original-value m) "alice"))
              (should (equal (ide-sql-mutation-new-value m) "alicia")))
            ;; Cache file must exist in var/sql-grid/
            (should (file-exists-p (ide-sql-grid--cache-file)))
            ;; Visual feedback: getter must propertize cell with font-lock-warning-face
            (goto-char (point-min))
            (search-forward "alicia")
            (should (equal (get-text-property (1- (point)) 'face)
                           '(font-lock-warning-face vtable)))
            ;; Restoring cell to original value should prune from ledger
            (cl-letf (((symbol-function 'read-string) (lambda (_prompt &optional _initial) "alice")))
              (ide-sql-grid-edit-cell))
            (should-not ide-sql-grid--mutations)
            (should-not (file-exists-p (ide-sql-grid--cache-file)))))
      (delete-directory ide-var-dir t))))

;;;; 7. Rollback Tests --------------------------------------------------------------------------------

(ert-deftest sql-grid-rollback-restores-table ()
  "Test C-c C-k rollback clears mutations and redraws original table."
  (let ((ide-var-dir (make-temp-file "sqlvargrid" t)))
    (unwind-protect
        (with-temp-buffer
          (ide-sql-grid-mode)
          (setq ide-sql--result-conn '("testdb" (product . sqlite))
                ide-sql--result-sql "select * from users;"
                ide-sql-grid--table-name "users"
                ide-sql-grid--pk-column "id"
                ide-sql-grid--mutations nil)
          (let* ((headers '("id" "name"))
                 (plists '((:id "1" :name "alice")))
                 (table (ide-sql-grid--render-vtable headers plists)))
            (setq ide-sql-grid--headers headers
                  ide-sql-grid--original-objects plists)
            (goto-char (point-min))
            (search-forward "alice")
            (cl-letf (((symbol-function 'read-string) (lambda (_prompt &optional _initial) "mutated")))
              (ide-sql-grid-edit-cell))
            (should (= (length ide-sql-grid--mutations) 1))
            (should (file-exists-p (ide-sql-grid--cache-file)))
            ;; Execute rollback
            (ide-sql-grid-rollback)
            (should-not ide-sql-grid--mutations)
            (should-not (file-exists-p (ide-sql-grid--cache-file)))
            (goto-char (point-min))
            (should (search-forward "alice" nil t))
            (should-not (search-forward "mutated" nil t))))
      (delete-directory ide-var-dir t))))

;;;; 8. Real SQLite End-to-End Test -------------------------------------------------------------------

(ert-deftest sql-grid-real-sqlite-mutation-and-commit ()
  "Test real SQLite execution, editing a cell, committing, and verifying DB changes."
  (skip-unless (executable-find "sqlite3"))
  (ide-sql-grid-test--with-db
   (with-temp-buffer
     (sql-mode)
     (setq ide-sql-connection "testdb")
     (insert "select * from users order by id;")
     (goto-char 5)
     (let ((display-buffer-overriding-action '(display-buffer-no-window)))
       (ide-sql-run-dwim)
       (ide-sql-grid-test--wait
        (lambda ()
          (and (get-buffer ide-sql-result-buffer)
               (with-current-buffer ide-sql-result-buffer
                 (and (derived-mode-p 'ide-sql-grid-mode)
                      (not (string-search "running" (buffer-string))))))))
       (with-current-buffer ide-sql-result-buffer
         ;; Verify grid displayed
         (should (string-match-p "│ +1 │ alice" (buffer-string)))
         (should (string-match-p "│ +2 │ bob" (buffer-string)))
         ;; Move to alice and edit to "alice_updated"
         (goto-char (point-min))
         (search-forward "alice")
         (cl-letf (((symbol-function 'read-string)
                    (lambda (_p &optional _i) "alice_updated")))
           (ide-sql-grid-edit-cell))
         ;; Commit the mutation
         (let (commit-status)
           (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (ide-sql-grid-commit (lambda (st) (setq commit-status (or st 0))))
             (ide-sql-grid-test--wait (lambda () commit-status)))
           (should (zerop commit-status)))
         ;; Wait for rerun to complete
         (ide-sql-grid-test--wait
          (lambda ()
            (with-current-buffer ide-sql-result-buffer
              (and (null ide-sql-grid--mutations)
                   (not (string-search "running" (buffer-string)))))))
         ;; Verify buffer shows updated data
         (should (string-match-p "│ +1 │ alice_updated" (buffer-string)))
         ;; Verify DB file has updated value via direct sqlite3 query
         (should (equal (string-trim
                         (shell-command-to-string
                          (format "sqlite3 %S 'SELECT name FROM users WHERE id = 1;'" db)))
                        "alice_updated")))
       (kill-buffer ide-sql-result-buffer)))))

(provide 'lang-sql-grid-test)
;;; lang-sql-grid-test.el ends here
