;;; lang-sql-test.el --- Tests for modules/11-languages/lang-sql.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-sql-test.el -f ert-run-tests-batch-and-exit
;; Pure logic, plus real sqlite3 / sqlfluff / sql-formatter runs (skipped when missing).
;; sqls and the other databases were checked by hand.

;;; Code:

(require 'ert)
(require 'cl-lib)

(setq temporary-file-directory ide-cache-dir)

(defmacro ide-sql-test--project (&rest body)
  "Run BODY in a temp project holding a sqlite database and a connection file."
  `(let* ((dir (file-name-as-directory (make-temp-file "sqltest" t)))
          (default-directory dir)
          (sql-connection-alist nil)
          (ide-sql--history nil))
     (unwind-protect
         (progn
           (when (executable-find "sqlite3")
             (call-process "sqlite3" nil nil nil (expand-file-name "t.db" dir)
                           "create table users(id integer primary key, name text); insert into users values (1,'a'),(2,'b,c');"))
           (with-temp-file (expand-file-name ".sql-connections.eld" dir)
             (insert "((\"lite\" (sql-product 'sqlite) (sql-database \"t.db\"))\n"
                     " (\"pg\" (sql-product 'postgres) (sql-server \"db.local\") (sql-port 5433)\n"
                     "  (sql-user \"me\") (sql-database \"app\") (sql-password \"s3\"))\n"
                     " (\"evil\" (sql-product 'sqlite) (sql-database (error \"nope\"))))"))
           ,@body)
       (delete-directory dir t))))

(defun ide-sql-test--wait (pred)
  (let ((n 0)) (while (and (not (funcall pred)) (< (cl-incf n) 100)) (accept-process-output nil 0.1))))

(ert-deftest sql-menu-and-hooks-registered ()
  (should (eq (cdr (assoc '(sql-mode) ide-lang-menus)) #'ide-sql-dispatch))
  (should (memq #'ide-sql--run-command ide-run-command-functions))
  (should (eq (alist-get "\\.sql\\'" auto-mode-alist nil nil #'equal) 'sql-mode))
  (should (fboundp 'ide-lang-flymake-sqlfluff))
  (should (eq (lookup-key sql-mode-map (kbd "C-c C-c")) #'ide-sql-run-dwim)))

(ert-deftest sql-dialects-follow-the-product ()
  (should (equal (ide-sql--formatter-language 'postgres) "postgresql"))
  (should (equal (ide-sql--formatter-language 'ms) "transactsql"))
  (should (equal (ide-sql--sqlfluff-dialect 'ms) "tsql"))
  (should (equal (ide-sql--sqlfluff-dialect 'sqlite) "sqlite"))
  (should (equal (ide-sql--sqlfluff-dialect 'ansi) "ansi")))

(ert-deftest sql-connections-merge-project-file-and-ignore-code ()
  (ide-sql-test--project
   (let* ((all (ide-sql--connections)) (lite (assoc "lite" all)))
     (should (equal (mapcar #'car all) '("lite" "pg" "evil")))
     (should (equal (alist-get 'database lite) (expand-file-name "t.db" dir)))
     (should (eq (alist-get 'product lite) 'sqlite))
     ;; non-literal forms are dropped, never evaluated
     (should-not (alist-get 'database (assoc "evil" all)))
     (ide-sql--sync-connections)
     (should (assoc "lite" sql-connection-alist)))))

(ert-deftest sql-statement-bounds ()
  (with-temp-buffer
    (sql-mode)
    (insert "select 1;\n\nselect ';' as s, -- a ; comment\n  2;\nselect 3")
    (goto-char (point-min))
    (should (equal (ide-sql--statement-bounds) '(1 . 10)))
    (search-forward "as s")
    (let ((b (ide-sql--statement-bounds)))
      (should (string-prefix-p "select ';'" (buffer-substring (car b) (cdr b))))
      (should (string-suffix-p "2;" (buffer-substring (car b) (cdr b)))))
    (goto-char (point-max))
    (should (equal (buffer-substring (car (ide-sql--statement-bounds)) (cdr (ide-sql--statement-bounds)))
                   "select 3"))
    (goto-char 10)
    (should (equal (ide-sql--statement-bounds) '(1 . 10)))))

(ert-deftest sql-explain-and-describe-per-product ()
  (should (equal (ide-sql--explain-sql 'sqlite "select 1;" nil) "EXPLAIN QUERY PLAN select 1;"))
  (should (equal (ide-sql--explain-sql 'postgres "select 1" t) "EXPLAIN (ANALYZE, BUFFERS) select 1;"))
  (should (equal (ide-sql--explain-sql 'mysql "select 1" t) "EXPLAIN ANALYZE select 1;"))
  (should (string-match-p "SHOWPLAN_TEXT" (ide-sql--explain-sql 'ms "select 1" nil)))
  (should-error (ide-sql--explain-sql 'ansi "select 1" nil) :type 'user-error)
  (should (equal (ide-sql--describe-sql 'postgres "public.users") "\\d+ public.users"))
  (should (string-match-p "\\.schema \"?users" (ide-sql--describe-sql 'sqlite "\"users\"")))
  (should-error (ide-sql--describe-sql 'sqlite "users; drop table x") :type 'user-error))

(ert-deftest sql-client-command-lines ()
  (ide-sql-test--project
   (let ((pg (assoc "pg" (ide-sql--connections))))
     (skip-unless (executable-find "sqlite3"))
     (should-not (cl-some (lambda (e) (string-match-p ide-sql--secret-env-regexp e))
                          (cdr (ide-sql--spec (assoc "lite" (ide-sql--connections)) 'csv))))
     (should (equal (seq-take (reverse (car (ide-sql--spec (assoc "lite" (ide-sql--connections)) 'csv))) 2)
                    (list (expand-file-name "t.db" dir) "-csv")))
     ;; postgres: host, port, user, database; the password goes in the environment, not argv
     (cl-letf (((symbol-function 'ide-sql--program) (lambda (_) "psql")))
       (pcase-let ((`(,argv . ,env) (ide-sql--spec pg 'table)))
         (should (equal (cl-subseq argv (- (length argv) 8))
                        '("-h" "db.local" "-p" "5433" "-U" "me" "-d" "app")))
         (should (member "PGPASSWORD=s3" env))
         (should (member "MYSQL_HISTFILE=/dev/null" env))
         (should-not (member "s3" argv)))))))

(ert-deftest sql-shell-command-never-contains-the-password ()
  (let ((cmd (ide-sql--shell-command '("psql" "-d" "app") '("PGPASSWORD=s3cr3t" "PSQL_HISTORY=/x y") "/tmp/f.sql")))
    (should-not (string-match-p "s3cr3t" cmd))
    (should (string-match-p "\\`PGPASSWORD=\"\\$(cat " cmd))
    (should (string-match-p "PSQL_HISTORY=/x\\\\ y" cmd))
    ;; the line reads the secret from its 0600 file, then removes it
    (let ((file (progn (string-match "cat \\([^)]+\\))" cmd) (match-string 1 cmd))))
      (should (file-exists-p file))
      (should (equal (file-attribute-modes (file-attributes file)) "-rw-------"))
      (with-temp-buffer (insert-file-contents file) (should (equal (buffer-string) "s3cr3t")))
      (shell-command-to-string (replace-regexp-in-string "psql" "true" cmd))
      (should-not (file-exists-p file)))))

(ert-deftest sql-history-skips-statements-with-passwords ()
  (let ((ide-sql--history nil) (ide-var-dir (make-temp-file "sqlvar" t)))
    (unwind-protect
        (progn (ide-sql--history-add "create user x password 'p'" "a")
               (ide-sql--history-add "select 1" "a")
               (should (equal ide-sql--history '(("select 1" . "a")))))
      (delete-directory ide-var-dir t))))

(ert-deftest sql-project-connections-stay-in-their-buffer ()
  (ide-sql-test--project
   (let ((global (default-value 'sql-connection-alist)))
     (with-temp-buffer
       (sql-mode)
       (ide-sql--sync-connections)
       (should (assoc "lite" sql-connection-alist)))
     (should (equal (default-value 'sql-connection-alist) global)))))

(defun ide-sql-test--server-p (program &rest args)
  "Non-nil when running PROGRAM with ARGS (against a throwaway test server) succeeds."
  (and (executable-find program) (zerop (apply #'call-process program nil nil nil args))))

(defun ide-sql-test--run (conn sql &optional mode)
  "Run SQL on CONN; return (STATUS . TEXT)."
  (let (out)
    (ide-sql--spawn conn sql (or mode 'table) (lambda (text status _) (setq out (cons status (if (and (eq (or mode (quote table)) (quote csv)) (zerop status)) (ide-sql--csv-filter conn text) text)))))
    (ide-sql-test--wait (lambda () out))
    out))

(ert-deftest sql-postgres-throwaway-server ()
  "Needs a test server on 127.0.0.1:54329 (user me, database app, password secretpw)."
  (skip-unless (let ((process-environment (cons "PGPASSWORD=secretpw" process-environment)))
                 (ide-sql-test--server-p "psql" "-h" "127.0.0.1" "-p" "54329" "-U" "me" "-d" "app" "-c" "select 1")))
  (let ((conn '("pg" (product . postgres) (server . "127.0.0.1") (port . 54329) (user . "me")
                (database . "app") (password . "secretpw"))))
    (should (string-match-p "(1 row)" (cdr (ide-sql-test--run conn "select 1 as x"))))
    (should (equal (cdr (ide-sql-test--run conn "select 'a,b' as x, null as y" 'csv)) "x,y\n\"a,b\",\n"))
    (should (/= 0 (car (ide-sql-test--run conn "select * from missing_table"))))))

(ert-deftest sql-mariadb-throwaway-server ()
  "Needs a test server on 127.0.0.1:33069 (user me, database app, password secretpw)."
  (skip-unless (let ((process-environment (cons "MYSQL_PWD=secretpw" process-environment)))
                 (ide-sql-test--server-p (or (executable-find "mariadb") "mysql") "-h" "127.0.0.1" "-P" "33069"
                                         "-u" "me" "app" "-e" "select 1")))
  (let ((conn '("maria" (product . mariadb) (server . "127.0.0.1") (port . 33069) (user . "me")
                (database . "app") (password . "secretpw"))))
    (should (string-match-p "| 1 |" (cdr (ide-sql-test--run conn "select 1"))))
    (should (equal (cdr (ide-sql-test--run conn "select 'a,b' as x, null as y, 'l1\nl2' as z" 'csv))
                   "x,y,z\n\"a,b\",,\"l1\nl2\"\n"))
    ;; client chatter on stderr never reaches the CSV
    (should-not (string-match-p "WARNING\\|Deprecated" (cdr (ide-sql-test--run conn "select 1" 'csv))))))

(ert-deftest sql-csv-conversions ()
  (should (equal (ide-sql--tsv-to-csv "a\tb\n1\tx,y\n2\tq\\tr\n3\tNULL\n") "a,b\n1,\"x,y\"\n2,q\tr\n3,"))
  (should (equal (ide-sql--sqlcmd-to-csv
                  (subst-char-in-string ?| ?\x1f "id|name|note\n--|----|----\n1|a|x,y\n2|b|NULL\n3|c|l1\nl2\n\n(3 rows affected)\n"))
                 "id,name,note\n1,a,\"x,y\"\n2,b,\n3,c,\"l1\nl2\"")))

(ert-deftest sql-sqls-connections-from-entries ()
  (ide-sql-test--project
   (let ((all (ide-sql--connections)))
     (should (equal (ide-sql--sqls-connection (assoc "lite" all))
                    (list :alias "lite" :driver "sqlite3" :dataSourceName (expand-file-name "t.db" dir))))
     (should (equal (plist-get (ide-sql--sqls-connection (assoc "pg" all)) :dataSourceName)
                    "host=db.local port=5433 user=me password=s3 dbname=app sslmode=disable"))
     (should-not (ide-sql--sqls-connection (assoc "evil" all)))
     ;; the active connection goes first
     (let ((ide-sql--lsp-choice (list (cons dir "pg"))))
       (let ((conns (plist-get (ide-sql--sqls-settings dir) :connections)))
         (should (equal (plist-get (aref conns 0) :alias) "pg"))
         (should (= (length conns) 2)))))))

(ert-deftest sql-workspace-config-keeps-other-languages ()
  (ide-sql-test--project
   (let ((eglot-workspace-configuration '(:gopls (:staticcheck t))))
     (let ((cfg (ide-sql--workspace-config nil)))
       (should (plist-get cfg :gopls))
       (should (plist-get cfg :sqls))))))

(ert-deftest sql-templates-expand-without-yasnippet ()
  (should (equal (ide-sql--template-plain "SELECT ${1:*}\nFROM ${2:table};$0")
                 "SELECT *\nFROM table;")))

(ert-deftest sql-history-is-deduplicated-and-newest-first ()
  (let ((ide-sql--history nil) (ide-var-dir (make-temp-file "sqlvar" t)))
    (unwind-protect
        (progn (ide-sql--history-add "select 1" "a") (ide-sql--history-add "select 2" "a")
               (ide-sql--history-add "select 1" "b")
               (should (equal ide-sql--history '(("select 1" . "b") ("select 2" . "a"))))
               (setq ide-sql--history 'unloaded)
               (should (equal (car (ide-sql--history-load)) '("select 1" . "b"))))
      (delete-directory ide-var-dir t))))

(ert-deftest sql-runs-against-real-sqlite-and-exports-csv ()
  (skip-unless (executable-find "sqlite3"))
  (ide-sql-test--project
   (with-temp-buffer
     (sql-mode)
     (setq ide-sql-connection "lite")
     (insert "select * from users order by id;")
     (goto-char 5)
     (let ((ide-var-dir dir) (display-buffer-overriding-action '(display-buffer-no-window)))
       (ide-sql-run-dwim)
       (ide-sql-test--wait (lambda () (and (get-buffer ide-sql-result-buffer)
                                           (with-current-buffer ide-sql-result-buffer
                                             (not (string-search "running" (buffer-string)))))))
       (with-current-buffer ide-sql-result-buffer
         (should (string-match-p "│ +2 │ b,c" (buffer-string)))
         (should (string-match-p "lite \\[sqlite\\]" header-line-format)))
       (let ((csv (expand-file-name "out.csv" dir)))
         (ide-sql-export-csv csv)
         (ide-sql-test--wait (lambda () (file-exists-p csv)))
         (should (equal (with-temp-buffer (insert-file-contents csv) (buffer-string))
                        "id,name\n1,a\n2,\"b,c\"\n")))
       (kill-buffer ide-sql-result-buffer)))))

(ert-deftest sql-s-f10-command-runs-the-file ()
  (skip-unless (executable-find "sqlite3"))
  (ide-sql-test--project
   (let ((f (expand-file-name "q.sql" dir)))
     (write-region "select name from users;" nil f)
     (with-current-buffer (find-file-noselect f)
       (setq ide-sql-connection "lite")
       (let ((cmd (ide-sql--run-command)))
         (should (string-match-p "sqlite3.* < " cmd))
         (should (string-match-p "│ b,c" (shell-command-to-string cmd))))
       (kill-buffer)))))

(ert-deftest sql-formatter-uses-the-dialect ()
  (skip-unless (ide-lang-node-exe "sql-formatter"))
  (with-temp-buffer
    (sql-mode)
    (sql-set-product 'postgres)
    (insert "select a,b from t where x=1")
    (ide-sql-format)
    (should (equal (buffer-string) "SELECT\n  a,\n  b\nFROM\n  t\nWHERE\n  x = 1\n"))))

(ert-deftest sql-sqlfluff-reports-and-fixes ()
  (skip-unless (executable-find "sqlfluff"))
  (with-temp-buffer
    (sql-mode)
    (sql-set-product 'sqlite)
    (insert "select  a from t\nselec 1;\n")
    (let ((trusted-content :all) res)  ; sqlfluff waits for a trusted project
      (ide-lang-flymake-sqlfluff (lambda (d &rest _) (setq res (or d 'none))))
      (ide-sql-test--wait (lambda () res))
      (should (consp res))
      (should (cl-some (lambda (d) (eq (flymake-diagnostic-type d) :error)) res))
      (should (cl-every (lambda (d) (string-prefix-p "sqlfluff: " (flymake-diagnostic-text d))) res)))
    (erase-buffer)
    (insert "select  a,b from t where x=1;\n")
    (ide-sql-fix)
    (should (equal (buffer-string) "select\n  a,\n  b\nfrom t\nwhere x = 1;\n"))))

(ert-deftest sql-mssql-input-and-repl-entry ()
  (let ((conn '("ms" (product . ms) (server . "h") (port . 1444) (user . "sa") (database . "d") (password . "pw"))))
    (should (equal (ide-sql--prepare conn "select 1") "select 1;\nGO\n"))
    (should (equal (ide-sql--prepare conn "SET X ON;\nGO\nselect 1;\nGO") "SET X ON;\nGO\nselect 1;\nGO\n"))
    (should (equal (ide-sql--prepare '("p" (product . postgres)) "select 1") "select 1;\n"))
    (let ((e (ide-sql--repl-entry conn)))
      (should-not (assq 'sql-password (cdr e)))
      (should (equal (cadr (assq 'sql-server (cdr e))) "h,1444"))
      (should-not (string-search "pw" (format "%S" e))))
    (should (string-match-p "sqlserver://sa:p%40ss@h:1444\\?database=d"
                            (plist-get (ide-sql--sqls-connection (cons "ms" (cons '(password . "p@ss") (cdr conn))))
                                       :dataSourceName)))
    (pcase-let ((`(,argv . ,env) (cl-letf (((symbol-function 'ide-sql--program) (lambda (_) "sqlcmd")))
                                   (ide-sql--spec conn 'csv))))
      (should (member "-s" argv))
      (should (member "SQLCMDPASSWORD=pw" env))
      (should-not (member "pw" argv)))))

(ert-deftest sql-mssql-throwaway-server ()
  "Needs a test container on 127.0.0.1:14339 (user sa, database app) and its password in SQL_TEST_MSSQL_PASSWORD."
  (let ((pw (getenv "SQL_TEST_MSSQL_PASSWORD")))
    (skip-unless (and pw (executable-find "sqlcmd")
                      (let ((process-environment (cons (concat "SQLCMDPASSWORD=" pw) process-environment)))
                        (ide-sql-test--server-p "sqlcmd" "-S" "127.0.0.1,14339" "-U" "sa" "-C" "-d" "app" "-Q" "select 1"))))
    (let ((conn `("ms" (product . ms) (server . "127.0.0.1") (port . 14339) (user . "sa")
                  (database . "app") (password . ,pw))))
      (should (string-match-p "(1 rows? affected)" (cdr (ide-sql-test--run conn "select 1 as x"))))
      (should (equal (cdr (ide-sql-test--run conn "select 'a,b' as x, null as y, 'l1'+char(10)+'l2' as z" 'csv))
                     "x,y,z\n\"a,b\",,\"l1\nl2\"\n"))
      (should (string-match-p "Clustered Index\\|Table Scan\\|StmtText"
                              (cdr (ide-sql-test--run conn (ide-sql--explain-sql 'ms "select name from sys.objects" nil)))))
      (should (/= 0 (car (ide-sql-test--run conn "select * from missing_table")))))))

(provide 'lang-sql-test)
;;; lang-sql-test.el ends here
