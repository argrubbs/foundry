;;; lang-sql.el --- SQL: connections, run and explain, schema, sqls, sqlfluff, formatter -*- lexical-binding: t; -*-

;;; Commentary:
;; A database client inside the editor:
;;
;;   edit      sql-mode + sql-indent + sqlup (keywords upper-cased as you type) + sqls (Eglot:
;;             schema-aware completion, hover, diagnostics)
;;   connect   `ide-sql-connect' picks an entry of `sql-connection-alist' for the buffer; entries
;;             in a gitignored .sql-connections.eld (project root) are merged in, so credentials
;;             stay out of git.  `ide-sql-add-connection' writes that file.
;;   run       C-c C-c runs the region or the statement at point and shows the result in the
;;             drawer (one-shot client process, not the *SQL* comint buffer); also paragraph,
;;             buffer, a file, a folder of migrations; S-<f10> runs the file on the connection
;;   explain   EXPLAIN / EXPLAIN ANALYZE of the statement at point (per product)
;;   schema    tables, describe the table at point, the native `sql-list-all' / `sql-list-table'
;;   results   `g' re-run, `t' table <-> CSV, `w' copy CSV, `e' export CSV; history of statements
;;   quality   sql-formatter (dialect from `sql-product') on `ide-sql-format' and on save,
;;             sqlfluff as a Flymake backend, `ide-sql-fix' (sqlfluff fix), `ide-sql-parse'
;;
;; Products with a one-shot client: sqlite, postgres, mysql/mariadb, ms (sqlcmd).  Only sqlite
;; was exercised against a real server; the others follow the client documentation.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'sql)
(require 'transient)

(defvar ide-var-dir)
(defvar ide-cache-dir)
(defvar ide-run-command-functions)
(defvar ide-eglot-format-on-save)
(defvar eglot-ignored-server-capabilities)
(defvar eglot-workspace-configuration)
(defvar sqlind-basic-offset)
(defvar sql-mysql-login-params)
(defvar sql-mariadb-login-params)
(defvar sql-ms-login-params)
(defvar sql-ms-options)
(defvar sql-ms-program)
(defvar sql-mysql-program)
(defvar sql-mariadb-program)
(declare-function ide-lang-run "00-lang-lib")
(declare-function ide-lang-root "00-lang-lib")
(declare-function ide-lang-browse "00-lang-lib")
(declare-function ide-lang-node-exe "00-lang-lib")
(declare-function ide-lang-exe-or-error "00-lang-lib")
(declare-function ide-lang-diag "00-lang-lib")
(declare-function ide-lang-define-linter "00-lang-lib")
(declare-function ide-lang-register-menu "00-lang-lib")
(declare-function ide-add-drawer-buffer "02-layout")
(declare-function yas-expand-snippet "yasnippet")
(declare-function eglot-shutdown "eglot")
(declare-function auth-source-pick-first-password "auth-source")

(defgroup ide-sql nil "SQL support." :group 'ide-lang)

(defcustom ide-sql-connections-file ".sql-connections.eld"
  "Project-local connection file (gitignored): a list of `sql-connection-alist' entries.
Only literal values are read from it (strings, numbers, quoted symbols)."
  :type 'string)

(defcustom ide-sql-format-on-save t
  "Format SQL buffers with sql-formatter (through Apheleia) when saving."
  :type 'boolean)

(defcustom ide-sql-keyword-case "upper"
  "Keyword case sql-formatter produces: upper, lower or preserve."
  :type '(choice (const "upper") (const "lower") (const "preserve")))

(defcustom ide-sql-history-size 200 "Statements kept in the history." :type 'integer)

(defvar-local ide-sql-connection nil
  "Name of the connection this buffer runs on (also settable as a file-local variable).")
(put 'ide-sql-connection 'safe-local-variable #'stringp)

;;;; Mode, indentation, uppercase keywords ----------------------------------------------

(add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))

(use-package sql-indent
  :init (setq sqlind-basic-offset 2)
  :hook (sql-mode . sqlind-minor-mode))

(use-package sqlup-mode
  :hook ((sql-mode sql-interactive-mode) . sqlup-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(sql-mode . ("sqls"))))

(defun ide-sql--setup ()
  "Per-buffer setup: indentation, the connection, the language server, format on save."
  (setq-local tab-width 2 indent-tabs-mode nil
              ide-eglot-format-on-save nil)
  (ide-sql--sync-connections)
  (unless ide-sql-connection
    (let ((all (ide-sql--connections)))
      (when (and all (null (cdr all))) (ide-sql--select (caar all) t))))
  (when ide-sql-connection (ide-sql--select ide-sql-connection t))
  (ide-sql--register-dir-class)
  (unless ide-sql-format-on-save (setq-local apheleia-inhibit t))
  ;; sqls formatting crashes on the tested version; sql-formatter does it.
  (setq-local eglot-ignored-server-capabilities
              (append '(:documentFormattingProvider :documentRangeFormattingProvider
                        :documentOnTypeFormattingProvider)
                      (and (boundp 'eglot-ignored-server-capabilities)
                           (default-value 'eglot-ignored-server-capabilities))))
  (when (and buffer-file-name (executable-find "sqls")) (eglot-ensure)))

(add-hook 'sql-mode-hook #'ide-sql--setup)

;;;; Products and dialects -------------------------------------------------------------------

(defun ide-sql--product (&optional conn)
  "Product symbol of CONN (a normalized connection) or of this buffer."
  (let ((p (or (and conn (alist-get 'product conn)) sql-product 'ansi)))
    (pcase p ('mssql 'ms) ('postgresql 'postgres) ('sqlite3 'sqlite) (_ p))))

(defun ide-sql--formatter-language (&optional product)
  "sql-formatter language for PRODUCT (default: the buffer's)."
  (pcase (or product (ide-sql--product))
    ('sqlite "sqlite") ('postgres "postgresql") ('mysql "mysql") ('mariadb "mariadb")
    ('ms "transactsql") ('oracle "plsql") ('db2 "db2") (_ "sql")))

(defun ide-sql--sqlfluff-dialect (&optional product)
  "sqlfluff dialect for PRODUCT (default: the buffer's)."
  (pcase (or product (ide-sql--product))
    ('sqlite "sqlite") ('postgres "postgres") ('mysql "mysql") ('mariadb "mariadb")
    ('ms "tsql") ('oracle "oracle") ('db2 "db2") ('vertica "vertica") (_ "ansi")))

;;;; Connections -------------------------------------------------------------------------------

(defun ide-sql--literal (form)
  "The value of FORM when it is a literal (string, number, quoted symbol), else nil."
  (pcase form
    ((pred stringp) form)
    ((pred numberp) form)
    (`(quote ,(and (pred symbolp) s)) s)
    (_ nil)))

(defun ide-sql--normalize (entry basedir)
  "ENTRY (NAME (sql-VAR FORM)...) as (NAME (VAR . value)...); sqlite paths made absolute."
  (let (out)
    (dolist (item (cdr entry))
      (when (and (consp item) (symbolp (car item))
                 (string-prefix-p "sql-" (symbol-name (car item))))
        (when-let* ((v (ide-sql--literal (cadr item))))
          (push (cons (intern (substring (symbol-name (car item)) 4)) v) out))))
    (setq out (nreverse out))
    (let ((db (alist-get 'database out)))
      (when (and (stringp db) (eq (ide-sql--product out) 'sqlite) (not (string-prefix-p ":" db)))
        (setf (alist-get 'database out) (expand-file-name db basedir))))
    (cons (car entry) out)))

(defun ide-sql--read-file (file)
  "The first Lisp form of FILE, or nil when unreadable."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case nil (read (current-buffer)) (error nil)))))

(defun ide-sql--connections-file (&optional dir)
  "The connection file at or above DIR, or nil."
  (when-let* ((d (locate-dominating-file (or dir default-directory) ide-sql-connections-file)))
    (expand-file-name ide-sql-connections-file d)))

(defun ide-sql--connections (&optional dir)
  "Alist (NAME . normalized) of `sql-connection-alist' plus the project file's entries."
  (let* ((root (ide-lang-root dir))
         (file (ide-sql--connections-file dir))
         (project (when file
                   (mapcar (lambda (e) (ide-sql--normalize e (file-name-directory file)))
                           (seq-filter (lambda (e) (and (consp e) (stringp (car e))))
                                       (ide-sql--read-file file)))))
         (global (mapcar (lambda (e) (ide-sql--normalize (cons (format "%s" (car e)) (cdr e)) root))
                         (cl-remove-if (lambda (e) (assoc (format "%s" (car e)) project))
                                       (default-value 'sql-connection-alist)))))
    (append project global)))

(defun ide-sql--entry (conn)
  "The `sql-connection-alist' entry for the normalized connection CONN."
  (cons (car conn)
        (mapcar (lambda (kv)
                  (list (intern (format "sql-%s" (car kv)))
                        (if (symbolp (cdr kv)) (list 'quote (cdr kv)) (cdr kv))))
                (cdr conn))))

(defun ide-sql--sync-connections ()
  "Make the project file's connections visible to `sql-connect' in this buffer only."
  (let ((project (ide-sql--connections)) (global (default-value 'sql-connection-alist)))
    (setq-local sql-connection-alist
                (append (mapcar #'ide-sql--entry project)
                        (cl-remove-if (lambda (e) (assoc (format "%s" (car e)) project)) global)))))

(defun ide-sql--get (conn key)
  "Value of KEY (a symbol) in CONN as a string, or nil."
  (when-let* ((v (alist-get key (cdr conn)))) (format "%s" v)))

(defun ide-sql--find (name)
  (assoc name (ide-sql--connections)))

(defvar ide-sql--lsp-choice nil "Alist (PROJECT-ROOT . connection name) feeding sqls.")

(defun ide-sql--select (name &optional quiet)
  "Make connection NAME the one of this buffer: product, history and the language server."
  (let ((conn (or (ide-sql--find name) (user-error "No connection named %s" name))))
    (setq ide-sql-connection name)
    (when-let* ((p (ide-sql--product conn)))
      (unless (eq p sql-product) (sql-set-product p)))
    (setf (alist-get (ide-lang-root) ide-sql--lsp-choice nil nil #'equal) name)
    (when-let* ((s (and (fboundp 'eglot-current-server) (eglot-current-server))))
      (eglot-signal-didChangeConfiguration s))
    (when (eq (ide-sql--product conn) 'ms) (setq-local sql-send-terminator t))
    (unless quiet (message "SQL connection: %s (%s)" name (ide-sql--product conn)))
    conn))

;;;###autoload
(defun ide-sql-connect (name)
  "Choose the connection NAME for this buffer from the known connections."
  (interactive
   (let ((names (mapcar #'car (ide-sql--connections))))
     (unless names (user-error "No connections; run M-x ide-sql-add-connection"))
     (list (completing-read "Connection: " names nil t nil nil ide-sql-connection))))
  (ide-sql--select name))

(defun ide-sql--ensure-connection ()
  "The normalized connection of this buffer, asking for one when needed."
  (let ((all (ide-sql--connections)))
    (cond
     ((and ide-sql-connection (assoc ide-sql-connection all)))
     ((null all) (user-error "No connections; run M-x ide-sql-add-connection"))
     ((null (cdr all)) (ide-sql--select (caar all)))
     (t (call-interactively #'ide-sql-connect) (ide-sql--find ide-sql-connection)))))

(defun ide-sql-add-connection ()
  "Add a connection to the project's gitignored connection file."
  (interactive)
  (let* ((product (intern (completing-read "Product: " '("sqlite" "postgres" "mysql" "mariadb" "ms") nil t)))
         (name (read-string "Connection name: "))
         (entry
          (if (eq product 'sqlite)
              `(,name (sql-product 'sqlite) (sql-database ,(read-file-name "Database file: " nil nil nil)))
            (let ((server (read-string "Host: " "localhost"))
                  (port (read-string "Port (empty for default): "))
                  (user (read-string "User: "))
                  (db (read-string "Database: ")))
              `(,name (sql-product ',product) (sql-server ,server)
                      ,@(unless (string-empty-p port) `((sql-port ,(string-to-number port))))
                      (sql-user ,user) (sql-database ,db)))))
         (root (ide-lang-root))
         (file (or (ide-sql--connections-file) (expand-file-name ide-sql-connections-file root)))
         (all (ide-sql--read-file file)))
    (when (and (eq product 'sqlite) (stringp (cadr (nth 2 entry))))
      (setf (cadr (nth 2 entry)) (file-relative-name (expand-file-name (cadr (nth 2 entry))) (file-name-directory file))))
    (with-temp-file file
      (let ((print-length nil) (print-level nil))
        (insert ";; SQL connections (keep this file out of git; passwords may go here)\n")
        (pp (append (cl-remove name all :key #'car :test #'equal) (list entry)) (current-buffer))))
    (ide-sql--sync-connections)
    (ide-sql--offer-gitignore root file)
    (message "Connection %s saved to %s; add (sql-password \"...\") there if it needs one"
             name (abbreviate-file-name file))))

(defun ide-sql--offer-gitignore (root file)
  "Offer to list FILE in ROOT's .gitignore when ROOT is a git work tree."
  (let ((ignore (expand-file-name ".gitignore" root))
        (rel (file-relative-name file root)))
    (when (and (file-directory-p (expand-file-name ".git" root))
               (not (and (file-exists-p ignore)
                         (with-temp-buffer (insert-file-contents ignore)
                                           (re-search-forward (concat "^/?" (regexp-quote rel) "$") nil t))))
               (y-or-n-p (format "Add %s to .gitignore? " rel)))
      (with-temp-buffer
        (when (file-exists-p ignore) (insert-file-contents ignore))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert rel "\n")
        (write-region nil nil ignore)))))

(defun ide-sql-edit-connections ()
  "Open the project connection file (created empty when missing)."
  (interactive)
  (let ((file (or (ide-sql--connections-file)
                  (expand-file-name ide-sql-connections-file (ide-lang-root)))))
    (unless (file-exists-p file)
      (with-temp-file file (insert ";; Entries like sql-connection-alist; keep this file out of git.\n()\n")))
    (find-file file)))

;;;; One-shot clients ---------------------------------------------------------------------------

(defun ide-sql--password (conn)
  "Password of CONN: the entry's, else auth-source's, else nil (the client's own files apply)."
  (or (ide-sql--get conn 'password)
      (when-let* ((server (ide-sql--get conn 'server)) (user (ide-sql--get conn 'user)))
        (ignore-errors
          (require 'auth-source)
          (auth-source-pick-first-password :host server :user user
                                           :port (ide-sql--get conn 'port))))))

(defun ide-sql--program (product)
  "Path of the command line client for PRODUCT.
For mysql and mariadb the `mariadb' binary wins when `mysql' is only its deprecated alias."
  (let ((name (pcase product
                ('ms "sqlcmd")
                (_ (or (sql-get-product-feature product :sqli-program)
                       (user-error "No command line client known for %s" product))))))
    (when (member product '(mysql mariadb))
      (let ((m (executable-find "mysql")))
        (when (or (eq product 'mariadb) (and m (string-match-p "maria" (file-truename m))))
          (when (executable-find "mariadb") (setq name "mariadb")))))
    (ide-lang-exe-or-error name "install the client for this database")))

(defcustom ide-sql-mssql-trust-server-certificate 'local
  "Pass -C (trust the server certificate) to sqlcmd: `local' for localhost only, t, or nil."
  :type '(choice (const local) (const t) (const nil)))

(defun ide-sql--mssql-trust-p (host)
  (pcase ide-sql-mssql-trust-server-certificate
    ('local (member host '(nil "localhost" "127.0.0.1" "::1")))
    (v v)))

(defun ide-sql--client-history-env ()
  "Environment keeping the clients' history files inside the sandbox (or off)."
  (let ((dir (expand-file-name "sql-client-history/" ide-var-dir)))
    (make-directory dir t)
    (list (concat "PSQL_HISTORY=" dir "psql") (concat "SQLITE_HISTORY=" dir "sqlite")
          "MYSQL_HISTFILE=/dev/null" "MARIADB_HISTFILE=/dev/null")))

(defun ide-sql--spec (conn mode)
  "Command line and environment running CONN's client on stdin: (ARGV . ENV).
MODE is `table' (readable) or `csv'.  Passwords only ever appear in ENV."
  (pcase-let ((`(,argv . ,env) (ide-sql--spec-1 conn mode)))
    (cons argv (append env (ide-sql--client-history-env)))))

(defun ide-sql--spec-1 (conn mode)
  (let* ((p (ide-sql--product conn))
         (db (ide-sql--get conn 'database)) (host (ide-sql--get conn 'server))
         (port (ide-sql--get conn 'port)) (user (ide-sql--get conn 'user))
         (pw (ide-sql--password conn)) (csv (eq mode 'csv)))
    (pcase p
      ('sqlite
       (unless db (user-error "Connection %s has no database file" (car conn)))
       (cons (list (ide-sql--program p) "-bail" "-header" (if csv "-csv" "-box") db) nil))
      ('postgres
       (cons `(,(ide-sql--program p) "-X" "-v" "ON_ERROR_STOP=1" "-P" "pager=off"
               ,@(when csv '("--csv"))
               ,@(when host `("-h" ,host)) ,@(when port `("-p" ,port))
               ,@(when user `("-U" ,user)) ,@(when db `("-d" ,db)))
             (when pw (list (concat "PGPASSWORD=" pw)))))
      ((or 'mysql 'mariadb)
       (cons `(,(ide-sql--program p) ,(if csv "--batch" "--table")
               ,@(when host `("-h" ,host)) ,@(when port `("-P" ,port))
               ,@(when user `("-u" ,user)) ,@(when db (list db)))
             (when pw (list (concat "MYSQL_PWD=" pw)))))
      ('ms
       (cons `(,(ide-sql--program p) "-b"
               ,@(when host `("-S" ,(if port (concat host "," port) host)))
               ,@(if user `("-U" ,user) '("-E"))
               ,@(when db `("-d" ,db))
               ,@(when (ide-sql--mssql-trust-p host) '("-C"))
               ,@(when csv (list "-s" ide-sql--sqlcmd-separator "-W")))
             (when pw (list (concat "SQLCMDPASSWORD=" pw)))))
      (_ (user-error "No one-shot client for product %s (sqlite, postgres, mysql, mariadb, ms)" p)))))

(defun ide-sql--prepare (conn sql)
  "SQL as the client's stdin: a terminating `;' for plain statements, GO for sqlcmd."
  (let* ((s (string-trim sql))
         (ms (eq (ide-sql--product conn) 'ms))
         (go (and ms (string-match-p "\\(?:\\`\\|\n\\)GO\\'" s)))
         (plain (and (not (string-empty-p s)) (not go) (not (string-match-p "\\`[.\\\\]" s))
                     (not (string-suffix-p ";" s)))))
    (concat s (if plain ";" "") "\n" (if (and ms (not go)) "GO\n" ""))))

(defun ide-sql--tsv-to-csv (text)
  "Convert mysql --batch output TEXT (tab separated, backslash escapes) to CSV.\nA field NULL becomes empty (a text value spelled NULL cannot be told apart)."
  (mapconcat
   (lambda (line)
     (mapconcat (lambda (f)
                  (let ((f (replace-regexp-in-string "\\\\\\([tn\\\\]\\)"
                                                     (lambda (m) (pcase (match-string 1 m) ("t" "\t") ("n" "\n") (_ "\\")))
                                                     (if (equal f "NULL") "" f) t t)))
                    (if (string-match-p "[\",\n]" f)
                        (concat "\"" (replace-regexp-in-string "\"" "\"\"" f) "\"")
                      f)))
                (split-string line "\t") ","))
   (split-string text "\n" t) "\n"))

(defconst ide-sql--sqlcmd-separator "\x1f"
  "Column separator asked of sqlcmd for CSV: a character no value contains.")

(defun ide-sql--sqlcmd-to-csv (text)
  "Convert sqlcmd output TEXT (columns split by `ide-sql--sqlcmd-separator') to CSV.
Drops the dashes rule and the `(N rows affected)' footer, joins rows whose values
contain newlines, and turns NULL into an empty field."
  (let ((sep ide-sql--sqlcmd-separator) rows ncols)
    (dolist (line (split-string text "\n"))
      (let ((line (string-remove-suffix "\r" line)))
        (cond
         ((string-match-p "\\`([0-9]+ rows? affected)\\'" line) (setq ncols nil))
         ((string-match-p (concat "\\`[-" sep "]+\\'") line))
         ((string-empty-p line))
         ((null ncols) (setq ncols (1+ (cl-count ?\x1f line))) (push line rows))
         ;; a line that does not complete a row continues the previous value
         ((and rows (< (cl-count ?\x1f (car rows)) (1- ncols))) (setcar rows (concat (car rows) "\n" line)))
         ((< (cl-count ?\x1f line) (1- ncols)) (setcar rows (concat (car rows) "\n" line)))
         (t (push line rows)))))
    (mapconcat
     (lambda (row)
       (mapconcat (lambda (f)
                    (cond ((equal f "NULL") "")
                          ((string-match-p "[\",\n]" f)
                           (concat "\"" (replace-regexp-in-string "\"" "\"\"" f) "\""))
                          (t f)))
                  (split-string row sep) ","))
     (nreverse rows) "\n")))

(defun ide-sql--csv-filter (conn text)
  "CSV text of the client output TEXT for CONN, ending in a newline."
  (let ((csv (pcase (ide-sql--product conn)
               ((or 'mysql 'mariadb) (ide-sql--tsv-to-csv text))
               ('ms (ide-sql--sqlcmd-to-csv text))
               (_ text))))
    (if (or (string-empty-p csv) (string-suffix-p "\n" csv)) csv (concat csv "\n"))))

(defconst ide-sql--noise-regexp
  "^\\(?:WARNING: option --ssl-verify-server-cert is disabled.*\\|.*Deprecated program name.*\\)\n?"
  "Client chatter on stderr that is not about the statement.")

(defun ide-sql--spawn (conn sql mode callback)
  "Run SQL through CONN's client; call CALLBACK with (TEXT EXIT-STATUS SECONDS).
TEXT is stdout only for CSV that succeeded; otherwise stdout followed by stderr."
  (pcase-let* ((`(,argv . ,env) (ide-sql--spec conn mode))
               (err-file (let ((temporary-file-directory ide-cache-dir)) (make-temp-file "sql-err")))
               (process-environment (append (list (concat "SQL_ERR_FILE=" err-file)) env process-environment))
               (default-directory (ide-lang-root))
               (out (generate-new-buffer " *sql-out*"))
               (start (float-time))
               (proc (make-process
                      :name "sql" :buffer out :noquery t :connection-type 'pipe
                      :command `("sh" "-c" "exec \"$0\" \"$@\" 2>\"$SQL_ERR_FILE\"" ,@argv)
                      :sentinel
                      (lambda (p _event)
                        (when (memq (process-status p) '(exit signal))
                          (let* ((stdout (with-current-buffer out (buffer-string)))
                                 (stderr (replace-regexp-in-string
                                          ide-sql--noise-regexp ""
                                          (with-temp-buffer (insert-file-contents err-file) (buffer-string))))
                                 (status (process-exit-status p)))
                            (kill-buffer out)
                            (delete-file err-file)
                            (funcall callback
                                     (if (and (eq mode 'csv) (zerop status)) stdout
                                       (concat stdout (if (or (string-empty-p stdout) (string-empty-p stderr)) "" "\n") stderr))
                                     status (- (float-time) start))))))))
    (process-send-string proc (ide-sql--prepare conn sql))
    (process-send-eof proc)
    proc))

;;;; Results ------------------------------------------------------------------------------------

(defconst ide-sql-result-buffer "*run: sql*"
  "Result buffer; the name docks it into the bottom drawer.")

(defvar-local ide-sql--result-conn nil)
(defvar-local ide-sql--result-sql nil)
(defvar-local ide-sql--result-mode 'table)

(defvar-keymap ide-sql-result-mode-map
  :doc "Keys of SQL result buffers."
  "g" #'ide-sql-result-rerun
  "t" #'ide-sql-result-toggle-csv
  "w" #'ide-sql-copy-csv
  "e" #'ide-sql-export-csv)

(define-derived-mode ide-sql-result-mode special-mode "SQL-Result"
  "Results of a statement.  \\{ide-sql-result-mode-map}"
  (setq-local truncate-lines t))

(defun ide-sql--show (conn sql mode)
  "Run SQL on CONN and show the result in the drawer in MODE (`table' or `csv')."
  (let ((buf (get-buffer-create ide-sql-result-buffer)))
    (with-current-buffer buf
      (ide-sql-result-mode)
      (setq ide-sql--result-conn conn ide-sql--result-sql sql ide-sql--result-mode mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "-- %s (%s)\n" (car conn) (ide-sql--product conn)) "-- running...\n")))
    (display-buffer buf)
    (ide-sql--history-add sql (car conn))
    (ide-sql--spawn
     conn sql mode
     (lambda (text status secs)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert (if (and (eq mode 'csv) (zerop status)) (ide-sql--csv-filter conn text) text))
             (goto-char (point-min))
             (setq header-line-format
                   (format " %s [%s]  %.2fs%s   g rerun  t table/csv  w copy CSV  e export"
                           (car conn) (ide-sql--product conn) secs
                           (if (zerop status) "" (format "  FAILED (exit %d)" status)))))))))))

(defun ide-sql-result-rerun ()
  "Run the statement of this result buffer again."
  (interactive)
  (ide-sql--show ide-sql--result-conn ide-sql--result-sql ide-sql--result-mode))

(defun ide-sql-result-toggle-csv ()
  "Show this result as CSV, or back as a table."
  (interactive)
  (ide-sql--show ide-sql--result-conn ide-sql--result-sql
                 (if (eq ide-sql--result-mode 'csv) 'table 'csv)))

(defun ide-sql--csv-for (sql conn callback)
  "Run SQL on CONN in CSV mode and call CALLBACK with the clean CSV text (errors are reported)."
  (ide-sql--spawn conn sql 'csv
                  (lambda (text status _secs)
                    (if (zerop status)
                        (funcall callback (ide-sql--csv-filter conn text))
                      (message "SQL failed: %s" (string-trim (car (split-string text "\n" t))))))))

(defun ide-sql--sql-and-conn ()
  "SQL and connection for the CSV commands: from the result buffer, or the statement at point."
  (if (derived-mode-p 'ide-sql-result-mode)
      (cons ide-sql--result-sql ide-sql--result-conn)
    (cons (ide-sql--dwim-text) (ide-sql--ensure-connection))))

(defun ide-sql-copy-csv ()
  "Copy the result of the statement (at point, or of this result buffer) as CSV."
  (interactive)
  (pcase-let ((`(,sql . ,conn) (ide-sql--sql-and-conn)))
    (ide-sql--csv-for sql conn (lambda (csv) (kill-new csv) (message "CSV copied (%d bytes)" (length csv))))))

(defun ide-sql-export-csv (file)
  "Write the result of the statement (at point, or of this result buffer) to FILE as CSV."
  (interactive (list (read-file-name "Export CSV to: " nil nil nil "result.csv")))
  (pcase-let ((`(,sql . ,conn) (ide-sql--sql-and-conn)))
    (ide-sql--csv-for sql conn (lambda (csv)
                                 (write-region (concat (string-trim-right csv "\n+") "\n") nil file)
                                 (message "Exported to %s" file)))))

;;;; Statements, regions, running ---------------------------------------------------------------------

(defun ide-sql--terminators ()
  "Positions just after each `;' outside strings and comments."
  (save-excursion
    (goto-char (point-min))
    (let (ends)
      (while (search-forward ";" nil t)
        (unless (nth 8 (save-excursion (syntax-ppss (1- (point))))) (push (point) ends)))
      (nreverse ends))))

(defun ide-sql--statement-bounds (&optional pos)
  "(BEG . END) of the statement around POS (default: point).
Between statements the one before applies.  `;' inside dollar-quoted bodies splits wrongly:
use the paragraph or the region there."
  (save-excursion
    (goto-char (or pos (point)))
    (let* ((p (point)) (ends (ide-sql--terminators))
           (end (or (cl-find-if (lambda (e) (>= e p)) ends) (point-max)))
           (beg (or (car (last (cl-remove-if (lambda (e) (>= e end)) ends))) (point-min))))
      (when (string-blank-p (buffer-substring-no-properties beg end))
        (when-let* ((prev (car (last (cl-remove-if (lambda (e) (>= e beg)) ends)))))
          (setq end beg beg prev)))
      (goto-char beg) (skip-chars-forward " \t\n")
      (cons (point) end))))

(defun ide-sql--paragraph-bounds ()
  (save-excursion
    (let (beg end)
      (backward-paragraph) (skip-chars-forward " \t\n") (setq beg (point))
      (forward-paragraph) (setq end (point))
      (cons beg end))))

(defun ide-sql--dwim-text ()
  "The active region, else the statement at point, as a string."
  (string-trim
   (if (use-region-p)
       (buffer-substring-no-properties (region-beginning) (region-end))
     (pcase-let ((`(,b . ,e) (ide-sql--statement-bounds)))
       (buffer-substring-no-properties b e)))))

(defun ide-sql--run-text (text &optional mode)
  (when (string-empty-p (string-trim text)) (user-error "Nothing to run"))
  (ide-sql--show (ide-sql--ensure-connection) text (or mode 'table)))

(defun ide-sql-run-dwim ()
  "Run the region, else the statement at point."
  (interactive)
  (ide-sql--run-text (ide-sql--dwim-text)))

(defun ide-sql-run-statement ()
  "Run the statement at point."
  (interactive)
  (pcase-let ((`(,b . ,e) (ide-sql--statement-bounds)))
    (ide-sql--run-text (buffer-substring-no-properties b e))))

(defun ide-sql-run-region (beg end)
  "Run the region."
  (interactive "r")
  (ide-sql--run-text (buffer-substring-no-properties beg end)))

(defun ide-sql-run-paragraph ()
  "Run the paragraph (text between blank lines) at point."
  (interactive)
  (pcase-let ((`(,b . ,e) (ide-sql--paragraph-bounds)))
    (ide-sql--run-text (buffer-substring-no-properties b e))))

(defun ide-sql-run-buffer ()
  "Run the whole buffer."
  (interactive)
  (ide-sql--run-text (buffer-substring-no-properties (point-min) (point-max))))

(defun ide-sql-run-file (file)
  "Run FILE (default: this buffer's file) on the connection, after confirmation."
  (interactive (list (read-file-name "Run SQL file: " nil nil t
                                     (and buffer-file-name (file-name-nondirectory buffer-file-name)))))
  (let ((conn (ide-sql--ensure-connection)))
    (when (y-or-n-p (format "Run %s on %s? " (file-name-nondirectory file) (car conn)))
      (ide-sql--show conn (with-temp-buffer (insert-file-contents file) (buffer-string)) 'table))))

(defun ide-sql-run-migrations (dir)
  "Run every .sql file of DIR in name order on the connection, after confirmation."
  (interactive
   (list (read-directory-name "Migrations directory: "
                              (let ((root (ide-lang-root)))
                                (cl-find-if #'file-directory-p
                                            (mapcar (lambda (d) (expand-file-name d root))
                                                    '("migrations" "db/migrations" "sql" "db")))))))
  (let ((files (directory-files dir t "\\.sql\\'")) (conn (ide-sql--ensure-connection)))
    (unless files (user-error "No .sql files in %s" dir))
    (when (y-or-n-p (format "Run %d files (%s ... %s) on %s? " (length files)
                            (file-name-nondirectory (car files))
                            (file-name-nondirectory (car (last files))) (car conn)))
      (ide-sql--show conn
                     (mapconcat (lambda (f) (with-temp-buffer (insert-file-contents f)
                                                              (string-trim (buffer-string))))
                                files "\n")
                     'table))))

;;;; Explain and schema -------------------------------------------------------------------------------

(defun ide-sql--explain-sql (product sql analyze)
  "SQL wrapped into the product's EXPLAIN (with ANALYZE when asked), or an error."
  (let ((s (string-trim-right (string-trim sql) ";+")))
    (pcase product
      ('sqlite (if analyze (concat "EXPLAIN " s ";") (concat "EXPLAIN QUERY PLAN " s ";")))
      ('postgres (concat (if analyze "EXPLAIN (ANALYZE, BUFFERS) " "EXPLAIN ") s ";"))
      ('mysql (concat (if analyze "EXPLAIN ANALYZE " "EXPLAIN ") s ";"))
      ('mariadb (concat (if analyze "ANALYZE " "EXPLAIN ") s ";"))
      ('ms (format "SET %s ON;\nGO\n%s;\nGO\n" (if analyze "STATISTICS PROFILE" "SHOWPLAN_TEXT") s))
      (_ (user-error "No EXPLAIN known for %s" product)))))

(defun ide-sql--explain (analyze)
  (let* ((conn (ide-sql--ensure-connection)) (sql (ide-sql--dwim-text)))
    (when (and analyze (not (eq (ide-sql--product conn) 'sqlite))
               (not (string-match-p "\\`\\(?:select\\|with\\)\\b" (downcase sql)))
               (not (y-or-n-p "ANALYZE executes the statement; continue? ")))
      (user-error "Cancelled"))
    (ide-sql--show conn (ide-sql--explain-sql (ide-sql--product conn) sql analyze) 'table)))

(defun ide-sql-explain ()
  "EXPLAIN the statement at point (the query plan, without running it)."
  (interactive) (ide-sql--explain nil))

(defun ide-sql-explain-analyze ()
  "EXPLAIN ANALYZE the statement at point (executes it; sqlite shows the opcodes)."
  (interactive) (ide-sql--explain t))

(defun ide-sql--valid-table (name)
  "NAME stripped of quotes, or a `user-error' when it is not a plain (qualified) identifier."
  (let ((n (replace-regexp-in-string "[\"`\\[]\\|]" "" name)))
    (unless (string-match-p "\\`[[:alnum:]_$]+\\(?:\\.[[:alnum:]_$]+\\)*\\'" n)
      (user-error "Not a table name: %s" name))
    n))

(defun ide-sql--describe-sql (product table)
  (let ((tbl (ide-sql--valid-table table)))
    (pcase product
      ('sqlite (format ".schema %s\nPRAGMA table_info('%s');" tbl tbl))
      ('postgres (format "\\d+ %s" tbl))
      ((or 'mysql 'mariadb) (format "DESCRIBE %s;\nSHOW CREATE TABLE %s;" tbl tbl))
      ('ms (format "EXEC sp_help '%s';\nGO" tbl))
      (_ (user-error "No describe for %s" product)))))

(defun ide-sql--tables-sql (product)
  (pcase product
    ('sqlite ".tables")
    ('postgres "\\d")
    ((or 'mysql 'mariadb) "SHOW FULL TABLES;")
    ('ms "SELECT TABLE_SCHEMA, TABLE_NAME, TABLE_TYPE FROM INFORMATION_SCHEMA.TABLES ORDER BY 1, 2;")
    (_ (user-error "No table listing for %s" product))))

(defun ide-sql--table-at-point ()
  (save-excursion
    (skip-chars-backward "[:alnum:]_.$\"`")
    (let ((b (point)))
      (skip-chars-forward "[:alnum:]_.$\"`")
      (buffer-substring-no-properties b (point)))))

(defun ide-sql-tables ()
  "List the tables and views of the connection."
  (interactive)
  (let ((conn (ide-sql--ensure-connection)))
    (ide-sql--show conn (ide-sql--tables-sql (ide-sql--product conn)) 'table)))

(defun ide-sql-describe-table (table)
  "Describe TABLE (default: the name at point): columns, types, DDL."
  (interactive (list (read-string "Describe table: " (ide-sql--table-at-point))))
  (let ((conn (ide-sql--ensure-connection)))
    (ide-sql--show conn (ide-sql--describe-sql (ide-sql--product conn) table) 'table)))

(defun ide-sql--sqlcmd-wrapper ()
  "A launcher for sqlcmd that drops the bare trailing -P `sql.el' appends for an empty password.
The real password travels in SQLCMDPASSWORD."
  (let ((file (ide-lang-tools-dir "sql" "sqlcmd-wrapper")))
    (unless (file-executable-p file)
      (with-temp-file file
        (insert "#!/bin/sh\nn=$#; i=0\nfor a in \"$@\"; do i=$((i+1))\n"
                "  if [ $i -lt $n ] || [ \"$a\" != \"-P\" ]; then set -- \"$@\" \"$a\"; fi\ndone\n"
                "shift $n\nexec " (shell-quote-argument (ide-sql--program 'ms)) " \"$@\"\n"))
      (set-file-modes file #o700))
    file))

;; sql.el sends no GO for sqlcmd unless asked; without it nothing executes.
(defvar sql-send-terminator)
(add-hook 'sql-interactive-mode-hook
          (lambda () (when (eq (ide-sql--product) 'ms) (setq-local sql-send-terminator t))))

(defun ide-sql--repl-entry (conn)
  "A `sql-connection-alist' entry for CONN without its password."
  (let* ((e (ide-sql--entry conn))
         (ms (eq (ide-sql--product conn) 'ms))
         (items (cl-remove-if (lambda (i) (memq (car i) (if ms '(sql-password sql-port) '(sql-password))))
                              (cdr e))))
    (when (and ms (ide-sql--get conn 'server) (ide-sql--get conn 'port))
      (setf (cadr (assq 'sql-server items))
            (format "%s,%s" (ide-sql--get conn 'server) (ide-sql--get conn 'port))))
    (cons (car e) items)))

(defun ide-sql-repl ()
  "Open the interactive client (*SQL*) for this buffer's connection.
Passwords go to the client through its environment (never argv or a prompt)."
  (interactive)
  (let* ((conn (ide-sql--ensure-connection))
         (product (ide-sql--product conn))
         (spec-env (cdr (ide-sql--spec conn 'table)))
         (process-environment (append spec-env process-environment))
         (sql-connection-alist (list (ide-sql--repl-entry conn)))
         (sql-mysql-login-params '(user database server))
         (sql-mariadb-login-params '(user database server))
         (sql-ms-login-params '(user server database))
         (sql-ms-options (if (ide-sql--mssql-trust-p (ide-sql--get conn 'server)) '("-C") nil))
         (sql-ms-program (if (eq product 'ms) (ide-sql--sqlcmd-wrapper) sql-ms-program))
         (sql-mysql-program (if (eq product 'mysql) (ide-sql--program 'mysql) sql-mysql-program))
         (sql-mariadb-program (if (eq product 'mariadb) (ide-sql--program 'mariadb) sql-mariadb-program)))
    (sql-connect (car conn))))

;;;; History ------------------------------------------------------------------------------------------------

(defvar ide-sql--history 'unloaded "List of (SQL . CONNECTION), newest first.")

(defun ide-sql--history-file () (expand-file-name "sql-history.eld" ide-var-dir))

(defun ide-sql--history-load ()
  (when (eq ide-sql--history 'unloaded)
    (setq ide-sql--history (let ((h (ide-sql--read-file (ide-sql--history-file)))) (if (listp h) h nil))))
  ide-sql--history)

(defun ide-sql--history-add (sql conn)
  "Remember SQL run on CONN, newest first, without duplicates.
Statements that mention passwords are not kept."
  (ide-sql--history-load)
  (unless (string-match-p "password\\|identified by\\|secret" sql)
   (setq ide-sql--history
        (seq-take (cons (cons sql conn) (cl-remove sql ide-sql--history :key #'car :test #'equal))
                  ide-sql-history-size))
   (ignore-errors
    (with-temp-file (ide-sql--history-file)
      (let ((print-length nil)) (prin1 ide-sql--history (current-buffer)))))))

(defun ide-sql-history ()
  "Insert a statement from the history at point."
  (interactive)
  (let* ((h (ide-sql--history-load))
         (cands (mapcar (lambda (e) (cons (format "%s  [%s]" (replace-regexp-in-string "[ \t\n]+" " " (car e)) (cdr e)) (car e))) h)))
    (unless cands (user-error "No history yet"))
    (insert (cdr (assoc (completing-read "History: " (mapcar #'car cands) nil t) cands)) "\n")))

;;;; Formatting (sql-formatter), fixing and parsing (sqlfluff) ---------------------------------------------------

(defun ide-sql--formatter-args ()
  (list "-l" (ide-sql--formatter-language)
        "-c" (json-serialize `(:tabWidth 2 :keywordCase ,ide-sql-keyword-case))))

(defun ide-sql--filter-region (beg end program args)
  "Pipe the text of BEG..END through PROGRAM ARGS and return the output, or signal on failure."
  (let ((src (current-buffer)) (err (let ((temporary-file-directory ide-cache-dir)) (make-temp-file "sql-err"))))
    (unwind-protect
        (with-temp-buffer
          (let* ((tmp (current-buffer))
                 (status (let ((default-directory (ide-lang-root)))
                           (with-current-buffer src
                             (apply #'call-process-region beg end program nil (list tmp err) nil args)))))
            (if (and (eq status 0) (> (buffer-size) 0))
                (buffer-string)
              (error "%s: %s" (file-name-nondirectory program)
                     (let ((text (with-temp-buffer (insert-file-contents err) (buffer-string))))
                       (string-trim (if (string-match "\\(?:Parse \\)?[Ee]rror:[^\n]*" text)
                                        (match-string 0 text)
                                      (substring text 0 (min (length text) 300)))))))))
      (delete-file err))))

(defun ide-sql--replace-with (beg end text)
  "Replace BEG..END with TEXT, touching only what changed (point and marks stay put)."
  (unless (equal text (buffer-substring-no-properties beg end))
    (replace-region-contents beg end (lambda () text))))

(defun ide-sql-format (&optional beg end)
  "Format the region or buffer with sql-formatter, in the dialect of `sql-product'."
  (interactive (if (use-region-p) (list (region-beginning) (region-end)) (list (point-min) (point-max))))
  (let* ((exe (or (ide-lang-node-exe "sql-formatter") (user-error "sql-formatter not found; run M-x ide-mise-setup")))
         (beg (or beg (point-min))) (end (or end (point-max)))
         (to-end (= end (point-max)))
         (out (ide-sql--filter-region beg end exe (ide-sql--formatter-args))))
    (ide-sql--replace-with beg end (string-trim-right out "\n+"))
    (when to-end (save-excursion (goto-char (point-max)) (unless (bolp) (insert "\n"))))))

(defun ide-sql--formatter-exe ()
  (or (ide-lang-node-exe "sql-formatter") (user-error "sql-formatter not found; run M-x ide-mise-setup")))

(defvar apheleia-formatters)
(defvar apheleia-mode-alist)
(defvar apheleia-inhibit)

;; Apheleia (format on save, `ide-reformat-buffer') runs the same sql-formatter command.
(with-eval-after-load 'apheleia
  (setf (alist-get 'ide-sql apheleia-formatters)
        '((ide-sql--formatter-exe) (ide-sql--formatter-args)))
  (setf (alist-get 'sql-mode apheleia-mode-alist) 'ide-sql))

(defun ide-sql--sqlfluff-project-config ()
  "Path of the .sqlfluff above this buffer's file, or nil."
  (when-let* ((dir (locate-dominating-file (or buffer-file-name default-directory) ".sqlfluff")))
    (expand-file-name ".sqlfluff" dir)))

(defun ide-sql--sqlfluff-config-p ()
  "Non-nil when the project .sqlfluff sets a dialect (then we do not pass one)."
  (when-let* ((file (ide-sql--sqlfluff-project-config)))
    (with-temp-buffer
      (insert-file-contents file)
      (re-search-forward "^[ \t]*dialect[ \t]*=" nil t))))

(defun ide-sql--sqlfluff-default-config ()
  "A default sqlfluff config in the sandbox: 2-space indent."
  (let ((file (ide-lang-tools-dir "sqlfluff" "default.cfg")))
    (unless (file-exists-p file)
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "[sqlfluff:indentation]\ntab_space_size = 2\nindent_unit = space\n")))
    file))

(defun ide-sql--sqlfluff-args (verb file)
  "Arguments of `sqlfluff VERB' reading stdin for FILE.
A project .sqlfluff wins; without one the default config and the dialect of `sql-product' apply.
Linting leaves LT02 (indentation) to sql-formatter, whose layout differs from sqlfluff's."
  (append (list verb)
          (when (equal verb "lint") '("--format" "json" "--exclude-rules" "LT02"))
          (unless (equal verb "parse") '("--disable-progress-bar"))
          (unless (ide-sql--sqlfluff-project-config)
            (list "--config" (ide-sql--sqlfluff-default-config)))
          (unless (ide-sql--sqlfluff-config-p) (list "--dialect" (ide-sql--sqlfluff-dialect)))
          (when file (list "--stdin-filename" file))
          '("-")))

(defun ide-sql--parse-sqlfluff (output buffer)
  "Flymake diagnostics from sqlfluff's JSON OUTPUT for BUFFER."
  (when (string-match "^\\[" output)
    (let ((json (json-parse-string (substring output (match-beginning 0))
                                   :object-type 'alist :array-type 'list))
          diags)
      (dolist (file json)
        (dolist (v (alist-get 'violations file))
          (let ((code (alist-get 'code v)))
            (push (ide-lang-diag buffer "sqlfluff" (alist-get 'start_line_no v) (alist-get 'start_line_pos v)
                                 (if (equal code "PRS") :error :warning)
                                 (format "%s: %s" code (alist-get 'description v))
                                 (when (eq (alist-get 'start_line_no v) (alist-get 'end_line_no v))
                                   (alist-get 'end_line_pos v)))
                  diags))))
      (nreverse diags))))

(ide-lang-define-linter
 "sqlfluff" :modes '(sql-mode) :program "sqlfluff" :stdin t
 :args (lambda (file) (ide-sql--sqlfluff-args "lint" file))
 :parse #'ide-sql--parse-sqlfluff)

;; Deliberately not `:safe': a project .sqlfluff can load Python (Jinja templater
;; library_path, plugins).  It pauses until the project is trusted
;; (M-x ide-lang-trust-project); see `ide-lang-define-linter'.

(defun ide-sql-fix ()
  "Fix the buffer with `sqlfluff fix' (layout, capitalisation, ...), keeping point."
  (interactive)
  (let ((exe (ide-lang-exe-or-error "sqlfluff")))
    (ide-sql--replace-with
     (point-min) (point-max)
     (ide-sql--filter-region (point-min) (point-max) exe
                             (ide-sql--sqlfluff-args "fix" buffer-file-name)))))

(defun ide-sql-parse ()
  "Show sqlfluff's parse tree of the region or buffer."
  (interactive)
  (let* ((exe (ide-lang-exe-or-error "sqlfluff"))
         (beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (text (ide-sql--filter-region beg end exe (ide-sql--sqlfluff-args "parse" nil)))
         (buf (get-buffer-create "*run: sqlfluff parse*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer) (insert text) (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

;;;; Templates -----------------------------------------------------------------------------------------------

(defconst ide-sql-templates
  '(("select" . "SELECT ${1:*}\nFROM ${2:table}\nWHERE ${3:condition};")
    ("join" . "SELECT ${1:a.*}\nFROM ${2:a}\nJOIN ${3:b} ON ${3:b}.${4:a_id} = ${2:a}.id\nWHERE ${5:true};")
    ("insert" . "INSERT INTO ${1:table} (${2:columns})\nVALUES (${3:values});")
    ("update" . "UPDATE ${1:table}\nSET ${2:column} = ${3:value}\nWHERE ${4:condition};")
    ("delete" . "DELETE FROM ${1:table}\nWHERE ${2:condition};")
    ("create table" . "CREATE TABLE ${1:name} (\n  id INTEGER PRIMARY KEY,\n  ${2:column} ${3:TEXT} NOT NULL\n);")
    ("create index" . "CREATE INDEX ${1:idx_name} ON ${2:table} (${3:column});")
    ("alter add column" . "ALTER TABLE ${1:table} ADD COLUMN ${2:column} ${3:TEXT};")
    ("cte" . "WITH ${1:name} AS (\n  SELECT ${2:*} FROM ${3:table}\n)\nSELECT * FROM ${1:name};")
    ("group count" . "SELECT ${1:column}, COUNT(*) AS n\nFROM ${2:table}\nGROUP BY ${1:column}\nORDER BY n DESC;")
    ("window row_number" . "SELECT *, ROW_NUMBER() OVER (PARTITION BY ${1:key} ORDER BY ${2:ts} DESC) AS rn\nFROM ${3:table};"))
  "Query templates for `ide-sql-insert-template': name -> yasnippet-style text.")

(defun ide-sql--template-plain (template)
  "TEMPLATE without field markers, as plain text."
  (replace-regexp-in-string "\\$[0-9]+" ""
                            (replace-regexp-in-string "\\${[0-9]+:\\([^}]*\\)}" "\\1" template)))

(defun ide-sql-insert-template (name)
  "Insert the query template NAME at point (a yasnippet with fields when available)."
  (interactive (list (completing-read "Template: " (mapcar #'car ide-sql-templates) nil t)))
  (let ((tpl (cdr (assoc name ide-sql-templates))))
    (if (and (require 'yasnippet nil t) (fboundp 'yas-expand-snippet))
        (yas-expand-snippet tpl)
      (insert (ide-sql--template-plain tpl)))))

;;;; Docs, server --------------------------------------------------------------------------------------------------

(defun ide-sql-docs ()
  "Open the SQL reference of this buffer's product in the in-Emacs browser."
  (interactive)
  (ide-lang-browse
   (pcase (ide-sql--product)
     ('sqlite "https://www.sqlite.org/lang.html")
     ('postgres "https://www.postgresql.org/docs/current/sql-commands.html")
     ('mysql "https://dev.mysql.com/doc/refman/en/sql-statements.html")
     ('mariadb "https://mariadb.com/kb/en/sql-statements/")
     ('ms "https://learn.microsoft.com/en-us/sql/t-sql/language-reference")
     (_ "https://en.wikipedia.org/wiki/SQL_syntax"))))

(defun ide-sql-rule-docs (code)
  "Open the sqlfluff documentation of rule CODE (default: the one at point's diagnostic)."
  (interactive
   (list (read-string "Rule: "
                      (when-let* ((d (car (flymake-diagnostics (point)))))
                        (and (string-match "\\b\\([A-Z][A-Z][0-9][0-9]\\)\\b" (flymake-diagnostic-text d))
                             (match-string 1 (flymake-diagnostic-text d)))))))
  (ide-lang-browse (format "https://docs.sqlfluff.com/en/stable/reference/rules.html#rule-%s" code)))

;;;; sqls workspace configuration ----------------------------------------------------------------------------------------

(defun ide-sql--sqls-connection (conn)
  "The sqls connection plist for the normalized CONN, or nil for unsupported products."
  (let ((db (ide-sql--get conn 'database)) (host (or (ide-sql--get conn 'server) "localhost"))
        (port (ide-sql--get conn 'port)) (user (ide-sql--get conn 'user)) (pw (ide-sql--get conn 'password)))
    (pcase (ide-sql--product conn)
      ('sqlite (and db (list :alias (car conn) :driver "sqlite3" :dataSourceName db)))
      ('postgres
       (list :alias (car conn) :driver "postgresql"
             :dataSourceName
             (string-join (delq nil (list (format "host=%s" host) (and port (format "port=%s" port))
                                          (and user (format "user=%s" user)) (and pw (format "password=%s" pw))
                                          (and db (format "dbname=%s" db)) "sslmode=disable"))
                          " ")))
      ((or 'mysql 'mariadb)
       (list :alias (car conn) :driver "mysql"
             :dataSourceName (format "%s%s@tcp(%s:%s)/%s" (or user "root") (if pw (concat ":" pw) "")
                                     host (or port "3306") (or db ""))))
      ('ms
       (list :alias (car conn) :driver "mssql"
             :dataSourceName (format "sqlserver://%s%s@%s:%s?database=%s&TrustServerCertificate=true"
                                     (url-hexify-string (or user "sa"))
                                     (if pw (concat ":" (url-hexify-string pw)) "") host (or port "1433") (or db "")))))))

(defun ide-sql--sqls-settings (root)
  "The `:sqls' settings for the project at ROOT: the active connection first."
  (let* ((all (ide-sql--connections root))
         (active (alist-get root ide-sql--lsp-choice nil nil #'equal))
         (ordered (if (assoc active all) (cons (assoc active all) (cl-remove active all :key #'car :test #'equal)) all))
         (conns (delq nil (mapcar #'ide-sql--sqls-connection ordered))))
    (list :connections (vconcat conns))))

(defun ide-sql--workspace-config (server)
  "Eglot workspace configuration for sqls: the shared base plus this project's connections."
  (let* ((root (file-name-as-directory (expand-file-name default-directory)))
         (base (default-value 'eglot-workspace-configuration))
         (base (if (functionp base) (funcall base server) base)))
    (plist-put (copy-sequence base) :sqls (ide-sql--sqls-settings (ide-lang-root root)))))

(dir-locals-set-class-variables
 'ide-sql '((sql-mode . ((eglot-workspace-configuration . ide-sql--workspace-config)))))

(with-eval-after-load 'eglot
  ;; Dir-local values must be safe to apply unprompted; ours is a function of this file.
  (let ((old (get 'eglot-workspace-configuration 'safe-local-variable)))
    (put 'eglot-workspace-configuration 'safe-local-variable
         (lambda (v) (or (eq v 'ide-sql--workspace-config) (and old (funcall old v)))))))

(defun ide-sql--register-dir-class ()
  "Let Eglot ask `ide-sql--workspace-config' for sqls settings in this project."
  (dir-locals-set-directory-class (ide-lang-root) 'ide-sql))

(defun ide-sql-restart-server ()
  "Restart sqls (after editing the connection file)."
  (interactive)
  (ide-sql--sync-connections)
  (when-let* ((s (eglot-current-server))) (eglot-shutdown s))
  (eglot-ensure))

;;;; Run hook, keys, menu ----------------------------------------------------------------------------------------------------------

(defconst ide-sql--secret-env-regexp "\\`\\(?:PGPASSWORD\\|MYSQL_PWD\\|SQLCMDPASSWORD\\)="
  "Environment entries that carry a password.")

(defun ide-sql--delete-stale-secrets ()
  "Remove password files of command lines that were built but never run."
  (dolist (f (directory-files ide-cache-dir t "\\`sql-pw"))
    (when (> (float-time (time-since (file-attribute-modification-time (file-attributes f)))) 600)
      (delete-file f))))

(defun ide-sql--shell-command (argv env file)
  "A shell line running ARGV with ENV on stdin FILE, never containing a password.
Secrets are written to 0600 files that the line reads and removes itself."
  (let (secret-files)
    (ide-sql--delete-stale-secrets)
    (let* ((assigns
            (mapcar (lambda (e)
                      (if (string-match-p ide-sql--secret-env-regexp e)
                          (let* ((name (substring e 0 (string-search "=" e)))
                                 (f (let ((temporary-file-directory ide-cache-dir)) (make-temp-file "sql-pw"))))
                            (write-region (substring e (1+ (string-search "=" e))) nil f nil 'silent)
                            (push f secret-files)
                            (format "%s=\"$(cat %s)\"" name (shell-quote-argument f)))
                        (let ((i (string-search "=" e)))
                          (concat (substring e 0 (1+ i)) (shell-quote-argument (substring e (1+ i)))))))
                    env))
           (cmd (concat (mapconcat #'identity assigns " ") (if assigns " " "")
                        (mapconcat #'shell-quote-argument argv " ")
                        " < " (shell-quote-argument file))))
      (if secret-files
          (format "%s; rc=$?; rm -f %s; exit $rc" cmd
                  (mapconcat #'shell-quote-argument secret-files " "))
        cmd))))

(defun ide-sql--run-command ()
  "Shell command running this file on the connection (S-<f10>)."
  (when (and (derived-mode-p 'sql-mode) buffer-file-name)
    (when-let* ((conn (ignore-errors (ide-sql--ensure-connection))))
      (pcase-let ((`(,argv . ,env) (ide-sql--spec conn 'table)))
        (ide-sql--shell-command argv env buffer-file-name)))))

(add-hook 'ide-run-command-functions #'ide-sql--run-command)

(with-eval-after-load 'sql
  (define-key sql-mode-map (kbd "C-c C-c") #'ide-sql-run-dwim)
  (define-key sql-mode-map (kbd "C-c C-e") #'ide-sql-explain)
  (define-key sql-mode-map (kbd "C-c C-d") #'ide-sql-describe-table)
  (define-key sql-mode-map (kbd "C-c C-o") #'ide-sql-connect))

;;;###autoload (autoload 'ide-sql-dispatch "lang-sql" nil t)
(transient-define-prefix ide-sql-dispatch ()
  "SQL: connect, run, explain, schema, format, lint."
  [["Connection"
    ("c" "choose" ide-sql-connect)
    ("a" "add (project file)" ide-sql-add-connection)
    ("E" "edit file" ide-sql-edit-connections)
    ("i" "interactive client" ide-sql-repl)]
   ["Run"
    ("r" "statement / region" ide-sql-run-dwim)
    ("p" "paragraph" ide-sql-run-paragraph)
    ("b" "buffer" ide-sql-run-buffer)
    ("f" "file..." ide-sql-run-file)
    ("m" "migrations folder" ide-sql-run-migrations)
    ("h" "history" ide-sql-history)]
   ["Explain / schema"
    ("e" "explain" ide-sql-explain)
    ("A" "explain analyze" ide-sql-explain-analyze)
    ("t" "tables" ide-sql-tables)
    ("d" "describe table" ide-sql-describe-table)
    ("L" "sql-list-all (client)" sql-list-all)
    ("T" "sql-list-table (client)" sql-list-table)
    ("w" "copy result as CSV" ide-sql-copy-csv)
    ("x" "export CSV" ide-sql-export-csv)]
   ["Quality"
    ("F" "format" ide-sql-format)
    ("X" "sqlfluff fix" ide-sql-fix)
    ("P" "sqlfluff parse" ide-sql-parse)
    ("l" "flymake diagnostics" flymake-show-buffer-diagnostics)
    ("R" "rule docs" ide-sql-rule-docs)]
   ["Write / docs / server"
    ("s" "insert template" ide-sql-insert-template)
    ("D" "product docs" ide-sql-docs)
    ("S" "restart sqls" ide-sql-restart-server)]])

(ide-lang-register-menu '(sql-mode) #'ide-sql-dispatch)

(provide 'lang-sql)
;;; lang-sql.el ends here
