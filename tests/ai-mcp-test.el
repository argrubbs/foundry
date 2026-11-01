;;; ai-mcp-test.el --- Tests for the emcp and elisp-dev-mcp integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/ai-mcp-test.el
;; Real servers, no network beyond localhost: emcp is started on a free port
;; and spoken to over HTTP; elisp-dev-mcp is driven through mcp-server-lib's
;; JSON-RPC entry point, as its stdio script does.  No Emacs server is started.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'url)

(defun ai-mcp-test--free-port ()
  "A localhost TCP port nothing listens on."
  (let* ((proc (make-network-process :name "ai-mcp-test-port" :server t
                                     :host "127.0.0.1" :service t))
         (port (process-contact proc :service)))
    (delete-process proc)
    port))

(defun ai-mcp-test--post (url body &optional session)
  "POST JSON BODY to URL; return (SESSION-ID . parsed JSON reply)."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         (append '(("Content-Type" . "application/json")
                   ("Accept" . "application/json, text/event-stream"))
                 (and session `(("Mcp-Session-Id" . ,session)))))
        (url-request-data (encode-coding-string (json-encode body) 'utf-8)))
    (with-current-buffer (url-retrieve-synchronously url t t 10)
      (unwind-protect
          (progn
            (goto-char (point-min))
            (let ((sid (and (re-search-forward "^mcp-session-id: *\\(.+?\\)\r?$" nil t)
                            (match-string 1))))
              (goto-char (point-min))
              (re-search-forward "^\r?$")
              ;; Replies may come as plain JSON or as one server-sent event;
              ;; notifications get an empty 202.
              (re-search-forward "^data: " nil t)
              (let ((json (string-trim (buffer-substring (point) (point-max)))))
                (cons sid (and (not (string-empty-p json))
                               (json-parse-string json :object-type 'alist :array-type 'list))))))
        (kill-buffer)))))

(defmacro ai-mcp-test--with-emcp (url-var &rest body)
  "Start emcp on a free port, bind URL-VAR to its URL, run BODY, stop it.
`emcp-http-port', which starting sets, is restored afterwards."
  (declare (indent 1))
  `(progn
     (require 'emcp)
     (let ((ide-ai-emcp-port (ai-mcp-test--free-port)) (ide-ai-emcp-profile 'inspect)
           (emcp-http-port emcp-http-port))
       (cl-letf (((symbol-function 'ide-ai--mcp-ensure-server) (lambda () "test")))
         (ide-ai-mcp-start)
         (unwind-protect (let ((,url-var (ide-ai--emcp-url))) ,@body)
           (ide-ai-mcp-stop))))))

(defun ai-mcp-test--raw-status (port host &optional origin)
  "Send a raw initialize POST to emcp on PORT with HOST (and ORIGIN); return the HTTP status."
  (let* ((body (json-encode '((jsonrpc . "2.0") (id . 1) (method . "initialize")
                             (params . ((protocolVersion . "2025-11-25") (capabilities . ())
                                        (clientInfo . ((name . "test") (version . "0"))))))))
         (out "")
         (proc (make-network-process :name "ai-mcp-test-raw" :host "127.0.0.1" :service port
                                     :coding 'binary :filter (lambda (_p s) (setq out (concat out s))))))
    (unwind-protect
        (progn
          (process-send-string
           proc (concat "POST /mcp HTTP/1.1\r\nHost: " host "\r\n"
                        (if origin (concat "Origin: " origin "\r\n") "")
                        "Content-Type: application/json\r\nAccept: application/json, text/event-stream\r\n"
                        (format "Content-Length: %d\r\nConnection: close\r\n\r\n" (string-bytes body)) body))
          (with-timeout (10 (error "No reply"))
            (while (not (string-match-p "\\`HTTP/1\\.1 [0-9]+ " out)) (accept-process-output proc 0.1)))
          (string-match "\\`HTTP/1\\.1 \\([0-9]+\\) " out)
          (string-to-number (match-string 1 out)))
      (delete-process proc))))

;;;; emcp ------------------------------------------------------------------------------

(ert-deftest ai-mcp-emcp-serves-inspect-tools-on-the-configured-port ()
  (ai-mcp-test--with-emcp url
    (should (string-match-p (format ":%d/mcp\\'" ide-ai-emcp-port) url))
    (let* ((init (ai-mcp-test--post url `((jsonrpc . "2.0") (id . 1) (method . "initialize")
                                          (params . ((protocolVersion . "2025-11-25")
                                                     (capabilities . ,(make-hash-table))
                                                     (clientInfo . ((name . "test") (version . "0"))))))))
           (sid (car init)))
      (should (alist-get 'serverInfo (alist-get 'result (cdr init))))
      (ai-mcp-test--post url '((jsonrpc . "2.0") (method . "notifications/initialized")) sid)
      (let* ((tools (cdr (ai-mcp-test--post url '((jsonrpc . "2.0") (id . 2) (method . "tools/list")) sid)))
             (names (mapcar (lambda (tl) (alist-get 'name tl)) (alist-get 'tools (alist-get 'result tools)))))
        (should (member "describe" names))
        (should (member "find-references" names))
        ;; inspect is read-only: no eval or key sending.
        (should-not (member "eval" names))
        (should-not (member "send-keys" names))))))

(ert-deftest ai-mcp-emcp-stops-cleanly ()
  (let ((port emcp-http-port))
    (ai-mcp-test--with-emcp url (should url))
    (should-not (ide-ai--emcp-url))
    (should (equal emcp-http-port port))))

(ert-deftest ai-mcp-emcp-stop-stops-the-profile-it-started ()
  (ai-mcp-test--with-emcp url
    (should url)
    (setq ide-ai-emcp-profile 'develop)          ; changed after starting
    (ide-ai-mcp-stop)
    (should-not (alist-get 'inspect emcp--servers))
    (should-not (ide-ai--emcp-url))
    (let (said)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (ide-ai-mcp-stop))
      (should (string-match-p "not running" said)))))

(ert-deftest ai-mcp-emcp-answers-only-localhost-requests ()
  (ai-mcp-test--with-emcp url
    (let ((port ide-ai-emcp-port))
      (should (string-match-p (format ":%d/" port) url))
      (should (= 201 (ai-mcp-test--raw-status port (format "127.0.0.1:%d" port))))
      (should (= 201 (ai-mcp-test--raw-status port (format "localhost:%d" port) "http://localhost:3000")))
      ;; 201 Created is the answer to initialize.  DNS rebinding: a page on
      ;; evil.example resolving to 127.0.0.1, or any foreign page, gets 403.
      (should (= 403 (ai-mcp-test--raw-status port (format "evil.example:%d" port))))
      (should (= 403 (ai-mcp-test--raw-status port (format "127.0.0.1:%d" (1+ port)))))
      (should (= 403 (ai-mcp-test--raw-status port (format "127.0.0.1:%d" port) "http://evil.example")))
      (should (= 403 (ai-mcp-test--raw-status port (format "127.0.0.1:%d" port) "null"))))))

;;;; elisp-dev-mcp -----------------------------------------------------------------------

(defun ai-mcp-test--elisp-dev-call (tool args)
  "Call elisp-dev-mcp TOOL with ARGS; return the text of its reply."
  (let* ((reply (json-parse-string
                 (mcp-server-lib-process-jsonrpc
                  (mcp-server-lib-create-tools-call-request tool 1 args) "elisp-dev-mcp")
                 :object-type 'alist :array-type 'list))
         (result (alist-get 'result reply)))
    (should result)
    (should-not (eq (alist-get 'isError result) t))
    (alist-get 'text (car (alist-get 'content result)))))

(ert-deftest ai-mcp-elisp-dev-answers-and-reads-elpaca-sources ()
  (ide-ai-mcp-elisp-dev-enable)
  (unwind-protect
      (progn
        (should (string-match-p "mapcar" (ai-mcp-test--elisp-dev-call
                                          "elisp-describe-function" '(("function" . "mapcar")))))
        ;; Elpaca-installed source is readable because its trees are allowed.
        (should (string-match-p "define-minor-mode corfu-mode"
                                (ai-mcp-test--elisp-dev-call
                                 "elisp-read-source-file" '(("library-or-path" . "corfu"))))))
    (elisp-dev-mcp-disable)))

(defmacro ai-mcp-test--with-user-fn (&rest body)
  "Run BODY with a fake config dir whose user/secret.el defines and uses
`ai-mcp-test-user-fn', and lisp/public.el defines `ai-mcp-test-public-fn'."
  `(let* ((user-emacs-directory (file-name-as-directory (make-temp-file "cfg" t)))
          (secret (expand-file-name "user/ai-mcp-test-secret.el" user-emacs-directory))
          (public (expand-file-name "lisp/ai-mcp-test-public.el" user-emacs-directory)))
     (unwind-protect
         (progn
           (make-directory (file-name-directory secret) t)
           (make-directory (file-name-directory public) t)
           (with-temp-file secret
             (insert ";;; ai-mcp-test-secret.el --- x -*- lexical-binding: t; -*-\n;;; Commentary:\n;; hunter2 lives here\n;;; Code:\n"
                     "(defun ai-mcp-test-user-fn () \"x\" \"hunter2\")\n(defun ai-mcp-test-user-caller () (ai-mcp-test-public-fn))\n"
                     "(provide 'ai-mcp-test-secret)\n"))
           (with-temp-file public (insert ";;; -*- lexical-binding: t; -*-\n(defun ai-mcp-test-public-fn () \"x\" 42)\n"))
           (load secret nil t) (load public nil t)
           ,@body)
       (delete-directory user-emacs-directory t))))

(ert-deftest ai-mcp-elisp-dev-cannot-read-user-config ()
  (ide-ai-mcp-elisp-dev-enable)
  (unwind-protect
      (ai-mcp-test--with-user-fn
       (should-not (cl-some (lambda (d) (string-prefix-p (expand-file-name "user/" user-emacs-directory)
                                                         (expand-file-name d)))
                            elisp-dev-mcp-additional-allowed-dirs))
       (should (string-match-p "42" (ai-mcp-test--elisp-dev-call
                                     "elisp-get-function-definition" '(("function" . "ai-mcp-test-public-fn")))))
       (let* ((reply (json-parse-string
                      (mcp-server-lib-process-jsonrpc
                       (mcp-server-lib-create-tools-call-request
                        "elisp-get-function-definition" 1 '(("function" . "ai-mcp-test-user-fn")))
                       "elisp-dev-mcp")
                      :object-type 'alist :array-type 'list))
              (text (format "%S" reply)))
         (should (string-match-p "withheld" text))
         (should-not (string-match-p "hunter2" text))))
    (elisp-dev-mcp-disable)))

(defun ai-mcp-test--emcp-tool (fn args)
  "Call emcp tool handler FN with ARGS (an alist); return its text."
  (let ((h (make-hash-table :test 'equal)) result)
    (dolist (a args) (puthash (car a) (cdr a) h))
    (funcall fn nil nil (lambda (r) (setq result r)) #'ignore h)
    (mapconcat (lambda (c) (alist-get 'text c)) (alist-get 'content result) "\n")))

(ert-deftest ai-mcp-emcp-cannot-read-user-config ()
  (require 'emcp)
  (require 'emcp-tools)
  (ai-mcp-test--with-user-fn
   (should (string-match-p "42" (ai-mcp-test--emcp-tool #'emcp-tools-find-definition
                                                        '(("symbol" . "ai-mcp-test-public-fn")))))
   (let ((text (ai-mcp-test--emcp-tool #'emcp-tools-find-definition '(("symbol" . "ai-mcp-test-user-fn")))))
     (should (string-match-p "withheld" text))
     (should-not (string-match-p "hunter2" text)))
   ;; nor its commentary, through describe
   (let ((load-path (cons (file-name-directory secret) load-path)))
     (let ((text (ai-mcp-test--emcp-tool #'emcp-tools-describe '(("symbol" . "ai-mcp-test-secret") ("kind" . "feature")))))
       (should (string-match-p "withheld" text))
       (should-not (string-match-p "hunter2" text))))
   ;; references from user/ are not listed either
   (let ((text (ai-mcp-test--emcp-tool #'emcp-tools-find-references '(("symbol" . "ai-mcp-test-public-fn")))))
     (should-not (string-match-p "ai-mcp-test-user-caller\\|ai-mcp-test-secret" text)))))

;;;; Emacs server -------------------------------------------------------------------------------

(defmacro ai-mcp-test--server (live &rest body)
  "Run BODY as if another Emacs owns every server name; this one serves only when LIVE.
`server-start' is stubbed: it starts nothing unless LIVE is `on-start'."
  (declare (indent 1))
  `(let ((server-name "server") (server-process nil) (ide-ai-mcp-server-name "mcgeemacs-test") dummy)
     (require 'server)
     (unwind-protect
         (cl-letf (((symbol-function 'server-running-p) (lambda (&rest _) t))
                   ((symbol-function 'server-start)
                    (lambda (&rest _)
                      (when (eq ,live 'on-start)
                        (setq dummy (make-network-process :name "ai-mcp-test-server" :server t
                                                          :host "127.0.0.1" :service t)
                              server-process dummy)))))
           (when (eq ,live t)
             (setq dummy (make-network-process :name "ai-mcp-test-server" :server t
                                               :host "127.0.0.1" :service t)
                   server-process dummy server-name "mine"))
           ,@body)
       (when dummy (delete-process dummy)))))

(ert-deftest ai-mcp-server-is-judged-by-this-emacs ()
  ;; Another Emacs owns the name and server-start only warns: a clear error.
  (ai-mcp-test--server nil
    (should-error (ide-ai--mcp-ensure-server))
    (should (equal server-name "server"))
    ;; registration names the socket this Emacs will serve
    (should (member "--socket=mcgeemacs-test" (cadr (ide-ai-mcp-claude-commands)))))
  ;; Starting works: our name.
  (ai-mcp-test--server 'on-start
    (should (equal (ide-ai--mcp-ensure-server) "mcgeemacs-test")))
  ;; Already serving: keep and report our own name.
  (ai-mcp-test--server t
    (should (equal (ide-ai--mcp-ensure-server) "mine"))
    (should (member "--socket=mine" (cadr (ide-ai-mcp-claude-commands))))))

;;;; Registration and startup -------------------------------------------------------------

(ert-deftest ai-mcp-claude-commands-point-at-real-endpoints ()
  (let* ((ide-ai-mcp-server-name "mcgeemacs-test")
         (cmds (ide-ai-mcp-claude-commands))
         (http (car cmds)) (stdio (cadr cmds))
         (script (nth (1+ (cl-position "--" stdio :test #'equal)) stdio)))
    (should (equal (car (last http)) (format "http://127.0.0.1:%d/mcp" ide-ai-emcp-port)))
    (should (file-executable-p script))
    (should (member "--socket=mcgeemacs-test" stdio))
    (should (member "--init-function=ide-ai-mcp-elisp-dev-enable" stdio))
    (should (fboundp 'ide-ai-mcp-elisp-dev-enable))
    (should (fboundp 'elisp-dev-mcp-disable))))

(ert-deftest ai-mcp-autostart-never-runs-in-batch ()
  (let ((ide-ai-mcp-autostart t) started)
    (cl-letf (((symbol-function 'ide-ai-mcp-start) (lambda () (setq started t))))
      (ide-ai--mcp-autostart))
    (should-not started)))

(provide 'ai-mcp-test)
;;; ai-mcp-test.el ends here
