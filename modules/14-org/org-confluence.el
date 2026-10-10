;;; org-confluence.el --- Confluence Cloud search, ingest, publish -*- lexical-binding: t; -*-

;;; Commentary:
;; REST v1 connector.  Configure `confluence-site' (the part before
;; .atlassian.net) and credentials via $CONFLUENCE_EMAIL / $CONFLUENCE_API_TOKEN
;; or an auth-source entry for host <site>.atlassian.net.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'json)
(require 'dom)
(require 'url-http)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(declare-function org-export-as "ox")
(declare-function org-mode "org")
(declare-function consult--read "consult")
(declare-function consult--dynamic-collection "consult")

(defgroup confluence nil "Confluence Cloud integration." :group 'tools)
(defcustom confluence-site nil "Atlassian site name, e.g. \"acme\" for acme.atlassian.net."
  :type '(choice (const nil) string))
(defcustom confluence-default-space nil "Default space key for new pages."
  :type '(choice (const nil) string))

(defvar-local confluence-page-id nil "Confluence page id of this Org buffer.")
(defvar-local confluence-page-space nil "Confluence space key of this Org buffer.")

(defun confluence--base ()
  (unless confluence-site (user-error "Set `confluence-site' (e.g. \"acme\")"))
  (format "https://%s.atlassian.net/wiki/rest/api" confluence-site))

(defun confluence--auth ()
  (let* ((host (format "%s.atlassian.net" confluence-site))
         (found (car (auth-source-search :host host :max 1)))
         (email (or (getenv "CONFLUENCE_EMAIL") (plist-get found :user)))
         (token (or (getenv "CONFLUENCE_API_TOKEN")
                    (let ((s (plist-get found :secret))) (if (functionp s) (funcall s) s)))))
    (unless (and email token)
      (user-error "Set CONFLUENCE_EMAIL and CONFLUENCE_API_TOKEN, or add an auth-source entry for %s" host))
    (concat "Basic " (base64-encode-string (concat email ":" token) t))))

(defun confluence--request (method path &optional body query)
  "Perform METHOD on PATH (relative to REST base) and return parsed JSON."
  (let* ((url (concat (confluence--base) path
                      (when query
                        (concat "?" (mapconcat (lambda (kv) (format "%s=%s" (car kv) (url-hexify-string (format "%s" (cdr kv)))))
                                               query "&")))))
         (url-request-method method)
         (url-request-extra-headers `(("Authorization" . ,(confluence--auth))
                                      ("Accept" . "application/json")
                                      ("Content-Type" . "application/json")))
         (url-request-data (and body (encode-coding-string (json-serialize body) 'utf-8)))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (error "confluence: no response"))
    (unwind-protect
        (with-current-buffer buf
          (let ((status url-http-response-status))
            (goto-char url-http-end-of-headers)
            (let ((data (condition-case nil
                            (json-parse-string (decode-coding-string
                                                (buffer-substring-no-properties (point) (point-max)) 'utf-8)
                                               :object-type 'alist :array-type 'list
                                               :null-object nil :false-object nil)
                          (error nil))))
              (when (>= status 400)
                (error "confluence: HTTP %s: %s" status (or (alist-get 'message data) "request failed")))
              data)))
      (kill-buffer buf))))

;;;; Search ------------------------------------------------------------------

(defun confluence--search (text)
  "Return page results for TEXT."
  (when (>= (length text) 2)
    (let* ((cql (format "type=page AND (title ~ \"%s*\" OR text ~ \"%s\")"
                        (replace-regexp-in-string "\"" "" text)
                        (replace-regexp-in-string "\"" "" text)))
           (res (confluence--request "GET" "/content/search" nil
                                     `(("cql" . ,cql) ("limit" . 25) ("expand" . "space")))))
      (alist-get 'results res))))

(defun confluence--candidate (page)
  (propertize (format "%s  [%s]" (alist-get 'title page)
                      (alist-get 'key (alist-get 'space page)))
              'confluence-id (alist-get 'id page)))

;;;###autoload
(defun confluence-search ()
  "Search Confluence pages with live consult results and open the selection."
  (interactive)
  (require 'consult)
  (let* ((choice
          (consult--read
           (consult--dynamic-collection
            (lambda (input) (mapcar #'confluence--candidate (confluence--search input))))
           :prompt "Confluence: " :category 'confluence-page :require-match t
           :sort nil))
         (id (get-text-property 0 'confluence-id choice)))
    (if id (confluence-open-page id)
      (user-error "No page selected"))))

;;;###autoload
(defalias 'consult-confluence #'confluence-search)

;;;; Storage format -> Org ---------------------------------------------------

(defun confluence--preprocess-storage (xhtml)
  "Turn Confluence-specific macros in XHTML into plain HTML."
  (with-temp-buffer
    (insert xhtml)
    (goto-char (point-min))
    ;; code macro -> pre/code
    (while (re-search-forward
            "<ac:structured-macro[^>]*ac:name=\"code\"[^>]*>\\(?:.\\|\n\\)*?\\(?:<ac:parameter ac:name=\"language\">\\([^<]*\\)</ac:parameter>\\)?\\(?:.\\|\n\\)*?<ac:plain-text-body><!\\[CDATA\\[\\(\\(?:.\\|\n\\)*?\\)\\]\\]></ac:plain-text-body>\\(?:.\\|\n\\)*?</ac:structured-macro>"
            nil t)
      (let ((lang (or (match-string 1) "")) (code (match-string 2)))
        (replace-match (format "<pre><code class=\"language-%s\">%s</code></pre>" lang
                               (replace-regexp-in-string "<" "&lt;" (replace-regexp-in-string "&" "&amp;" code)))
                       t t)))
    (goto-char (point-min))
    (while (re-search-forward "</?\\(?:ac\\|ri\\):[^>]*>" nil t) (replace-match "" t t))
    (buffer-string)))

(defun confluence--html-to-org (html)
  "Convert HTML to Org text, via pandoc when present."
  (if (executable-find "pandoc")
      (with-temp-buffer
        (insert html)
        (if (zerop (call-process-region (point-min) (point-max) "pandoc" t t nil
                                        "-f" "html" "-t" "org" "--wrap=none"))
            (buffer-string)
          (confluence--html-to-org-fallback html)))
    (confluence--html-to-org-fallback html)))

(defun confluence--html-to-org-fallback (html)
  "Minimal HTML to Org conversion without external tools."
  (let ((s html))
    (dolist (r '(("<h1[^>]*>\\(.*?\\)</h1>" . "\n* \\1\n")
                 ("<h2[^>]*>\\(.*?\\)</h2>" . "\n** \\1\n")
                 ("<h3[^>]*>\\(.*?\\)</h3>" . "\n*** \\1\n")
                 ("<h[4-6][^>]*>\\(.*?\\)</h[4-6]>" . "\n**** \\1\n")
                 ("<strong>\\(.*?\\)</strong>" . "*\\1*")
                 ("<em>\\(.*?\\)</em>" . "/\\1/")
                 ("<code[^>]*>\\(.*?\\)</code>" . "~\\1~")
                 ("<li[^>]*>" . "\n- ")
                 ("<br ?/?>" . "\n")
                 ("</p>" . "\n\n")))
      (setq s (replace-regexp-in-string (car r) (cdr r) s)))
    (setq s (replace-regexp-in-string
             "<pre><code class=\"language-\\([^\"]*\\)\">\\(\\(?:.\\|\n\\)*?\\)</code></pre>"
             "\n#+begin_src \\1\n\\2\n#+end_src\n" s))
    (setq s (replace-regexp-in-string "<[^>]+>" "" s))
    (dolist (e '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&nbsp;" . " ") ("&amp;" . "&")))
      (setq s (replace-regexp-in-string (car e) (cdr e) s)))
    s))

;;;###autoload
(defun confluence-open-page (id)
  "Fetch page ID and open it as an Org buffer."
  (interactive "sPage id: ")
  (let* ((page (confluence--request "GET" (format "/content/%s" id) nil
                                    '(("expand" . "body.storage,version,space"))))
         (title (alist-get 'title page))
         (space (alist-get 'key (alist-get 'space page)))
         (xhtml (alist-get 'value (alist-get 'storage (alist-get 'body page))))
         (org (confluence--html-to-org (confluence--preprocess-storage xhtml)))
         (buf (get-buffer-create (format "*confluence:%s*" title))))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "#+title: %s\n#+confluence_id: %s\n#+confluence_space: %s\n\n" title id space) org)
      (org-mode)
      (setq confluence-page-id id confluence-page-space space)
      (goto-char (point-min)))
    (pop-to-buffer-same-window buf)))

;;;; Org -> storage format ---------------------------------------------------

(defun confluence--html-to-storage (html)
  "Make exported HTML valid Confluence storage XHTML."
  (let ((s html))
    ;; src blocks -> code macro
    (setq s (replace-regexp-in-string
             "<pre class=\"src src-\\([^\"]*\\)\">\\(\\(?:.\\|\n\\)*?\\)</pre>"
             (lambda (m)
               (let* ((lang (progn (string-match "src-\\([^\"]*\\)" m) (match-string 1 m)))
                      (code (progn (string-match ">\\(\\(?:.\\|\n\\)*\\)</pre>" m) (match-string 1 m)))
                      (code (replace-regexp-in-string "<[^>]+>" "" code))
                      (code (replace-regexp-in-string "&lt;" "<" code))
                      (code (replace-regexp-in-string "&gt;" ">" code))
                      (code (replace-regexp-in-string "&amp;" "&" code)))
                 (format "<ac:structured-macro ac:name=\"code\"><ac:parameter ac:name=\"language\">%s</ac:parameter><ac:plain-text-body><![CDATA[%s]]></ac:plain-text-body></ac:structured-macro>"
                         lang code)))
             s t t))
    (setq s (replace-regexp-in-string "<\\(br\\|hr\\)\\([^>/]*\\)>" "<\\1\\2 />" s))
    s))

(defun confluence--buffer-keyword (name)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*\\)$" name) nil t)
      (string-trim (match-string 1)))))

;;;###autoload
(defun confluence-publish-buffer ()
  "Publish this Org buffer to Confluence, updating or creating the page."
  (interactive)
  (require 'ox-html)
  (let* ((id (or confluence-page-id (confluence--buffer-keyword "confluence_id")))
         (space (or confluence-page-space (confluence--buffer-keyword "confluence_space")
                    confluence-default-space
                    (read-string "Space key: ")))
         (title (or (confluence--buffer-keyword "title") (read-string "Page title: ")))
         (html (confluence--html-to-storage
                (org-export-as 'html nil nil t '(:with-toc nil :section-numbers nil))))
         (body `((storage . ((value . ,html) (representation . "storage"))))))
    (if id
        (let* ((cur (confluence--request "GET" (format "/content/%s" id) nil '(("expand" . "version"))))
               (ver (1+ (alist-get 'number (alist-get 'version cur)))))
          (confluence--request "PUT" (format "/content/%s" id)
                               `((id . ,id) (type . "page") (title . ,title)
                                 (version . ((number . ,ver))) (body . ,body)))
          (message "Updated Confluence page %s (v%s)" id ver))
      (let ((res (confluence--request "POST" "/content"
                                      `((type . "page") (title . ,title)
                                        (space . ((key . ,space))) (body . ,body)))))
        (setq confluence-page-id (alist-get 'id res) confluence-page-space space)
        (save-excursion
          (goto-char (point-min))
          (insert (format "#+confluence_id: %s\n#+confluence_space: %s\n" confluence-page-id space)))
        (message "Created Confluence page %s" confluence-page-id)))))

(global-set-key (kbd "C-c o c") #'confluence-search)

(provide 'org-confluence)
;;; org-confluence.el ends here
