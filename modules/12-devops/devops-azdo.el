;;; devops-azdo.el --- Azure DevOps forge for Magit -*- lexical-binding: t; -*-

;;; Commentary:
;; Pull requests, review threads, voting, Azure Boards and Pipelines status,
;; all through the Azure DevOps REST API (api-version 7.1) so no browser is
;; needed.
;;
;; Authentication, in order: $AZURE_DEVOPS_EXT_PAT, auth-source entry for host
;; dev.azure.com, then an Entra token from `az account get-access-token'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url)
(require 'json)
(require 'transient)
(require 'tabulated-list)
(require 'shr)
(defvar shr-use-fonts)
(defvar shr-width)
(defvar shr-inhibit-images)

(declare-function magit-insert-section "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function magit-current-section "magit-section")
(declare-function magit-diff-range "magit-diff")
(declare-function magit-run-git "magit-git")
(declare-function magit-call-git "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-toplevel "magit-git")
(declare-function magit-refresh "magit-mode")
(declare-function magit-add-section-hook "magit-section")
(declare-function magit-file-at-point "magit-diff")
(declare-function org-mode "org")
(defvar magit-status-mode-map)
(defvar magit-diff-mode-map)

(declare-function ide-run-with-timeout "00-core")

(defgroup azdo nil "Azure DevOps integration." :group 'tools)

(defcustom azdo-organization nil "Organization; inferred from the git remote when nil."
  :type '(choice (const nil) string))
(defcustom azdo-project nil "Project; inferred from the git remote when nil."
  :type '(choice (const nil) string))
(defcustom azdo-repository nil "Repository; inferred from the git remote when nil."
  :type '(choice (const nil) string))
(defcustom azdo-default-target-branch nil
  "Default PR target branch.  When nil, use the remote HEAD, else main/master."
  :type '(choice (const nil) string))
(defcustom azdo-cache-ttl 60 "Seconds before Magit-section data is refetched." :type 'integer)
(defcustom azdo-api-version "7.1" "REST api-version." :type 'string)
(defcustom azdo-request-timeout 30 "Seconds before an async request is abandoned." :type 'integer)

(defconst azdo--entra-resource "499b84ac-1321-427f-aa17-267ca6975798"
  "Application ID of Azure DevOps, used for Entra tokens.")

;;;; Context -----------------------------------------------------------------

(defun azdo--git-lines (&rest args)
  "Return output lines of git ARGS in `default-directory', or nil on failure."
  (ignore-errors (apply #'process-lines "git" args)))

(defun azdo--parse-remote (url)
  "Parse Azure DevOps remote URL into a plist (:org :project :repo), or nil."
  (let ((m (cond
            ((string-match "dev\\.azure\\.com/\\([^/]+\\)/\\([^/]+\\)/_git/\\([^/?]+\\)" url)
             (list (match-string 1 url) (match-string 2 url) (match-string 3 url)))
            ((string-match "ssh\\.dev\\.azure\\.com[:/]v3/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+?\\)\\(?:\\.git\\)?\\'" url)
             (list (match-string 1 url) (match-string 2 url) (match-string 3 url)))
            ((string-match "//\\(?:[^@/]+@\\)?\\([^./]+\\)\\.visualstudio\\.com\\(?:/DefaultCollection\\)?/\\([^/]+\\)/_git/\\([^/?]+\\)" url)
             (list (match-string 1 url) (match-string 2 url) (match-string 3 url))))))
    ;; Decode only after all groups are captured: `url-unhex-string' clobbers match data.
    (when m
      (cl-destructuring-bind (org project repo) (mapcar #'url-unhex-string m)
        (list :org org :project project :repo repo)))))

(defun azdo--remote-info ()
  "Return (REMOTE . PLIST) for the first Azure DevOps remote of this repo."
  (cl-loop for r in (azdo--git-lines "remote")
           for url = (car (azdo--git-lines "config" "--get" (format "remote.%s.url" r)))
           for info = (and url (azdo--parse-remote url))
           when info return (cons r info)))

(defvar azdo--ctx-cache (make-hash-table :test #'equal)
  "Maps directory to (TIME . CONTEXT); avoids spawning git on every call.")
(defconst azdo--ctx-ttl 30 "Seconds a cached repository context stays valid.")

(defun azdo--compute-ctx ()
  (let* ((ri (azdo--remote-info))
         (info (cdr ri)))
    (when (or info (and azdo-organization azdo-project azdo-repository))
      (list :org (or azdo-organization (plist-get info :org))
            :project (or azdo-project (plist-get info :project))
            :repo (or azdo-repository (plist-get info :repo))
            :remote (or (car ri) "origin")))))

(defun azdo--ctx ()
  "Return plist (:org :project :repo :remote) for the current repo, or nil.
Results are cached per directory for `azdo--ctx-ttl' seconds."
  (let* ((key (expand-file-name default-directory))
         (hit (gethash key azdo--ctx-cache)))
    (if (and hit (< (float-time (time-since (car hit))) azdo--ctx-ttl))
        (cdr hit)
      (when (> (hash-table-count azdo--ctx-cache) 50) (clrhash azdo--ctx-cache))
      (let ((ctx (azdo--compute-ctx)))
        (puthash key (cons (current-time) ctx) azdo--ctx-cache)
        ctx))))

(defun azdo--ctx! ()
  "Like `azdo--ctx' but signal a user error when not in an Azure DevOps repo."
  (or (azdo--ctx) (user-error "Not inside an Azure DevOps repository")))

(defun azdo--base (&optional ctx)
  "Project-level API base URL for CTX."
  (let ((c (or ctx (azdo--ctx!))))
    (format "https://dev.azure.com/%s/%s/_apis"
            (url-hexify-string (plist-get c :org))
            (url-hexify-string (plist-get c :project)))))

(defun azdo--git-base (&optional ctx)
  "Repository-level git API base URL for CTX."
  (let ((c (or ctx (azdo--ctx!))))
    (format "%s/git/repositories/%s" (azdo--base c)
            (url-hexify-string (plist-get c :repo)))))

;;;; Authentication ----------------------------------------------------------

(defvar azdo--token-cache nil "Cons of (EXPIRY-TIME . TOKEN).")

(defun azdo--auth-header ()
  "Return the Authorization header value."
  (let ((pat (or (getenv "AZURE_DEVOPS_EXT_PAT")
                 (when-let* ((found (car (auth-source-search :host "dev.azure.com" :max 1))))
                   (let ((s (plist-get found :secret)))
                     (if (functionp s) (funcall s) s))))))
    (cond
     (pat (concat "Basic " (base64-encode-string (concat ":" pat) t)))
     ((and azdo--token-cache (time-less-p (current-time) (car azdo--token-cache)))
      (concat "Bearer " (cdr azdo--token-cache)))
     ((executable-find "az")
      (let* ((res (ide-run-with-timeout 30 "az" "account" "get-access-token"
                                        "--resource" azdo--entra-resource
                                        "--query" "accessToken" "-o" "tsv"))
             (tok (string-trim (nth 1 res))))
        (when (eq (car res) 'timeout)
          (user-error "azdo: `az account get-access-token' timed out"))
        (when (string-empty-p tok)
          (user-error "azdo: `az' returned no token (%s); run `az login' or set AZURE_DEVOPS_EXT_PAT"
                      (string-trim (nth 2 res))))
        (setq azdo--token-cache (cons (time-add (current-time) (* 50 60)) tok))
        (concat "Bearer " tok)))
     (t (user-error "azdo: set AZURE_DEVOPS_EXT_PAT, add an auth-source entry for dev.azure.com, or install `az'")))))

;;;; HTTP --------------------------------------------------------------------

(defun azdo--url (base path &optional query)
  "Build URL from BASE, PATH and QUERY alist (api-version is appended)."
  (let ((q (append query `(("api-version" . ,azdo-api-version)))))
    (concat base path "?"
            (mapconcat (lambda (kv) (format "%s=%s" (car kv) (url-hexify-string (format "%s" (cdr kv)))))
                       (cl-remove-if (lambda (kv) (null (cdr kv))) q) "&"))))

(defun azdo--encode (body)
  "Encode BODY (string, alist, vector) to a UTF-8 JSON unibyte string."
  (encode-coding-string (if (stringp body) body (json-serialize body)) 'utf-8))

(defun azdo--parse-response ()
  "Parse the current url response buffer into (STATUS . DATA)."
  (goto-char (point-min))
  (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status) 0)))
    (goto-char (if (and (boundp 'url-http-end-of-headers) url-http-end-of-headers)
                   url-http-end-of-headers (point-min)))
    (skip-chars-forward " \r\n")
    (let ((raw (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8)))
      (cons status
            (unless (string-empty-p (string-trim raw))
              (condition-case nil
                  (json-parse-string raw :object-type 'alist :array-type 'list
                                     :null-object nil :false-object nil)
                (error `((raw . ,raw)))))))))

(defun azdo--request (method url &optional body content-type)
  "Synchronously perform METHOD on URL with BODY; return parsed JSON."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(azdo--auth-header))
            ("Accept" . "application/json")
            ("Content-Type" . ,(or content-type "application/json"))))
         (url-request-data (and body (azdo--encode body)))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (error "azdo: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let* ((res (azdo--parse-response)) (status (car res)) (data (cdr res)))
            (when (or (>= status 400) (zerop status))
              (error "azdo: HTTP %s: %s" status
                     (or (alist-get 'message data) (alist-get 'raw data) url)))
            data))
      (kill-buffer buf))))

(defconst azdo--failed 'azdo--failed
  "Value passed to async callbacks when the request failed.
Distinct from nil, which is a legitimate empty result.")

(defun azdo--request-async (method url callback &optional body)
  "Perform METHOD on URL asynchronously and call CALLBACK exactly once.
CALLBACK receives the parsed data, or `azdo--failed' on error, bad status
or after `azdo-request-timeout' seconds.  Failures are logged."
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("Authorization" . ,(azdo--auth-header))
            ("Accept" . "application/json")
            ("Content-Type" . "application/json")))
         (url-request-data (and body (azdo--encode body)))
         (done nil)
         (buf nil))
    (setq buf
          (url-retrieve
           url
           (lambda (status)
             (let ((b (current-buffer)) res err)
               (unless done
                 (setq done t)
                 (condition-case e (setq res (azdo--parse-response))
                   (error (setq err (error-message-string e))))
                 (kill-buffer b)
                 (cond ((plist-get status :error)
                        (message "azdo: request to %s failed: %S" url (plist-get status :error))
                        (funcall callback azdo--failed))
                       (err (message "azdo: unreadable response from %s: %s" url err)
                            (funcall callback azdo--failed))
                       ((not (<= 200 (car res) 299))
                        (message "azdo: HTTP %s from %s" (car res) url)
                        (funcall callback azdo--failed))
                       (t (funcall callback (cdr res)))))))
           nil t t))
    (run-at-time azdo-request-timeout nil
                 (lambda ()
                   (unless done
                     (setq done t)
                     (when (buffer-live-p buf)
                       (when-let* ((p (get-buffer-process buf))) (delete-process p))
                       (kill-buffer buf))
                     (message "azdo: request timed out: %s" url)
                     (funcall callback azdo--failed))))))

(defun azdo--get (base path &optional query) (azdo--request "GET" (azdo--url base path query)))
(defun azdo--post (base path body &optional query) (azdo--request "POST" (azdo--url base path query) body))
(defun azdo--patch (base path body &optional query) (azdo--request "PATCH" (azdo--url base path query) body))
(defun azdo--put (base path body &optional query) (azdo--request "PUT" (azdo--url base path query) body))

(defun azdo--value (data) "Return the `value' list of a collection response DATA."
       (alist-get 'value data))

(defvar azdo--user-cache nil)
(defun azdo--me ()
  "Return the authenticated user's identity alist (id, displayName, ...)."
  (or azdo--user-cache
      (let* ((c (azdo--ctx!))
             (d (azdo--request
                 "GET" (format "https://dev.azure.com/%s/_apis/connectionData?api-version=7.1-preview.1"
                               (plist-get c :org)))))
        (setq azdo--user-cache (alist-get 'authenticatedUser d)))))

(defun azdo--me-id () (alist-get 'id (azdo--me)))

;;;; Compose buffers (multi-line input) --------------------------------------

(defvar-local azdo--compose-callback nil)
(defvar azdo-compose-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'azdo-compose-finish)
    (define-key m (kbd "C-c C-k") #'azdo-compose-abort)
    m))
(define-derived-mode azdo-compose-mode text-mode "AzDO-Compose"
  "Mode for composing PR descriptions and comments.
\\[azdo-compose-finish] submits, \\[azdo-compose-abort] cancels.")

(defun azdo--compose (name initial callback)
  "Pop up compose buffer NAME seeded with INITIAL; CALLBACK gets the text."
  (let ((buf (get-buffer-create (format "*azdo-compose:%s*" name))))
    (with-current-buffer buf
      (erase-buffer) (azdo-compose-mode)
      (insert (or initial ""))
      (setq azdo--compose-callback callback)
      (setq header-line-format "Write text.  C-c C-c to submit, C-c C-k to cancel."))
    (pop-to-buffer buf)))

(defun azdo-compose-finish ()
  "Submit the compose buffer."
  (interactive)
  (let ((text (string-trim (buffer-string))) (cb azdo--compose-callback))
    (when (string-empty-p text) (user-error "Empty text"))
    (quit-window t)
    (funcall cb text)))

(defun azdo-compose-abort ()
  "Cancel composing."
  (interactive) (quit-window t) (message "Cancelled"))

;;;; Branch helpers ----------------------------------------------------------

(defun azdo--current-branch ()
  (car (azdo--git-lines "rev-parse" "--abbrev-ref" "HEAD")))

(defun azdo--default-target ()
  (or azdo-default-target-branch
      (when-let* ((ref (car (azdo--git-lines "symbolic-ref" "--short"
                                             (format "refs/remotes/%s/HEAD"
                                                     (plist-get (azdo--ctx!) :remote))))))
        (string-remove-prefix (concat (plist-get (azdo--ctx!) :remote) "/") ref))
      (cl-find-if (lambda (b) (azdo--git-lines "rev-parse" "--verify" "--quiet" (concat "refs/heads/" b)))
                  '("main" "master"))
      "main"))

(defun azdo--branch (ref) (string-remove-prefix "refs/heads/" ref))

;;;; Pull requests -----------------------------------------------------------

(defun azdo--pr-status (pr)
  "Short review-status label for PR alist."
  (let* ((votes (mapcar (lambda (r) (alist-get 'vote r)) (alist-get 'reviewers pr)))
         (min (if votes (apply #'min votes) 0))
         (max (if votes (apply #'max votes) 0)))
    (cond ((eq (alist-get 'isDraft pr) t) "Draft")
          ((< min 0) (if (<= min -10) "Rejected" "Waiting"))
          ((>= max 10) "Approved")
          ((>= max 5) "Approved w/ suggestions")
          (t "No votes"))))

(defun azdo--pr-line (pr)
  (format "#%-6s %-24s %-9s %s"
          (alist-get 'pullRequestId pr)
          (truncate-string-to-width
           (or (alist-get 'displayName (alist-get 'createdBy pr)) "?") 24)
          (azdo--pr-status pr)
          (alist-get 'title pr)))

(defun azdo--list-prs (&optional filter)
  "Fetch active PRs.  FILTER is nil/`all', `mine' or `review'."
  (let* ((me (and (memq filter '(mine review)) (azdo--me-id)))
         (data (azdo--get (azdo--git-base) "/pullrequests"
                          `(("searchCriteria.status" . "active")
                            ("searchCriteria.creatorId" . ,(and (eq filter 'mine) me))
                            ("searchCriteria.reviewerId" . ,(and (eq filter 'review) me))
                            ("$top" . 100)))))
    (azdo--value data)))

(defun azdo--get-pr (id)
  (azdo--get (azdo--git-base) (format "/pullrequests/%s" id)))

(defvar-local azdo-current-pr nil "PR id associated with this buffer.")

(defun azdo--pr-id-at-point ()
  "Return the PR id at point, in a review buffer, or for the current branch."
  (or (when (and (fboundp 'magit-current-section) (magit-current-section))
        (let ((s (magit-current-section)))
          (when (eq (oref s type) 'azdo-pr)
            (alist-get 'pullRequestId (oref s value)))))
      azdo-current-pr
      (when-let* ((br (azdo--current-branch))
                  (res (azdo--value
                        (azdo--get (azdo--git-base) "/pullrequests"
                                   `(("searchCriteria.status" . "active")
                                     ("searchCriteria.sourceRefName" . ,(concat "refs/heads/" br)))))))
        (alist-get 'pullRequestId (car res)))))

(defun azdo--read-pr (&optional filter prompt)
  "Select a PR interactively and return its alist."
  (let* ((prs (azdo--list-prs filter))
         (cands (mapcar (lambda (pr) (cons (azdo--pr-line pr) pr)) prs)))
    (unless cands (user-error "No pull requests"))
    (let ((completion-extra-properties
           `(:annotation-function
             ,(lambda (c) (when-let* ((pr (cdr (assoc c cands))))
                            (format "  %s → %s"
                                    (azdo--branch (alist-get 'sourceRefName pr))
                                    (azdo--branch (alist-get 'targetRefName pr))))))))
      (cdr (assoc (completing-read (or prompt "Pull request: ") cands nil t) cands)))))

(defun azdo--pr-id (&optional prompt)
  (or (azdo--pr-id-at-point)
      (alist-get 'pullRequestId (azdo--read-pr nil prompt))))

;;;###autoload
(defun azdo-pr-list (&optional filter)
  "List PRs (FILTER: all, mine, review) and act on the selected one."
  (interactive
   (list (pcase (completing-read "Show: " '("All Active" "Created by Me" "Needs My Review") nil t)
           ("Created by Me" 'mine) ("Needs My Review" 'review) (_ 'all))))
  (let* ((pr (azdo--read-pr filter))
         (id (alist-get 'pullRequestId pr))
         (act (completing-read (format "PR #%s: " id)
                               '("review" "checkout" "threads" "vote" "complete" "abandon") nil t)))
    (pcase act
      ("review" (azdo-pr-review id))
      ("checkout" (azdo-pr-checkout id))
      ("threads" (azdo-pr-threads id))
      ("vote" (call-interactively #'azdo-pr-vote))
      ("complete" (azdo-pr-complete id))
      ("abandon" (azdo-pr-abandon id)))))

(defun azdo--pr-template ()
  "Return contents of a PR template in the repo, or nil."
  (when-let* ((root (car (azdo--git-lines "rev-parse" "--show-toplevel")))
              (f (cl-find-if #'file-exists-p
                             (mapcar (lambda (p) (expand-file-name p root))
                                     '(".azuredevops/pull_request_template.md"
                                       ".azure/pull_request_template.md"
                                       "docs/pull_request_template.md"
                                       "pull_request_template.md")))))
    (with-temp-buffer (insert-file-contents f) (buffer-string))))

(defun azdo--resolve-reviewer (query)
  "Resolve QUERY (name/email) to an identity id via the Identities API."
  (let* ((c (azdo--ctx!))
         (d (azdo--request
             "GET" (format "https://vssps.dev.azure.com/%s/_apis/identities?searchFilter=General&filterValue=%s&api-version=7.1-preview.1"
                           (plist-get c :org) (url-hexify-string query))))
         (hit (car (azdo--value d))))
    (unless hit (user-error "No identity found for %s" query))
    (alist-get 'id hit)))

;;;###autoload
(defun azdo--pr-body (src tgt title desc draft reviewer-ids wi-ids)
  "Build the JSON body for creating a PR.  Pure; reviewer/work item ids are resolved."
  `((sourceRefName . ,(concat "refs/heads/" src))
    (targetRefName . ,(concat "refs/heads/" tgt))
    (title . ,title) (description . ,desc)
    (isDraft . ,(if draft t :false))
    ,@(when reviewer-ids
        `((reviewers . ,(vconcat (mapcar (lambda (id) `((id . ,id) (isRequired . t))) reviewer-ids)))))
    ,@(when wi-ids
        `((workItemRefs . ,(vconcat (mapcar (lambda (id) `((id . ,(format "%s" id)))) wi-ids)))))))

(defun azdo--read-work-item-ids (items)
  "Let the user pick any number of work ITEMS one at a time; return their ids.
Selection is by repeated prompt, not `completing-read-multiple', because
titles may contain commas.  An empty answer finishes."
  (let ((cands (mapcar (lambda (w) (cons (azdo--wi-line w) (alist-get 'id w))) items))
        chosen)
    (when cands
      (while (let* ((left (cl-remove-if (lambda (c) (memql (cdr c) chosen)) cands))
                    (pick (and left
                               (completing-read
                                (format "Link work item (%d linked; empty to finish): " (length chosen))
                                (mapcar #'car left) nil t))))
               (when (and pick (not (string-empty-p pick)))
                 (push (cdr (assoc pick cands)) chosen)
                 t))))
    (nreverse chosen)))

;;;###autoload
(defun azdo-pr-create ()
  "Create a pull request from the current branch."
  (interactive)
  (let* ((src (or (azdo--current-branch) (user-error "No current branch")))
         (tgt (read-string "Target branch: " (azdo--default-target)))
         (title (read-string "Title: " (car (azdo--git-lines "log" "-1" "--format=%s"))))
         (draft (y-or-n-p "Create as draft? "))
         (reviewers (split-string (read-string "Required reviewers (emails/names, comma-separated): ")
                                  "," t "[ \t]+"))
         (items (condition-case err (azdo--assigned-work-items)
                  (error (message "azdo: could not load work items: %s" (error-message-string err))
                         nil)))
         (wi-ids (azdo--read-work-item-ids items)))
    (unless (azdo--git-lines "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")
      (when (y-or-n-p (format "Push %s to %s first? " src (plist-get (azdo--ctx!) :remote)))
        (magit-call-git "push" "--set-upstream" (plist-get (azdo--ctx!) :remote) src)))
    (azdo--compose
     "pr-description" (azdo--pr-template)
     (lambda (desc)
       (let* ((body (azdo--pr-body src tgt title desc draft
                                   (mapcar #'azdo--resolve-reviewer reviewers) wi-ids))
              (pr (azdo--post (azdo--git-base) "/pullrequests" body)))
         (message "Created PR #%s: %s" (alist-get 'pullRequestId pr) (alist-get 'title pr))
         (azdo--invalidate-cache)
         (azdo-pr-review (alist-get 'pullRequestId pr)))))))

;;;###autoload
(defun azdo-pr-checkout (&optional id)
  "Check out PR ID as a local tracking branch, or inspect its merge ref."
  (interactive)
  (let* ((id (or id (azdo--pr-id "Checkout PR: ")))
         (pr (azdo--get-pr id))
         (remote (plist-get (azdo--ctx!) :remote))
         (src (azdo--branch (alist-get 'sourceRefName pr)))
         (how (completing-read "Checkout: " '("source branch (tracking)" "merge ref (detached)") nil t)))
    (if (string-prefix-p "merge" how)
        (progn (azdo--fetch-merge-ref id)
               (magit-run-git "checkout" "--detach" (format "%s/pr/%s" remote id)))
      (magit-call-git "fetch" remote (format "+refs/heads/%s:refs/remotes/%s/%s" src remote src))
      (magit-run-git "checkout" "-B" src "--track" (format "%s/%s" remote src)))))

(defun azdo--fetch-merge-ref (id)
  "Ensure refs/remotes/REMOTE/pr/ID exists locally."
  (let ((remote (plist-get (azdo--ctx!) :remote)))
    (magit-call-git "fetch" remote
                    (format "+refs/pull/%s/merge:refs/remotes/%s/pr/%s" id remote id))))

(defvar azdo--review-pr nil "PR id being opened in a Magit diff.")

(define-minor-mode azdo-review-mode
  "Minor mode for Magit diff buffers showing an Azure DevOps PR.
\\{azdo-review-mode-map}"
  :lighter " AzDO"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "c") #'azdo-pr-comment-at-point)
            (define-key m (kbd "T") #'azdo-pr-threads)
            (define-key m (kbd "V") #'azdo-pr-vote)
            m))

(defun azdo--enable-review-mode ()
  (when azdo--review-pr
    (setq azdo-current-pr azdo--review-pr)
    (azdo-review-mode 1)))
(add-hook 'magit-diff-mode-hook #'azdo--enable-review-mode)

;;;###autoload
(defun azdo-pr-review (&optional id)
  "Open the diff of PR ID in Magit with review keys (`c' comments)."
  (interactive)
  (let* ((id (or id (azdo--pr-id "Review PR: ")))
         (remote (plist-get (azdo--ctx!) :remote)))
    (azdo--fetch-merge-ref id)
    (let ((azdo--review-pr id)
          (ref (format "%s/pr/%s" remote id)))
      (magit-diff-range (format "%s^1..%s" ref ref))
      (with-current-buffer (current-buffer) (azdo--enable-review-mode)))))

(defun azdo--hunk-position ()
  "Return (FILE LINE SIDE) for the diff line at point; SIDE is `right' or `left'."
  (let* ((section (magit-current-section))
         (hunk (cl-loop for s = section then (oref s parent) while s
                        when (eq (oref s type) 'hunk) return s))
         (file (or (magit-file-at-point) (user-error "Point is not in a file diff"))))
    (unless hunk (user-error "Point is not on a diff hunk line"))
    (let* ((to (oref hunk to-range)) (from (oref hunk from-range))
           (start (oref hunk content))
           (cur (line-beginning-position))
           (right (car to)) (left (car from)) (res nil))
      (save-excursion
        (goto-char start)
        (while (and (<= (point) cur) (not (eobp)) (not res))
          (let ((ch (char-after)))
            (cond ((eq ch ?-) (when (= (point) cur) (setq res (list file left 'left))) (cl-incf left))
                  ((eq ch ?+) (when (= (point) cur) (setq res (list file right 'right))) (cl-incf right))
                  (t (when (= (point) cur) (setq res (list file right 'right)))
                     (cl-incf right) (cl-incf left))))
          (forward-line 1)))
      (or res (user-error "Point is not on a diff line")))))

;;;###autoload
(defun azdo--comment-body (pos text)
  "Build the thread JSON for an inline comment TEXT at POS = (FILE LINE SIDE)."
  (let* ((file (nth 0 pos)) (line (nth 1 pos)) (side (nth 2 pos))
         (loc `((line . ,line) (offset . 1)))
         (right (eq side 'right)))
    `((comments . [((parentCommentId . 0) (content . ,text) (commentType . 1))])
      (status . 1)
      (threadContext . ((filePath . ,(concat "/" file))
                        ,(if right `(rightFileStart . ,loc) `(leftFileStart . ,loc))
                        ,(if right `(rightFileEnd . ,loc) `(leftFileEnd . ,loc)))))))

(defun azdo-pr-comment-at-point ()
  "Post an inline review comment on the diff line at point."
  (interactive)
  (let* ((id (or azdo-current-pr (azdo--pr-id)))
         (pos (azdo--hunk-position)))
    (azdo--compose
     (format "comment-%s" id) nil
     (lambda (text)
       (azdo--post (azdo--git-base) (format "/pullrequests/%s/threads" id)
                   (azdo--comment-body pos text))
       (message "Comment posted on %s:%s" (nth 0 pos) (nth 1 pos))))))

;;;; Threads buffer ----------------------------------------------------------

(defvar azdo-threads-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m tabulated-list-mode-map)
    (define-key m (kbd "r") #'azdo-thread-reply)
    (define-key m (kbd "x") #'azdo-thread-resolve)
    (define-key m (kbd "a") #'azdo-thread-activate)
    (define-key m (kbd "RET") #'azdo-thread-show)
    (define-key m (kbd "g") #'azdo-threads-refresh)
    m))

(define-derived-mode azdo-threads-mode tabulated-list-mode "AzDO-Threads"
  "Discussion threads of an Azure DevOps pull request."
  (setq tabulated-list-format [("ID" 6 t) ("Status" 10 t) ("Location" 38 t) ("Author" 20 t) ("Comment" 0 nil)]
        tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun azdo--thread-entries (threads)
  (cl-loop for th in threads
           for comments = (cl-remove-if (lambda (c) (equal (alist-get 'commentType c) "system"))
                                        (alist-get 'comments th))
           when comments
           collect
           (let* ((c (car comments))
                  (ctx (alist-get 'threadContext th))
                  (loc (if ctx
                           (format "%s:%s" (alist-get 'filePath ctx)
                                   (or (alist-get 'line (or (alist-get 'rightFileStart ctx)
                                                            (alist-get 'leftFileStart ctx))) ""))
                         "(general)")))
             (list (alist-get 'id th)
                   (vector (number-to-string (alist-get 'id th))
                           (format "%s" (or (alist-get 'status th) "unknown"))
                           loc
                           (or (alist-get 'displayName (alist-get 'author c)) "?")
                           (replace-regexp-in-string "\n" " " (or (alist-get 'content c) "")))))))

;;;###autoload
(defun azdo-pr-threads (&optional id)
  "Show discussion threads of PR ID."
  (interactive)
  (let* ((id (or id azdo-current-pr (azdo--pr-id "Threads of PR: ")))
         (ctx (azdo--ctx!))
         (buf (get-buffer-create (format "*azdo-threads:%s*" id))))
    (with-current-buffer buf
      (azdo-threads-mode)
      (setq azdo-current-pr id)
      (setq-local azdo--threads-ctx ctx)
      (azdo-threads-refresh))
    (pop-to-buffer buf)))

(defvar-local azdo--threads-ctx nil)
(defvar-local azdo--threads-data nil)

(defun azdo-threads-refresh ()
  "Reload threads."
  (interactive)
  (let ((default-directory default-directory))
    (setq azdo--threads-data
          (azdo--value (azdo--get (azdo--git-base azdo--threads-ctx)
                                  (format "/pullrequests/%s/threads" azdo-current-pr))))
    (setq tabulated-list-entries (azdo--thread-entries azdo--threads-data))
    (tabulated-list-print t)))

(defun azdo--thread-at-point ()
  (let ((id (tabulated-list-get-id)))
    (or (cl-find id azdo--threads-data :key (lambda (th) (alist-get 'id th)))
        (user-error "No thread at point"))))

(defun azdo-thread-show ()
  "Show the full conversation of the thread at point."
  (interactive)
  (let ((th (azdo--thread-at-point)))
    (with-current-buffer (get-buffer-create "*azdo-thread*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (c (alist-get 'comments th))
          (unless (equal (alist-get 'commentType c) "system")
            (insert (format "%s:\n%s\n\n" (alist-get 'displayName (alist-get 'author c))
                            (alist-get 'content c)))))
        (special-mode) (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun azdo-thread-reply ()
  "Reply to the thread at point."
  (interactive)
  (let* ((th (azdo--thread-at-point)) (tid (alist-get 'id th))
         (pr azdo-current-pr) (ctx azdo--threads-ctx) (buf (current-buffer)))
    (azdo--compose (format "reply-%s" tid) nil
                   (lambda (text)
                     (azdo--post (azdo--git-base ctx)
                                 (format "/pullrequests/%s/threads/%s/comments" pr tid)
                                 `((content . ,text) (parentCommentId . 1) (commentType . 1)))
                     (with-current-buffer buf (azdo-threads-refresh))))))

(defun azdo--set-thread-status (status)
  (let ((th (azdo--thread-at-point)))
    (azdo--patch (azdo--git-base azdo--threads-ctx)
                 (format "/pullrequests/%s/threads/%s" azdo-current-pr (alist-get 'id th))
                 `((status . ,status)))
    (azdo-threads-refresh)))

(defun azdo-thread-resolve () "Mark thread as resolved." (interactive) (azdo--set-thread-status "fixed"))
(defun azdo-thread-activate () "Mark thread as active." (interactive) (azdo--set-thread-status "active"))

;;;; Voting / completion -----------------------------------------------------

(defconst azdo--votes '((?a "Approve" 10) (?s "Approve with suggestions" 5)
                        (?w "Waiting for author" -5) (?r "Reject" -10) (?0 "Reset vote" 0)))

(defun azdo--cast-vote (id vote)
  (azdo--put (azdo--git-base) (format "/pullrequests/%s/reviewers/%s" id (azdo--me-id))
             `((vote . ,vote)))
  (azdo--invalidate-cache)
  (message "PR #%s: vote %s" id vote))

;;;###autoload
(defun azdo-pr-vote (key)
  "Vote on the current PR: a approve, s suggestions, w waiting, r reject, 0 reset."
  (interactive
   (list (read-char-choice "Vote [a]pprove [s]uggestions [w]aiting [r]eject [0]reset: "
                           (mapcar #'car azdo--votes))))
  (azdo--cast-vote (azdo--pr-id "Vote on PR: ") (nth 2 (assq key azdo--votes))))

;;;###autoload
(defun azdo--completion-body (pr strategy delete bypass reason)
  "Build the PATCH body that completes PR using STRATEGY (squash/rebase/merge)."
  `((status . "completed")
    (lastMergeSourceCommit
     . ((commitId . ,(alist-get 'commitId (alist-get 'lastMergeSourceCommit pr)))))
    (completionOptions
     . ((mergeStrategy . ,(pcase strategy ("merge" "noFastForward") (s s)))
        (deleteSourceBranch . ,(if delete t :false))
        (bypassPolicy . ,(if bypass t :false))
        ,@(when (and bypass reason) `((bypassReason . ,reason)))))))

(defun azdo-pr-complete (&optional id)
  "Complete (merge) PR ID, asking for strategy and options."
  (interactive)
  (let* ((id (or id (azdo--pr-id "Complete PR: ")))
         (pr (azdo--get-pr id))
         (strategy (completing-read "Merge strategy: " '("squash" "rebase" "merge") nil t nil nil "squash"))
         (delete (y-or-n-p "Delete source branch after merge? "))
         (bypass (y-or-n-p "Bypass branch policies (requires permission)? "))
         (reason (when bypass (read-string "Bypass reason: "))))
    (when (yes-or-no-p (format "Complete PR #%s \"%s\"? " id (alist-get 'title pr)))
      (azdo--patch (azdo--git-base) (format "/pullrequests/%s" id)
                   (azdo--completion-body pr strategy delete bypass reason))
      (azdo--invalidate-cache)
      (message "PR #%s completion requested" id))))

;;;###autoload
(defun azdo-pr-abandon (&optional id)
  "Abandon PR ID without merging."
  (interactive)
  (let ((id (or id (azdo--pr-id "Abandon PR: "))))
    (when (yes-or-no-p (format "Abandon PR #%s? " id))
      (azdo--patch (azdo--git-base) (format "/pullrequests/%s" id) '((status . "abandoned")))
      (azdo--invalidate-cache)
      (message "PR #%s abandoned" id))))

;;;; Azure Boards ------------------------------------------------------------

(defconst azdo--wi-fields "System.Id,System.Title,System.State,System.WorkItemType,System.AssignedTo")

(defun azdo--wi-field (wi f) (alist-get f (alist-get 'fields wi)))

(defun azdo--wi-line (wi)
  (format "#%s %s [%s %s]" (alist-get 'id wi) (azdo--wi-field wi 'System.Title)
          (azdo--wi-field wi 'System.WorkItemType) (azdo--wi-field wi 'System.State)))

(defun azdo--work-items-by-ids (ids)
  (when ids
    (azdo--value
     (azdo--get (azdo--base) "/wit/workitems"
                `(("ids" . ,(mapconcat #'number-to-string ids ","))
                  ("fields" . ,azdo--wi-fields))))))

(defun azdo--wiql (query)
  (let* ((res (azdo--post (azdo--base) "/wit/wiql" `((query . ,query))))
         (ids (mapcar (lambda (w) (alist-get 'id w)) (alist-get 'workItems res))))
    (azdo--work-items-by-ids (seq-take ids 200))))

(defun azdo--wiql-quote (value)
  "Escape VALUE for use inside a single-quoted WIQL string literal."
  (replace-regexp-in-string "'" "''" value t t))

(defun azdo--assigned-wiql ()
  "WIQL for open work items assigned to me in the current project."
  (format "SELECT [System.Id] FROM WorkItems WHERE [System.AssignedTo] = @Me AND [System.State] NOT IN ('Closed','Done','Removed','Resolved') AND [System.TeamProject] = '%s' ORDER BY [System.ChangedDate] DESC"
          (azdo--wiql-quote (plist-get (azdo--ctx!) :project))))

(defun azdo--assigned-work-items ()
  (azdo--wiql (azdo--assigned-wiql)))

(defun azdo--fetch-work-items-async (cb)
  "Fetch assigned work items asynchronously and pass them (or `azdo--failed') to CB."
  (let ((base (azdo--base)))
    (azdo--request-async
     "POST" (azdo--url base "/wit/wiql")
     (lambda (res)
       (if (eq res azdo--failed)
           (funcall cb res)
         (let ((ids (mapcar (lambda (w) (alist-get 'id w)) (alist-get 'workItems res))))
           (if (null ids)
               (funcall cb nil)
             (azdo--request-async
              "GET" (azdo--url base "/wit/workitems"
                               `(("ids" . ,(mapconcat #'number-to-string (seq-take ids 25) ","))
                                 ("fields" . ,azdo--wi-fields)))
              (lambda (d) (funcall cb (if (eq d azdo--failed) d (azdo--value d)))))))))
     `((query . ,(azdo--assigned-wiql))))))

(defun azdo--read-work-item ()
  (let* ((items (azdo--assigned-work-items))
         (cands (mapcar (lambda (w) (cons (azdo--wi-line w) w)) items)))
    (unless cands (user-error "No assigned work items"))
    (cdr (assoc (completing-read "Work item: " cands nil t) cands))))

(defun azdo--html-to-text (html)
  "Render HTML to plain text."
  (if (or (null html) (string-empty-p html)) ""
    (with-temp-buffer
      (insert html)
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (erase-buffer)
        (let ((shr-use-fonts nil) (shr-width 90) (shr-inhibit-images t))
          (shr-insert-document dom))
        (string-trim (buffer-substring-no-properties (point-min) (point-max)))))))

;;;###autoload
(defun azdo-work-item-open (id)
  "Open work item ID (description, acceptance criteria) in an Org buffer."
  (interactive (list (alist-get 'id (azdo--read-work-item))))
  (let* ((wi (azdo--get (azdo--base) (format "/wit/workitems/%s" id)))
         (f (alist-get 'fields wi))
         (buf (get-buffer-create (format "*azdo-workitem:%s*" id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+title: %s %s\n#+property: ID %s\n\n"
                        (alist-get 'System.WorkItemType f) id id))
        (insert (format "* %s\n  State: %s | Assigned: %s\n\n" (alist-get 'System.Title f)
                        (alist-get 'System.State f)
                        (or (alist-get 'displayName (alist-get 'System.AssignedTo f)) "-")))
        (insert "** Description\n" (azdo--html-to-text (alist-get 'System.Description f)) "\n\n")
        (when-let* ((ac (alist-get 'Microsoft.VSTS.Common.AcceptanceCriteria f)))
          (insert "** Acceptance Criteria\n" (azdo--html-to-text ac) "\n\n"))
        (when-let* ((rs (alist-get 'Microsoft.VSTS.TCM.ReproSteps f)))
          (insert "** Repro Steps\n" (azdo--html-to-text rs) "\n")))
      (org-mode) (goto-char (point-min)) (read-only-mode 1))
    (pop-to-buffer buf)))

;;;###autoload
(defun azdo-work-item-create ()
  "Create a work item."
  (interactive)
  (let* ((type (completing-read "Type: " '("Task" "Bug" "User Story" "Feature") nil t nil nil "Task"))
         (title (read-string "Title: "))
         (res (azdo--request
               "POST" (azdo--url (azdo--base) (format "/wit/workitems/$%s" (url-hexify-string type)))
               (vector `((op . "add") (path . "/fields/System.Title") (value . ,title))
                       `((op . "add") (path . "/fields/System.AssignedTo") (value . ,(alist-get 'uniqueName (azdo--me)))))
               "application/json-patch+json")))
    (azdo--invalidate-cache)
    (message "Created %s #%s" type (alist-get 'id res))))

;;;###autoload
(defun azdo-work-item-transition ()
  "Transition the state of a work item."
  (interactive)
  (let* ((wi (azdo--read-work-item))
         (state (completing-read "New state: " '("New" "Active" "Resolved" "Closed" "Done" "To Do" "In Progress") nil nil)))
    (azdo--request "PATCH" (azdo--url (azdo--base) (format "/wit/workitems/%s" (alist-get 'id wi)))
                   (vector `((op . "add") (path . "/fields/System.State") (value . ,state)))
                   "application/json-patch+json")
    (azdo--invalidate-cache)
    (message "#%s → %s" (alist-get 'id wi) state)))

;;;###autoload
(defun azdo-work-item-list ()
  "Browse assigned work items and open the chosen one."
  (interactive)
  (azdo-work-item-open (alist-get 'id (azdo--read-work-item))))

;;;; Cache & Magit sections --------------------------------------------------

(defvar azdo--cache (make-hash-table :test #'equal))
(defun azdo--cache-key (kind) (cons kind (plist-get (azdo--ctx) :repo)))
(defun azdo--invalidate-cache ()
  (clrhash azdo--cache)
  (clrhash azdo--ctx-cache))

(defun azdo--cache-store (key old data buffer)
  "Store DATA under KEY.  On failure keep OLD data so sections do not vanish.
Refresh BUFFER when it is a Magit buffer and the data changed."
  (when (> (hash-table-count azdo--cache) 200) (clrhash azdo--cache))
  (if (eq data azdo--failed)
      (puthash key (cons (current-time) old) azdo--cache)
    (puthash key (cons (current-time) data) azdo--cache)
    (when (and (not (equal old data)) (buffer-live-p buffer)
               (with-current-buffer buffer (derived-mode-p 'magit-mode)))
      (with-current-buffer buffer (magit-refresh)))))

(defun azdo--cached (kind fetch-fn buffer)
  "Return cached data for KIND; refresh asynchronously via FETCH-FN when stale.
FETCH-FN is called as (FETCH-FN CALLBACK); CALLBACK receives data or
`azdo--failed'.  BUFFER is refreshed on change."
  (let* ((key (azdo--cache-key kind)) (entry (gethash key azdo--cache)))
    (when (or (null entry) (> (float-time (time-since (car entry))) azdo-cache-ttl))
      (puthash key (cons (current-time) (cdr entry)) azdo--cache) ; throttle
      (condition-case err
          (funcall fetch-fn
                   (lambda (data)
                     (azdo--cache-store key (cdr (gethash key azdo--cache)) data buffer)))
        (error (message "azdo: %s" (error-message-string err)))))
    (cdr entry)))

;;;; Commit message #ID completion -------------------------------------------

(defun azdo-commit-capf ()
  "Completion-at-point for Azure Boards work item ids after `#'."
  (when (azdo--ctx)
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward "0-9")
        (when (eq (char-before) ?#)
          (let* ((beg (1- (point)))
                 (items (azdo--cached 'work-items #'azdo--fetch-work-items-async
                                      (current-buffer)))
                 (table (mapcar (lambda (w) (cons (format "#%s" (alist-get 'id w)) w)) items)))
            (list beg end (mapcar #'car table)
                  :exclusive 'no
                  :annotation-function
                  (lambda (c) (when-let* ((w (cdr (assoc c table)))) (format "  %s" (azdo--wi-field w 'System.Title))))
                  :company-docsig #'identity)))))))

(defun azdo--setup-commit-capf ()
  (when (azdo--ctx)
    (add-hook 'completion-at-point-functions #'azdo-commit-capf -10 t)))
(with-eval-after-load 'git-commit
  (add-hook 'git-commit-setup-hook #'azdo--setup-commit-capf))

;;;; Magit sections (loaded lazily; they need Magit's macros) ----------------

(let ((lib (expand-file-name "lib/" (file-name-directory (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path lib)
  (add-to-list 'load-path (file-name-directory (directory-file-name lib))))
(with-eval-after-load 'magit (require 'devops-azdo-magit))

;;;; Dispatchers -------------------------------------------------------------

;;;###autoload (autoload 'azdo-pr-dispatch "devops-azdo" nil t)
(transient-define-prefix azdo-pr-dispatch ()
  "Azure DevOps pull request actions."
  [["Pull Requests"
    ("c" "Create"   azdo-pr-create)
    ("l" "List / filter" azdo-pr-list)
    ("o" "Checkout" azdo-pr-checkout)
    ("r" "Review diff" azdo-pr-review)
    ("t" "Threads"  azdo-pr-threads)]
   ["Decide"
    ("v" "Vote (a/s/w/r/0)" azdo-pr-vote)
    ("m" "Complete / merge" azdo-pr-complete)
    ("x" "Abandon" azdo-pr-abandon)]])

;;;###autoload (autoload 'azdo-boards-dispatch "devops-azdo" nil t)
(transient-define-prefix azdo-boards-dispatch ()
  "Azure Boards actions."
  ["Work Items"
   ("l" "Browse assigned" azdo-work-item-list)
   ("n" "Create" azdo-work-item-create)
   ("t" "Transition state" azdo-work-item-transition)])

;;;###autoload (autoload 'azdo-dispatch "devops-azdo" nil t)
(transient-define-prefix azdo-dispatch ()
  "Azure DevOps command center."
  [["Pull Requests"
    ("p" "PR menu" azdo-pr-dispatch)
    ("c" "Create PR" azdo-pr-create)
    ("l" "List PRs" azdo-pr-list)]
   ["Boards"
    ("b" "Boards menu" azdo-boards-dispatch)
    ("w" "Browse work items" azdo-work-item-list)]
   ["Misc"
    ("g" "Refresh cached data" (lambda () (interactive) (azdo--invalidate-cache) (message "azdo cache cleared")))]])

(provide 'devops-azdo)
;;; devops-azdo.el ends here
