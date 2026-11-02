;;; devops-azdo-test.el --- Tests for modules/12-devops/devops-azdo.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for the Azure DevOps forge.  Nothing reaches the network:
;; HTTP responses, `az' and the repository context are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'devops-azdo)

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar url-http-content-type)

(defun azdo-test--response (status body &optional ctype)
  "A buffer that looks like a url.el response with STATUS, BODY and content type CTYPE."
  (let ((buf (generate-new-buffer " *azdo-test-response*")))
    (with-current-buffer buf
      (insert "HTTP/1.1 " (number-to-string status) " X\r\n\r\n")
      (setq-local url-http-response-status status)
      (setq-local url-http-end-of-headers (point))
      (setq-local url-http-content-type ctype)
      (insert body))
    buf))

(defmacro azdo-test--with-response (status body ctype &rest forms)
  "Run FORMS with every synchronous request answered by STATUS, BODY and CTYPE."
  (declare (indent 3))
  `(cl-letf (((symbol-function 'azdo--auth-header) (lambda () "Basic x"))
             ((symbol-function 'url-retrieve-synchronously)
              (lambda (&rest _) (azdo-test--response ,status ,body ,ctype))))
     ,@forms))

;;;; Voting from the PR list -----------------------------------------------------------

(ert-deftest azdo-pr-list-votes-on-the-selected-pr ()
  (let (voted)
    (cl-letf (((symbol-function 'azdo--read-pr) (lambda (&rest _) '((pullRequestId . 42))))
              ((symbol-function 'completing-read) (lambda (&rest _) "vote"))
              ((symbol-function 'azdo--read-vote-key) (lambda () ?a))
              ((symbol-function 'azdo--pr-id-at-point) (lambda () 7))
              ((symbol-function 'azdo--cast-vote) (lambda (id vote) (setq voted (list id vote)))))
      (azdo-pr-list 'all))
    (should (equal voted '(42 10)))))

(ert-deftest azdo-pr-vote-without-id-uses-the-current-pr ()
  (let (voted)
    (cl-letf (((symbol-function 'azdo--pr-id-at-point) (lambda () 7))
              ((symbol-function 'azdo--cast-vote) (lambda (id vote) (setq voted (list id vote)))))
      (azdo-pr-vote ?r))
    (should (equal voted '(7 -10)))))

;;;; Compose callbacks keep their repository -------------------------------------------

(ert-deftest azdo-compose-callback-runs-in-the-originating-repo ()
  (let* ((repo (file-name-as-directory (make-temp-file "azdo-repo" t)))
         (ctx '(:org "o" :project "p" :repo "r" :remote "origin"))
         posted seen-dir)
    (unwind-protect
        (cl-letf (((symbol-function 'azdo--ctx!)
                   (lambda () (if (equal default-directory repo) ctx
                                (user-error "Not inside an Azure DevOps repository"))))
                  ((symbol-function 'azdo--hunk-position) (lambda () '("a.el" 3 right)))
                  ((symbol-function 'azdo--post)
                   (lambda (base path _body &rest _)
                     (setq posted (list base path seen-dir)))))
          (with-temp-buffer
            (setq default-directory repo)
            (setq azdo-current-pr 5)
            (azdo-pr-comment-at-point))
          ;; Submitting happens with some other buffer (outside the repo) around.
          (let ((compose (get-buffer "*azdo-compose:comment-5*")))
            (should compose)
            (with-temp-buffer
              (setq default-directory "/")
              (set-buffer compose)
              (insert "looks good")
              (azdo-compose-finish)))
          (should posted)
          (should (equal (car posted) (azdo--git-base ctx)))
          (should (equal (cadr posted) "/pullrequests/5/threads"))
          ;; Generic callbacks see the originating directory too.
          (with-temp-buffer
            (setq default-directory repo)
            (azdo--compose "t" nil (lambda (_text) (setq seen-dir default-directory))))
          (with-current-buffer "*azdo-compose:t*"
            (insert "x")
            (let ((default-directory "/")) (azdo-compose-finish)))
          (should (equal seen-dir repo)))
      (delete-directory repo t))))

(ert-deftest azdo-thread-reply-survives-a-killed-threads-buffer ()
  (let ((ctx '(:org "o" :project "p" :repo "r" :remote "origin"))
        posted refreshed)
    (cl-letf (((symbol-function 'azdo--thread-at-point) (lambda () '((id . 9))))
              ((symbol-function 'azdo--post) (lambda (&rest _) (setq posted t)))
              ((symbol-function 'azdo-threads-refresh) (lambda () (setq refreshed t))))
      (let ((threads (generate-new-buffer "*azdo-threads-test*")))
        (with-current-buffer threads
          (setq-local azdo--threads-ctx ctx)
          (setq azdo-current-pr 3)
          (azdo-thread-reply))
        (kill-buffer threads))
      (with-current-buffer "*azdo-compose:reply-9*"
        (insert "thanks")
        (azdo-compose-finish)))
    (should posted)
    (should-not refreshed)))

;;;; Entra token lifetime --------------------------------------------------------------

(defun azdo-test--az-json (expires-in)
  "JSON like `az account get-access-token -o json' for a token valid EXPIRES-IN seconds."
  (let ((exp (+ (truncate (float-time)) expires-in)))
    (format "{\"accessToken\":\"tok-%d\",\"expiresOn\":\"%s\",\"expires_on\":%d,\"tokenType\":\"Bearer\"}"
            exp (format-time-string "%Y-%m-%d %H:%M:%S.000000" (seconds-to-time exp)) exp)))

(defmacro azdo-test--with-az (expires-in &rest body)
  "Run BODY with a stubbed `az' that hands out tokens valid EXPIRES-IN seconds.
The variable `calls' counts how often `az' ran."
  (declare (indent 1))
  `(let ((azdo--token-cache nil) (calls 0))
     (cl-letf (((symbol-function 'getenv)
                (let ((orig (symbol-function 'getenv)))
                  (lambda (v &rest r) (unless (equal v "AZURE_DEVOPS_EXT_PAT") (apply orig v r)))))
               ((symbol-function 'auth-source-search) (lambda (&rest _) nil))
               ((symbol-function 'executable-find) (lambda (&rest _) "/usr/bin/az"))
               ((symbol-function 'ide-run-with-timeout)
                (lambda (&rest _) (cl-incf calls) (list 0 (azdo-test--az-json ,expires-in) ""))))
       ,@body)))

(ert-deftest azdo-entra-token-uses-its-real-expiry ()
  ;; A fresh token is reused.
  (azdo-test--with-az 3600
    (should (string-prefix-p "Bearer tok-" (azdo--auth-header)))
    (azdo--auth-header)
    (should (= calls 1))
    (should (time-less-p (car azdo--token-cache) (time-add nil 3600))))
  ;; A cached token az hands out with two minutes left is not kept for 50 minutes.
  (azdo-test--with-az 120
    (azdo--auth-header)
    (azdo--auth-header)
    (should (= calls 2))))

(ert-deftest azdo-token-expiry-falls-back-to-local-expires-on ()
  (let* ((in-an-hour (time-add nil 3600))
         (local (format-time-string "%Y-%m-%d %H:%M:%S.000000" in-an-hour))
         (exp (azdo--token-expiry `((expiresOn . ,local)))))
    (should (< (abs (- (float-time exp) (- (float-time in-an-hour) azdo--token-margin))) 2))))

(ert-deftest azdo-401-drops-the-cached-token ()
  (let ((azdo--token-cache (cons (time-add nil 3600) "stale")))
    (azdo-test--with-response 401 "{\"message\":\"nope\"}" "application/json"
      (should-error (azdo--get "http://127.0.0.1:1" "/x")))
    (should-not azdo--token-cache)))

;;;; Bad PATs: 203 and HTML sign-in pages ----------------------------------------------

(ert-deftest azdo-203-sign-in-page-is-an-auth-error ()
  (azdo-test--with-response 203 "<html><body>Sign in</body></html>" "text/html; charset=utf-8"
    (let ((err (should-error (azdo--get "http://127.0.0.1:1" "/pullrequests"))))
      (should (string-match-p "authentication failed" (error-message-string err))))))

(ert-deftest azdo-html-where-json-expected-is-an-auth-error ()
  (azdo-test--with-response 200 "\n<!DOCTYPE html><html>Sign in</html>" "text/html"
    (let ((err (should-error (azdo--get "http://127.0.0.1:1" "/pullrequests"))))
      (should (string-match-p "sign-in page" (error-message-string err)))))
  ;; Real JSON still goes through.
  (azdo-test--with-response 200 "{\"value\":[1,2]}" "application/json"
    (should (equal (azdo--value (azdo--get "http://127.0.0.1:1" "/x")) '(1 2)))))

(ert-deftest azdo-async-auth-failure-reports-failed ()
  (let (got)
    (cl-letf (((symbol-function 'azdo--auth-header) (lambda () "Basic x"))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _)
                 (let ((buf (azdo-test--response 203 "<html>Sign in</html>" "text/html")))
                   (with-current-buffer buf (funcall cb nil))
                   buf))))
      (azdo--request-async "GET" "http://127.0.0.1:1/x" (lambda (d) (setq got d))))
    (should (eq got azdo--failed))))

(provide 'devops-azdo-test)
;;; devops-azdo-test.el ends here
