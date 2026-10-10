;;; devops-azure.el --- Azure CLI command center -*- lexical-binding: t; -*-

;;; Commentary:
;; `azure-dispatch': subscriptions, resource discovery, AKS credentials and
;; live logs, all via the `az' CLI, output in the bottom drawer.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'transient)
(require 'compile)

(declare-function ide-run-with-timeout "00-core")

(defun azure--az ()
  "Return the az executable or signal a user error."
  (or (executable-find "az") (user-error "Azure CLI (`az') is not installed")))

(defun azure--json (&rest args)
  "Run `az ARGS -o json' and return parsed JSON (list of alists)."
  (let* ((res (apply #'ide-run-with-timeout 120 (azure--az) (append args '("-o" "json"))))
         (code (car res)))
    (cond ((eq code 'timeout) (user-error "az %s timed out after 120s" (car args)))
          ((not (eql code 0))
           (user-error "az %s failed (exit %s): %s" (car args) code (string-trim (nth 2 res)))))
    (json-parse-string (nth 1 res) :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil)))

(defun azure--run (name &rest args)
  "Run `az ARGS' in the bottom drawer, buffer *azure-NAME*."
  (let ((compilation-buffer-name-function (lambda (_) (format "*azure-%s*" name))))
    (compilation-start (mapconcat #'shell-quote-argument (cons (azure--az) args) " ")
                       'compilation-mode)))

(defun azure--pick (prompt items name-fn &optional annotate-fn)
  "Choose from ITEMS using NAME-FN for labels."
  (let* ((cands (mapcar (lambda (i) (cons (funcall name-fn i) i)) items))
         (completion-extra-properties
          (when annotate-fn
            `(:annotation-function ,(lambda (c) (funcall annotate-fn (cdr (assoc c cands))))))))
    (unless cands (user-error "Nothing found"))
    (cdr (assoc (completing-read prompt cands nil t) cands))))

;;;###autoload
(defun azure-subscription-switch ()
  "Interactively switch the active Azure subscription."
  (interactive)
  (let ((sub (azure--pick "Subscription: " (azure--json "account" "list")
                          (lambda (s) (alist-get 'name s))
                          (lambda (s) (format "  %s%s" (alist-get 'id s)
                                              (if (alist-get 'isDefault s) "  [active]" ""))))))
    (let ((res (ide-run-with-timeout 60 (azure--az) "account" "set" "--subscription" (alist-get 'id sub))))
      (unless (eql (car res) 0) (user-error "az account set failed: %s" (string-trim (nth 2 res)))))
    (setq azure-current-subscription (alist-get 'name sub))
    (message "Active subscription: %s" (alist-get 'name sub))))

(defvar azure-current-subscription nil "Name of the active subscription, for the modeline.")

(defun azure--table (name &rest args)
  "Show `az ARGS -o table' in drawer buffer *azure-NAME*."
  (apply #'azure--run name (append args '("-o" "table"))))

(defun azure-resource-groups () "List resource groups." (interactive) (azure--table "groups" "group" "list"))
(defun azure-aks-list () "List AKS clusters." (interactive) (azure--table "aks" "aks" "list"))
(defun azure-containerapps-list () "List Container Apps." (interactive) (azure--table "containerapps" "containerapp" "list"))
(defun azure-storage-list () "List storage accounts." (interactive) (azure--table "storage" "storage" "account" "list"))
(defun azure-keyvaults-list () "List Key Vaults." (interactive) (azure--table "keyvaults" "keyvault" "list"))

;;;###autoload
(defun azure-aks-get-credentials ()
  "Import credentials of an AKS cluster into the local kubeconfig for kubel."
  (interactive)
  (let ((c (azure--pick "AKS cluster: " (azure--json "aks" "list")
                        (lambda (c) (alist-get 'name c))
                        (lambda (c) (format "  %s" (alist-get 'resourceGroup c))))))
    (azure--run "aks-credentials" "aks" "get-credentials"
                "--resource-group" (alist-get 'resourceGroup c)
                "--name" (alist-get 'name c) "--overwrite-existing")))

;;;###autoload
(defun azure-containerapp-logs ()
  "Stream logs of a Container App into the bottom drawer."
  (interactive)
  (let ((a (azure--pick "Container App: " (azure--json "containerapp" "list")
                        (lambda (a) (alist-get 'name a))
                        (lambda (a) (format "  %s" (alist-get 'resourceGroup a))))))
    (azure--run (format "logs:%s" (alist-get 'name a)) "containerapp" "logs" "show"
                "--name" (alist-get 'name a) "--resource-group" (alist-get 'resourceGroup a)
                "--follow" "--tail" "100")))

;;;###autoload
(defun azure-resource-query (query)
  "Run an Azure Resource Graph QUERY."
  (interactive "sResource Graph query: ")
  (azure--run "graph" "graph" "query" "-q" query "-o" "table"))

;;;###autoload (autoload 'azure-dispatch "devops-azure" nil t)
(transient-define-prefix azure-dispatch ()
  "Azure cloud command center."
  [["Account"
    ("s" "Switch subscription" azure-subscription-switch)
    ("l" "Login" (lambda () (interactive) (azure--run "login" "login")))]
   ["Discover"
    ("g" "Resource groups" azure-resource-groups)
    ("k" "AKS clusters" azure-aks-list)
    ("c" "Container Apps" azure-containerapps-list)
    ("t" "Storage accounts" azure-storage-list)
    ("v" "Key Vaults" azure-keyvaults-list)
    ("q" "Resource Graph query" azure-resource-query)]
   ["Act"
    ("a" "AKS get-credentials" azure-aks-get-credentials)
    ("L" "Container App logs" azure-containerapp-logs)]])

(provide 'devops-azure)
;;; devops-azure.el ends here
