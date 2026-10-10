;;; devops-k8s.el --- Kubernetes porcelain -*- lexical-binding: t; -*-

;;; Commentary:
;; kubel for cluster browsing, k8s-mode for manifests.

;;; Code:

(use-package kubel
  :commands (kubel kubel-set-context kubel-set-namespace)
  :init (setq kubel-use-namespace-list 'on))

(use-package kubel-evil :disabled t)

(use-package k8s-mode
  :mode ("/k8s/.*\\.ya?ml\\'" "/manifests/.*\\.ya?ml\\'" "/kubernetes/.*\\.ya?ml\\'")
  :init (setq k8s-site-docs-version "v1.30"))

(defun ide-kubectl-logs (pod &optional namespace)
  "Stream logs for POD (in NAMESPACE) into the bottom drawer."
  (interactive
   (list (completing-read "Pod: " (process-lines "kubectl" "get" "pods" "-o" "name") nil t)))
  (let ((compilation-buffer-name-function (lambda (_) (format "*kubectl-logs:%s*" pod))))
    (compilation-start
     (format "kubectl logs -f --tail=200 %s%s" (shell-quote-argument pod)
             (if namespace (format " -n %s" (shell-quote-argument namespace)) ""))
     'compilation-mode)))

(provide 'devops-k8s)
;;; devops-k8s.el ends here
