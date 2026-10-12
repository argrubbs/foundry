;;; devops-k8s.el --- Kubernetes porcelain -*- lexical-binding: t; -*-

;;; Commentary:
;; kubel for cluster browsing, k8s-mode for manifests.

;;; Code:

(use-package kubel
  :commands (kubel kubel-set-context kubel-set-namespace)
  :init (setq kubel-use-namespace-list 'on))

(use-package k8s-mode
  :mode ("/k8s/.*\\.ya?ml\\'" "/manifests/.*\\.ya?ml\\'" "/kubernetes/.*\\.ya?ml\\'")
  :init (setq k8s-site-docs-version "v1.30"))

(provide 'devops-k8s)
;;; devops-k8s.el ends here
