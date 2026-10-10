;;; lang-ansible.el --- Ansible -*- lexical-binding: t; -*-

;;; Commentary:
;; ansible minor mode on YAML, ansible-language-server via Eglot, ansible-doc.

;;; Code:

(eval-when-compile (require 'eglot))

(use-package ansible
  :commands ansible
  :hook (yaml-ts-mode . ide--maybe-ansible))

(use-package ansible-doc
  :commands (ansible-doc ansible-doc-mode)
  :hook (yaml-ts-mode . ansible-doc-mode)
  :init (setq ansible-doc-buffer-name "*ansible-doc*"))

(defun ide--ansible-file-p ()
  "Non-nil if the current buffer looks like an Ansible file."
  (and buffer-file-name
       (string-match-p "\\(playbook\\|roles/\\|tasks/\\|handlers/\\|group_vars/\\|host_vars/\\)"
                       buffer-file-name)))

(defun ide--maybe-ansible ()
  "Enable `ansible' minor mode for Ansible-looking YAML buffers."
  (when (and (ide--ansible-file-p) (fboundp 'ansible)) (ansible 1)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               `(yaml-ts-mode
                 . ,(lambda (&rest _)
                      (if (and (ide--ansible-file-p) (executable-find "ansible-language-server"))
                          '("ansible-language-server" "--stdio")
                        '("yaml-language-server" "--stdio"))))))

(defun ide-ansible-doc-at-point ()
  "Show `ansible-doc' for the module at point in the bottom drawer."
  (interactive)
  (if-let* ((word (thing-at-point 'symbol t)))
      (ansible-doc word)
    (call-interactively #'ansible-doc)))

(provide 'lang-ansible)
;;; lang-ansible.el ends here
