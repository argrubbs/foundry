;;; lang-ansible.el --- Ansible -*- lexical-binding: t; -*-

;;; Commentary:
;; ansible minor mode on YAML, ansible-language-server via Eglot, ansible-doc, and
;; ansible-intelligence (lisp/) for variable definitions and references.  The
;; language server stays the primary provider; ansible-intelligence only answers
;; for Ansible/Jinja identifiers it owns (see its hook depths below).

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
  "Non-nil if the current buffer looks like an Ansible file.
Matches by path (playbooks, roles, vars directories) or, failing that, by a
top-level play or task key near the top of the file."
  (or (and buffer-file-name
           (string-match-p "\\(playbook\\|roles/\\|tasks/\\|handlers/\\|group_vars/\\|host_vars/\\)"
                           buffer-file-name)
           t)
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward "^[[:space:]-]*\\(?:hosts\\|tasks\\|roles\\):"
                                (min (+ (point-min) 20000) (point-max)) t)
             t))))

;; Variable definitions/references, Jinja completion and diagnostics.  It adds
;; its xref backend ahead of Eglot's (declining outside Ansible/Jinja
;; identifiers) and its completion, eldoc and Flymake sources after Eglot's.
(add-to-list 'load-path (expand-file-name "lisp/ansible-intelligence/" user-emacs-directory))
(autoload 'ansible-intelligence-mode "ansible-intelligence" nil t)

(defun ide--maybe-ansible-intelligence ()
  "Enable `ansible-intelligence-mode' in Ansible-looking YAML buffers."
  (when (and (ide--ansible-file-p) (not (bound-and-true-p ansible-intelligence-mode)))
    (ansible-intelligence-mode 1)))

(add-hook 'yaml-ts-mode-hook #'ide--maybe-ansible-intelligence)

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
