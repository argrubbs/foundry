;;; lang-ansible.el --- Ansible -*- lexical-binding: t; -*-

;;; Commentary:
;; ansible minor mode on YAML, ansible-language-server via Eglot, ansible-doc, and
;; ansible-xref (lisp/) for variable definitions and references.  The language
;; server stays the primary provider; ansible-xref only answers for variables,
;; includes and roles it can find in the project.

;;; Code:

(eval-when-compile (require 'eglot))

(defun ide--ansible-file-p ()
  "Non-nil if the current buffer looks like an Ansible file.
Matches by path (playbooks, roles, vars directories) or, failing that, by a
top-level play or task key near the top of the file."
  (or (and buffer-file-name
           (string-match-p "\\(playbook\\|roles/\\|tasks/\\|handlers/\\|group_vars/\\|host_vars/\\|vars/\\)"
                           buffer-file-name)
           t)
      (save-excursion
        (goto-char (point-min))
        (and (re-search-forward "^[[:space:]-]*\\(?:hosts\\|tasks\\|roles\\):"
                                (min (+ (point-min) 20000) (point-max)) t)
             t))))

;; Variable definitions and references via xref.  The backend goes ahead of
;; Eglot's (depth -90) and declines what it cannot resolve.
(autoload 'ansible-xref-backend "ansible-xref")

(defun ide--maybe-ansible-xref ()
  "Look up variables, includes and roles with `ansible-xref' in Ansible YAML buffers."
  (when (ide--ansible-file-p)
    (add-hook 'xref-backend-functions #'ansible-xref-backend -90 t)))

(add-hook 'yaml-ts-mode-hook #'ide--maybe-ansible-xref)

(defun ide--maybe-ansible ()
  "Enable `ansible' minor mode for Ansible-looking YAML buffers."
  (when (and (ide--ansible-file-p) (fboundp 'ansible)) (ansible 1)))

(add-hook 'yaml-ts-mode-hook #'ide--maybe-ansible)

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
