;;; org-core.el --- Org, agenda, capture -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-mode with modern typography, agenda views and capture templates.

;;; Code:

(defvar ide-notes-dir (expand-file-name "notes/" user-emacs-directory)
  "Root directory for Org notes (override in custom.el).")
(make-directory ide-notes-dir t)

(ide-builtin org
  :init
  (setq org-directory ide-notes-dir
        org-agenda-files (list (expand-file-name "tasks.org" ide-notes-dir)
                               (expand-file-name "projects.org" ide-notes-dir))
        org-id-locations-file (expand-file-name "org-id-locations" ide-var-dir)
        org-persist-directory (expand-file-name "org-persist/" ide-cache-dir)
        org-log-done 'time
        org-startup-indented t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-content-indentation 0
        org-todo-keywords '((sequence "TODO(t)" "DOING(d)" "WAIT(w)" "|" "DONE(x)" "CANCELLED(c)")))
  :config
  (require 'org-tempo)
  (dolist (tpl '(("py" . "src python") ("go" . "src go") ("sh" . "src bash")
                 ("tf" . "src terraform") ("sq" . "src sql") ("http" . "src restclient")
                 ("yml" . "src yaml") ("js" . "src json")))
    (add-to-list 'org-structure-template-alist tpl)))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :init (setq org-modern-star 'replace))

(with-eval-after-load 'org
  (setq org-agenda-custom-commands
        '(("d" "Daily tasks"
           ((agenda "" ((org-agenda-span 1)))
            (todo "DOING")))
          ("m" "Project milestones" tags-todo "milestone")
          ("i" "Azure DevOps issues" tags-todo "azdo")
          ("w" "Waiting" todo "WAIT")))
  (setq org-capture-templates
        `(("n" "Quick note" entry (file+headline ,(expand-file-name "inbox.org" ide-notes-dir) "Notes")
           "* %?\n  %U\n")
          ("t" "Task" entry (file+headline ,(expand-file-name "tasks.org" ide-notes-dir) "Tasks")
           "* TODO %?\n  %U\n")
          ("s" "Code snippet" entry (file+headline ,(expand-file-name "snippets.org" ide-notes-dir) "Snippets")
           "* %?\n  %U\n  #+begin_src %^{language|python|go|bash|terraform|sql}\n  %i\n  #+end_src\n")
          ("p" "Post-mortem" entry (file+headline ,(expand-file-name "postmortems.org" ide-notes-dir) "Post-mortems")
           "* %^{Incident} :postmortem:\n  %U\n** Summary\n   %?\n** Impact\n** Timeline\n** Root cause\n** Action items\n"))))

(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o n") #'org-capture)

(provide 'org-core)
;;; org-core.el ends here
