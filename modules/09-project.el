;;; 09-project.el --- project.el, direnv, ripgrep -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in project.el as the project backbone.

;;; Code:

(use-package project
  :ensure nil
  :init
  (setq project-switch-commands '((project-find-file "Find file" ?f)
                                  (consult-ripgrep "Ripgrep" ?g)
                                  (project-dired "Dired" ?d)
                                  (magit-project-status "Magit" ?m)
                                  (project-eshell "Eshell" ?e))
        project-vc-extra-root-markers '(".project" "go.mod" "pyproject.toml" "package.json")))

;; Terminal: ghostel (libghostty).  Its prebuilt native module is fetched on
;; first interactive use (`ghostel-module-auto-install' asks; loading never prompts).
(use-package ghostel :commands ghostel)

(setq grep-command "rg --no-heading -n "
      xref-search-program 'ripgrep)

(defun ide-project-terminal ()
  "Open a terminal in the project root (bottom drawer)."
  (interactive)
  (let ((default-directory (if-let* ((p (project-current))) (project-root p) default-directory)))
    (if (fboundp 'ghostel) (ghostel) (eshell))))

(provide '09-project)
;;; 09-project.el ends here
