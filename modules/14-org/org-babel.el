;;; org-babel.el --- Polyglot executable runbooks -*- lexical-binding: t; -*-

;;; Commentary:
;; Babel languages for DevOps runbooks.  Evaluation asks for confirmation
;; except for shell-free, read-only languages.

;;; Code:

(require 'cl-lib)
(defvar org-src-lang-modes)
(defvar org-confirm-babel-evaluate)

(use-package restclient :mode ("\\.http\\'" . restclient-mode))
(use-package ob-restclient :after (org restclient))
(use-package ob-go :after org)
(use-package ob-async :after org)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   (append '((python . t) (shell . t) (js . t) (emacs-lisp . t))
           (cl-loop for (lang . feat) in '((go . ob-go) (restclient . ob-restclient))
                    when (locate-library (symbol-name feat))
                    collect (cons lang t))))
  (dolist (m '(("dockerfile" . dockerfile) ("docker-file" . dockerfile)
               ("terraform" . terraform) ("json" . json-ts) ("yaml" . yaml-ts)))
    (add-to-list 'org-src-lang-modes m))
  (setq org-confirm-babel-evaluate #'ide-org-confirm-babel))

(defun ide-org-confirm-babel (lang _body)
  "Return nil (no prompt) only for LANG blocks that cannot execute code.
`emacs-lisp' must always prompt: export (including Confluence publishing)
evaluates blocks, and ingested pages are untrusted."
  (not (member lang '("json" "yaml" "docker-file" "dockerfile"))))

(provide 'org-babel)
;;; org-babel.el ends here
