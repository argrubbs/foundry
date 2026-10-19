;;; 01-lang-modes.el --- Which files get which language, and when its module loads -*- lexical-binding: t; -*-

;;; Commentary:
;; The eager half of language support.  Each lang-*.el in this directory is a few
;; hundred lines of commands, menus and linters that only matter once a file of that
;; language is open, so init.el does not load them: `ide-lang-lazy' loads a module the
;; first time one of its major modes starts (before the mode's hooks run, so the hooks
;; the module adds still apply to that first buffer).
;;
;; What must exist before that moment lives here: file-name associations, the
;; third-party packages (declared at startup so that `make packages' installs them and
;; Elpaca activates them), and autoloads for the few commands reachable from outside
;; the language's own buffers.  In batch runs (tests, docs, byte-compilation) every
;; module loads at once, so they see the whole configuration.

;;; Code:

(require '00-lang-lib)

(defvar emmet-jsx-major-modes)
(defvar imp-default-user-filters)
(defvar markdown-code-lang-modes)
(defvar org-mode-map)

;;;; Python --------------------------------------------------------------------------------

(ide-lang-lazy 'lang-python '(python-mode python-ts-mode))

(use-package python-pytest
  :commands (python-pytest-dispatch python-pytest-function python-pytest-file python-pytest)
  :init (setq python-pytest-executable "python -m pytest"))

(use-package envrc
  :hook (python-ts-mode . envrc-mode))

(use-package pet
  :hook (python-ts-mode . pet-mode)
  :init (setq pet-debug nil))

;;;; Go -----------------------------------------------------------------------------------

(ide-lang-lazy 'lang-go '(go-mode go-ts-mode go-mod-ts-mode))

(use-package gotest
  :commands (go-test-current-test go-test-current-file go-test-current-project
             go-test-current-benchmark))

(use-package go-gen-test :commands go-gen-test-dwim)

;;;; Rust ---------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(ide-lang-lazy 'lang-rust '(rust-ts-mode rust-mode))
(autoload 'ide-rust-install-debugger "lang-rust" nil t)

;;;; Lua ----------------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-ts-mode))
(ide-lang-lazy 'lang-lua '(lua-ts-mode))

;;;; Web: TypeScript / JavaScript / HTML / CSS --------------------------------------------

(dolist (entry '(("\\.[cm]?ts\\'" . typescript-ts-mode)
                 ("\\.tsx\\'" . tsx-ts-mode)
                 ("\\.[cm]?js\\'" . js-ts-mode)
                 ("\\.jsx\\'" . js-ts-mode)
                 ("\\.css\\'" . css-ts-mode)
                 ("\\.html?\\'" . html-ts-mode)))
  (add-to-list 'auto-mode-alist entry))

(ide-lang-lazy 'lang-web '(typescript-ts-mode tsx-ts-mode js-ts-mode css-ts-mode html-ts-mode))
(autoload 'ide-web-install-debugger "lang-web" nil t)

(use-package emmet-mode
  :commands emmet-mode
  :hook ((html-ts-mode css-ts-mode tsx-ts-mode js-ts-mode) . emmet-mode)
  :init (setq emmet-move-cursor-between-quotes t)
  :config
  (dolist (m '(tsx-ts-mode js-ts-mode typescript-ts-mode))
    (add-to-list 'emmet-jsx-major-modes m)))

(use-package nodejs-repl
  :commands (nodejs-repl nodejs-repl-send-region nodejs-repl-send-buffer nodejs-repl-switch-to-repl))

(use-package simple-httpd :commands (httpd-start httpd-stop httpd-running-p))
(use-package impatient-mode
  :commands (impatient-mode imp-visit-buffer)
  :config
  ;; a nil filter serves the buffer as it is, instead of as highlighted source
  (dolist (m '(html-ts-mode css-ts-mode js-ts-mode))
    (add-to-list 'imp-default-user-filters (cons m nil))))

;;;; PowerShell ---------------------------------------------------------------------------

(ide-lang-lazy 'lang-powershell '(powershell-mode))
(autoload 'ide-powershell-install-pses "lang-powershell" nil t)

(use-package powershell
  :mode ("\\.ps[dm]?1\\'" . powershell-mode)
  :init (setq powershell-indent-level 4))

;;;; Markdown -----------------------------------------------------------------------------

(ide-lang-lazy 'lang-markdown '(markdown-mode gfm-mode markdown-ts-mode))
;; Reachable from Org buffers and M-x before any Markdown file is open.
(autoload 'ide-markdown-to-org "lang-markdown" nil t)
(autoload 'ide-org-to-markdown "lang-markdown" nil t)
(autoload 'ide-markdown-org-menu "lang-markdown" nil t)
;; Emacs 31 does not autoload markdown-ts-mode: make M-x markdown-ts-mode work.
(autoload 'markdown-ts-mode "markdown-ts-mode" nil t)

(use-package markdown-toc :commands markdown-toc-generate-or-refresh-toc)

(use-package markdown-mode
  :mode (("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\)\\'" . markdown-mode)
         ("\\(?:README\\|CHANGELOG\\|CONTRIBUTING\\|CODE_OF_CONDUCT\\|SECURITY\\|SUPPORT\\|HISTORY\\|CHANGES\\|PULL_REQUEST_TEMPLATE\\)\\.\\(?:md\\|markdown\\)\\'" . gfm-mode)
         ("/\\.github/.*\\.md\\'" . gfm-mode))
  :init
  (setq markdown-fontify-code-blocks-natively t
        markdown-enable-math t
        markdown-enable-wiki-links t
        markdown-wiki-link-search-type '(parent-directories)
        markdown-italic-underscore t
        markdown-unordered-list-item-prefix "- "
        markdown-list-indent-width 2
        markdown-asymmetric-header nil
        markdown-header-scaling t
        markdown-hide-urls nil
        markdown-gfm-use-electric-backquote nil
        markdown-use-pandoc-style-yaml-metadata t
        markdown-spaces-after-code-fence 0
        markdown-indent-on-enter 'indent-and-new-item)
  :config
  ;; Fenced blocks are fontified by the tree-sitter modes where the grammar exists.
  (dolist (entry '(("sh" . bash-ts-mode) ("bash" . bash-ts-mode) ("shell" . bash-ts-mode)
                   ("console" . bash-ts-mode) ("python" . python-ts-mode) ("py" . python-ts-mode)
                   ("js" . js-ts-mode) ("javascript" . js-ts-mode) ("jsx" . tsx-ts-mode)
                   ("ts" . typescript-ts-mode) ("typescript" . typescript-ts-mode)
                   ("tsx" . tsx-ts-mode) ("json" . json-ts-mode) ("jsonc" . json-ts-mode)
                   ("yaml" . yaml-ts-mode) ("yml" . yaml-ts-mode) ("toml" . toml-ts-mode)
                   ("go" . go-ts-mode) ("rust" . rust-ts-mode) ("rs" . rust-ts-mode)
                   ("lua" . lua-ts-mode) ("css" . css-ts-mode) ("html" . html-ts-mode)
                   ("dockerfile" . dockerfile-ts-mode) ("hcl" . hcl-mode)
                   ("terraform" . hcl-mode) ("nix" . nix-ts-mode) ("sql" . sql-mode)
                   ("elisp" . emacs-lisp-mode) ("el" . emacs-lisp-mode)
                   ("org" . org-mode) ("diff" . diff-mode)))
    (when (or (not (string-suffix-p "-ts-mode" (symbol-name (cdr entry))))
              (let ((lang (replace-regexp-in-string
                           "-ts-mode\\'" "" (symbol-name (cdr entry)))))
                (treesit-language-available-p (intern (if (equal lang "tsx") "tsx" lang)))))
      (setf (alist-get (car entry) markdown-code-lang-modes nil nil #'equal) (cdr entry)))))

(use-package ox-gfm :after ox :defer t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c o m") #'ide-markdown-org-menu))

;;;; Terraform / Packer -------------------------------------------------------------------

(ide-lang-lazy 'lang-iac '(terraform-mode hcl-mode))
(autoload 'iac-dispatch "lang-iac" nil t)

(use-package hcl-mode
  :mode ("\\.hcl\\'" "\\.pkr\\.hcl\\'" "\\.pkrvars\\.hcl\\'"))

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tfvars\\'")
  :hook (terraform-mode . terraform-format-on-save-mode))

;;;; TOML, shell, Dockerfile, Compose, Terraform linting ----------------------------------

(add-to-list 'auto-mode-alist '("\\(?:Cargo\\|poetry\\|uv\\|pdm\\)\\.lock\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("/Pipfile\\'" . toml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . bash-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . conf-unix-mode))

(ide-lang-lazy 'lang-devops '(toml-ts-mode bash-ts-mode sh-mode dockerfile-ts-mode dockerfile-mode
                              yaml-ts-mode yaml-mode terraform-mode))

;;;; YAML: Ansible, Helm, Kubernetes, CI ---------------------------------------------------

(ide-lang-lazy 'lang-ansible '(yaml-ts-mode))

(use-package ansible :commands ansible)

(use-package ansible-doc
  :commands (ansible-doc ansible-doc-mode)
  :hook (yaml-ts-mode . ansible-doc-mode)
  :init (setq ansible-doc-buffer-name "*ansible-doc*"))

;; Chart templates get the Helm modes, which lang-k8s defines; the remaps route
;; yaml-mode and k8s-mode (devops-k8s.el) through the same decision.
(autoload 'ide-helm-auto-mode "lang-k8s")
(autoload 'ide-k8s--remap-k8s-mode "lang-k8s")
(autoload 'ide-k8s--remap-yaml-mode "lang-k8s")
(add-to-list 'auto-mode-alist '("/templates/.*\\.\\(ya?ml\\|tpl\\|txt\\)\\'" . ide-helm-auto-mode))
(add-to-list 'auto-mode-alist '("/_[^/]*\\.tpl\\'" . ide-helm-auto-mode))
(add-to-list 'major-mode-remap-alist '(k8s-mode . ide-k8s--remap-k8s-mode))
(add-to-list 'major-mode-remap-alist '(yaml-mode . ide-k8s--remap-yaml-mode))

(ide-lang-lazy 'lang-k8s '(yaml-ts-mode k8s-mode))
(autoload 'ide-kubectl-logs "lang-k8s" nil t)

(provide '01-lang-modes)
;;; 01-lang-modes.el ends here
