;;; 05-treesitter.el --- Tree-sitter grammars and mode remapping -*- lexical-binding: t; -*-

;;; Commentary:
;; Built-in treesit with grammars installed into the sandbox.

;;; Code:

(require 'treesit)

(setq treesit-language-source-alist
      '((python     "https://github.com/tree-sitter/tree-sitter-python")
        (go         "https://github.com/tree-sitter/tree-sitter-go")
        (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
        (hcl        "https://github.com/tree-sitter-grammars/tree-sitter-hcl")
        (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
        (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
        (bash       "https://github.com/tree-sitter/tree-sitter-bash")
        (json       "https://github.com/tree-sitter/tree-sitter-json")
        (rust       "https://github.com/tree-sitter/tree-sitter-rust")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (jsdoc      "https://github.com/tree-sitter/tree-sitter-jsdoc")
        (css        "https://github.com/tree-sitter/tree-sitter-css")
        (html       "https://github.com/tree-sitter/tree-sitter-html")
        (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
        (toml       "https://github.com/tree-sitter/tree-sitter-toml")
        (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser"
                    "tree-sitter-markdown/src")
        (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser"
                         "tree-sitter-markdown-inline/src")
        ;; Jinja regions inside Ansible YAML (used by lisp/ansible-intelligence).
        (jinja      "https://github.com/cathaysia/tree-sitter-jinja"
                    "v0.13.0" "tree-sitter-jinja/src")
        (sql        "https://github.com/DerekStride/tree-sitter-sql"
                    "gh-pages")))

(setq treesit-font-lock-level 4)

(defun ide-treesit-install-grammars ()
  "Install any missing tree-sitter grammars into the sandbox."
  (interactive)
  (make-directory (car treesit-extra-load-path) t)
  (dolist (entry treesit-language-source-alist)
    (let ((lang (car entry)))
      (unless (treesit-language-available-p lang)
        (condition-case err
            (let ((treesit--install-language-grammar-out-dir-history nil))
              (treesit-install-language-grammar lang (car treesit-extra-load-path)))
          (error (message "Grammar %s failed: %s" lang (error-message-string err))))))))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (sh-mode . bash-ts-mode)
        (bash-mode . bash-ts-mode)
        (json-mode . json-ts-mode)
        (js-json-mode . json-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (dockerfile-mode . dockerfile-ts-mode)
        (go-mode . go-ts-mode)
        (js-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (css-mode . css-ts-mode)
        (html-mode . html-ts-mode)
        (mhtml-mode . html-ts-mode)
        (lua-mode . lua-ts-mode)
        (toml-mode . toml-ts-mode)
        (conf-toml-mode . toml-ts-mode)))

(add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
(add-to-list 'auto-mode-alist '("Dockerfile\\(\\..*\\)?\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))

(use-package expand-region :commands er/expand-region)

(provide '05-treesitter)
;;; 05-treesitter.el ends here
