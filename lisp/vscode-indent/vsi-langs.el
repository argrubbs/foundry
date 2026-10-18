;;; vsi-langs.el --- Indentation rules of the languages Foundry configures -*- lexical-binding: t; -*-

;;; Commentary:
;; Each rule set is VS Code's `language-configuration.json' for that language
;; (extensions/<lang>/language-configuration.json), with the JavaScript regexps rewritten as
;; Emacs regexps.  Where a pattern uses lookahead or a back-reference, which Emacs regexps lack,
;; the rule is a predicate instead.  The VS Code pattern is quoted in each comment.
;;
;; YAML runs at `advanced', as VS Code's own yaml extension sets `editor.autoIndent' there.
;; Its increase/decrease patterns are kept for completeness but are not consulted: Enter in
;; YAML keeps the current indentation and only indents after an open bracket.

;;; Code:

(require 'vsi-core)

;;;; Shared rules --------------------------------------------------------------------------

(defconst vsi--slash-comment-rule
  (list :before "^[ \t]*//[ \t]*[^ \t]\\|[ \t]//[ \t]+[^ \t]"
        :after "[^ \t]"
        :indent 'none :append "// ")
  "Enter inside a `//' comment with text after the cursor continues the comment.")

;;;; JavaScript and TypeScript -------------------------------------------------------------

(defun vsi--js-doc-start-p (s)
  "^\\s*/\\*\\*(?!/)([^\\*]|\\*(?!/))*$ -- a `/**' line with no `*/'."
  (and (string-match "\\`[ \t]*/\\*\\*" s)
       (let ((rest (substring s (match-end 0))))
         (and (not (string-prefix-p "/" rest)) (not (string-match-p "\\*/" rest))))))

(defun vsi--js-doc-line-p (s)
  "^(\\t|[ ])*\\*([ ]([^\\*]|\\*(?!/))*)?$ -- a ` * text' line of a doc comment."
  (and (string-match "\\`[ \t]*\\*" s)
       (let ((rest (substring s (match-end 0))))
         (or (string= rest "")
             (and (string-prefix-p " " rest) (not (string-match-p "\\*/" rest)))))))

(defun vsi--js-doc-prev-p (s)
  "A previous line that starts or continues a doc comment and does not close it."
  (and (string-match-p "\\`[ \t]*\\(?:/\\*\\*\\|\\*\\)" s)
       (not (string-match-p "\\`[ \t]*\\*/" s))))

(defconst vsi--js-doc-close-re "^[ \t]* \\*/[ \t]*$" " */")
(defconst vsi--js-doc-close-text-re "^[ \t]* \\*[^/]*\\*/[ \t]*$" " *text*/")

(defun vsi--js-unindented-p (s)
  "A line inside a block comment that keeps its indentation."
  (or (string-match-p vsi--js-doc-close-text-re s)
      (string-match-p vsi--js-doc-close-re s)
      (vsi--js-doc-line-p s)))

(defun vsi--js-case-after-p (s)
  "The text after the cursor is not another label.
VS Code: ^(?!\\s*(\\bcase\\b|\\bdefault\\b))"
  (not (string-match-p "\\`[ \t]*\\(?:\\bcase\\b\\|\\bdefault\\b\\)" s)))

(defun vsi--js-outdent-before-p (s)
  "^\\s+([^{i\\s]|i(?!f\\b)) -- an indented line that is not a brace or an `if'."
  (and (string-match "\\`[ \t]+\\(.\\)" s)
       (let ((c (aref (match-string 1 s) 0)) (at (match-beginning 1)))
         (or (not (memq c '(?{ ?i ?\s ?\t)))
             (and (eq c ?i) (not (string-match-p "\\`if\\b" (substring s at))))))))

(defconst vsi--js-on-enter
  (list
   (list :before #'vsi--js-doc-start-p :after "^[ \t]*\\*/$" :indent 'indent-outdent :append " * ")
   (list :before #'vsi--js-doc-start-p :indent 'none :append " * ")
   (list :before #'vsi--js-doc-line-p :prev #'vsi--js-doc-prev-p :indent 'none :append "* ")
   (list :before vsi--js-doc-close-re :indent 'none :remove 1)
   (list :before vsi--js-doc-close-text-re :indent 'none :remove 1)
   (list :before "^[ \t]*\\(?:\\bcase[ \t].+:\\|\\bdefault:\\)$" :after #'vsi--js-case-after-p :indent 'indent)
   (list :prev "^[ \t]*\\(?:\\(?:\\(?:else ?\\)?if\\|for\\|while\\)[ \t]*(.*)[ \t]*\\|else[ \t]*\\)$"
         :before #'vsi--js-outdent-before-p :indent 'outdent)
   (list :before "^.*([^)]*$" :after "^[ \t]*).*$" :indent 'indent-outdent :append "\t")
   (list :before "^.*{[^}]*$" :after "^[ \t]*}.*$" :indent 'indent-outdent :append "\t")
   (list :before "^.*\\[[^]]*$" :after "^[ \t]*].*$" :indent 'indent-outdent :append "\t")
   vsi--slash-comment-rule))

(vsi-define-language 'javascript
  :offset-vars '(js-indent-level typescript-ts-mode-indent-offset)
  :modes '(js-ts-mode js-mode js2-mode javascript-mode typescript-ts-mode typescript-mode tsx-ts-mode)
  :brackets '(("${" . "}") ("{" . "}") ("[" . "]") ("(" . ")"))
  ;; ^\s*[\}\]\)].*$
  :decrease "^[ \t]*[]})]"
  ;; ^.*(\{[^}]*|\([^)]*|\[[^\]]*)$
  :increase "^.*\\(?:{[^}]*\\|([^)]*\\|\\[[^]]*\\)$"
  :unindented #'vsi--js-unindented-p
  ;; ^((.*=>\s*)|((.*[^\w]+|\s*)((if|while|for)\s*\(.*\)\s*|else\s*)))$
  :indent-next "^\\(?:.*=>[ \t]*\\|\\(?:.*[^A-Za-z0-9_]+\\|[ \t]*\\)\\(?:\\(?:if\\|while\\|for\\)[ \t]*(.*)[ \t]*\\|else[ \t]*\\)\\)$"
  :on-enter vsi--js-on-enter)

;;;; JSON ----------------------------------------------------------------------------------

(defun vsi--json-open-p (s open close)
  "Whether S has an OPEN bracket that the line does not close.
VS Code's pattern is a lookahead over balanced string literals; this walks
them.  CLOSE is the matching closing bracket."
  (let ((i 0) found)
    (while (and (not found) (setq i (string-search open s i)))
      (let ((j (1+ i)) (ok t) (done nil))
        (while (and ok (not done))
          (let ((q (string-search "\"" s j)))
            (if (null q)
                (setq done t ok (not (string-search close (substring s j))))
              (let ((k (1+ q)))
                (while (and (< k (length s)) (not (eq (aref s k) ?\")))
                  (setq k (+ k (if (eq (aref s k) ?\\) 2 1))))
                (if (>= k (length s)) (setq ok nil) (setq j (1+ k)))))))
        (when ok (setq found t)))
      (setq i (1+ i)))
    found))

(defun vsi--json-increase-p (s)
  (or (vsi--json-open-p s "{" "}") (vsi--json-open-p s "[" "]")))

(vsi-define-language 'json
  :offset-vars '(json-ts-mode-indent-offset js-indent-level)
  :modes '(json-ts-mode json-mode jsonc-mode)
  :increase #'vsi--json-increase-p
  ;; ^\s*[}\]],?\s*$
  :decrease "^[ \t]*[]}],?[ \t]*$"
  :on-enter (list vsi--slash-comment-rule))

;;;; YAML and Dockerfile -------------------------------------------------------------------

(defconst vsi--yaml-increase
  ;; ^\s*.*(:|-) ?(&\w+)?(\{[^}"']*|\([^)"']*)?$   (VS Code's file has `&amp;' for `&')
  "^[ \t]*.*\\(?::\\|-\\) ?\\(?:&[A-Za-z0-9_]+\\)?\\(?:{[^}\"']*\\|([^)\"']*\\)?$")

(vsi-define-language 'yaml
  :offset-vars '(yaml-indent-offset)
  :modes '(yaml-ts-mode yaml-mode)
  :auto-indent 'advanced
  :increase vsi--yaml-increase
  :decrease "^[ \t]+}$")

(vsi-define-language 'dockerfile
  :offset-vars '(dockerfile-ts-mode-indent-offset)
  :modes '(dockerfile-ts-mode dockerfile-mode)
  :increase vsi--yaml-increase
  :decrease "^[ \t]+}$")

;;;; Python --------------------------------------------------------------------------------

(vsi-define-language 'python
  :offset-vars '(python-indent-offset)
  :modes '(python-ts-mode python-mode)
  :on-enter (list (list :before "^[ \t]*\\(?:def\\|class\\|for\\|if\\|elif\\|else\\|while\\|try\\|with\\|finally\\|except\\|async\\).*?:[ \t]*$"
                        :indent 'indent)))

;;;; Go and Rust ---------------------------------------------------------------------------

(vsi-define-language 'go
  :offset-vars '(go-ts-mode-indent-offset)
  :modes '(go-ts-mode go-mode go-mod-ts-mode)
  ;; ^.*(\bcase\b.*:|\bdefault\b:|(\b(func|if|else|switch|select|for|struct)\b.*)?{[^}"'`]*|\([^)"'`]*)$
  :increase "^.*\\(?:\\bcase\\b.*:\\|\\bdefault\\b:\\|\\(?:\\b\\(?:func\\|if\\|else\\|switch\\|select\\|for\\|struct\\)\\b.*\\)?{[^}\"'`]*\\|([^)\"'`]*\\)$"
  ;; ^\s*(\bcase\b.*:|\bdefault\b:|}[)}]*[),]?|\)[,]?)$
  :decrease "^[ \t]*\\(?:\\bcase\\b.*:\\|\\bdefault\\b:\\|}[)}]*[),]?\\|)[,]?\\)$"
  :on-enter (list vsi--slash-comment-rule))

(vsi-define-language 'rust
  :offset-vars '(rust-ts-mode-indent-offset rust-indent-offset)
  :modes '(rust-ts-mode rust-mode)
  ;; ^.*\{[^}"']*$|^.*\([^\)"']*$
  :increase "^.*{[^}\"']*$\\|^.*([^)\"']*$"
  ;; ^\s*(\s*\/[*].*[*]\/\s*)*[})]
  :decrease "^[ \t]*\\(?:[ \t]*/[*].*[*]/[ \t]*\\)*[})]"
  :on-enter (list vsi--slash-comment-rule))

;;;; Lua -----------------------------------------------------------------------------------

(defun vsi--lua-increase-p (s)
  "A line that opens a block: a keyword or `{' with no `--' before it.
VS Code uses a tempered pattern: the keyword must not be followed by `end'
or `until'."
  (let ((case-fold-search nil) (start 0) found)
    (while (and (not found)
                (string-match "\\b\\(?:else\\|function\\|then\\|do\\|repeat\\)\\b" s start))
      (let ((mb (match-beginning 0)) (me (match-end 0)))
        (when (and (not (string-search "--" (substring s 0 mb)))
                   (not (string-match-p "\\b\\(?:end\\|until\\)\\b" (substring s me))))
          (setq found t))
        (setq start (1+ mb))))
    (or found
        (and (string-match "{[ \t]*$" s)
             (not (string-search "--" (substring s 0 (match-beginning 0))))))))

(vsi-define-language 'lua
  :offset-vars '(lua-ts-mode-indent-offset lua-indent-level)
  :modes '(lua-ts-mode lua-mode)
  :increase #'vsi--lua-increase-p
  ;; ^\s*((\b(elseif|else|end|until)\b)|(\})|(\)))
  :decrease "^[ \t]*\\(?:\\b\\(?:elseif\\|else\\|end\\|until\\)\\b\\|}\\|)\\)")

;;;; CSS -----------------------------------------------------------------------------------

(vsi-define-language 'css
  :offset-vars '(css-indent-offset)
  :modes '(css-ts-mode css-mode scss-mode less-css-mode)
  :increase "^.*{[^}]*$"
  :decrease "^[ \t]*}")

;;;; HTML ----------------------------------------------------------------------------------

(defconst vsi--html-void
  "area\\|base\\|br\\|col\\|embed\\|hr\\|img\\|input\\|keygen\\|link\\|menuitem\\|meta\\|param\\|source\\|track\\|wbr")

(defun vsi--html-increase-p (s)
  "A line that opens a tag, a comment or a brace and does not close it."
  (let ((case-fold-search nil) (start 0) found)
    ;; <(?!\?|(?:area|...|html|...)\b|[^>]*\/>)([-_\.A-Za-z0-9]+)(?=\s|>)\b[^>]*>(?!.*<\/\1>)
    (while (and (not found) (string-match "<\\([-_.A-Za-z0-9]+\\)[ \t>]" s start))
      (let* ((mb (match-beginning 0))
             (name (match-string 1 s))
             (tail (substring s (1+ mb))))
        (unless (or (string-match-p (concat "\\`\\(?:" vsi--html-void "\\|frame\\|html\\)\\b") tail)
                    (string-match-p "\\`[^>]*/>" tail))
          (let ((gt (string-search ">" s (1+ mb))))
            (when (and gt (not (string-match-p (concat "</" (regexp-quote name) ">")
                                               (substring s (1+ gt)))))
              (setq found t))))
        (setq start (1+ mb))))
    (or found
        ;; <!--(?!.*-->)
        (let ((i 0) open)
          (while (and (not open) (setq i (string-search "<!--" s i)))
            (unless (string-search "-->" (substring s (+ i 4))) (setq open t))
            (setq i (1+ i)))
          open)
        ;; \{[^}"']*$
        (string-match-p "{[^}\"']*$" s))))

(defun vsi--html-decrease-p (s)
  "^\\s*(<\\/(?!html)[-_\\.A-Za-z0-9]+\\b[^>]*>|-->|\\})"
  (or (and (string-match "\\`[ \t]*</\\([-_.A-Za-z0-9]+\\)\\b[^>]*>" s)
           (not (string-prefix-p "html" (match-string 1 s))))
      (string-match-p "\\`[ \t]*\\(?:-->\\|}\\)" s)))

(defun vsi--html-open-tag-p (s)
  "Text ending in an open tag (not a void element) followed by plain text."
  (let ((case-fold-search t) (start 0) found)
    (while (and (not found)
                (string-match "<\\([_:[:alnum:]][_:[:alnum:].-]*\\)\\(?:[^'\"/>]\\|\"[^\"]*\"\\|'[^']*'\\)*>[^<]*$"
                              s start))
      (let ((mb (match-beginning 0)))
        (unless (string-match-p (concat "\\`\\(?:" vsi--html-void "\\)") (substring s (1+ mb)))
          (setq found t))
        (setq start (1+ mb))))
    found))

(defun vsi--html-close-tag-p (s)
  (let ((case-fold-search t))
    (string-match-p "\\`</[_:[:alnum:]][_:[:alnum:].-]*[ \t]*>" s)))

(vsi-define-language 'html
  :offset-vars '(html-ts-indent-offset sgml-basic-offset)
  :modes '(html-ts-mode html-mode mhtml-mode)
  :brackets '(("<!--" . "-->") ("{" . "}") ("(" . ")"))
  :increase #'vsi--html-increase-p
  :decrease #'vsi--html-decrease-p
  :on-enter (list (list :before #'vsi--html-open-tag-p :after #'vsi--html-close-tag-p :indent 'indent-outdent)
                  (list :before #'vsi--html-open-tag-p :indent 'indent)))

;;;; Languages with no indentation rules ---------------------------------------------------
;; VS Code gives these only bracket handling and "keep the previous indentation".

(vsi-define-language 'shell :modes '(sh-mode bash-ts-mode) :offset-vars '(sh-basic-offset))
(vsi-define-language 'powershell :modes '(powershell-mode))
(vsi-define-language 'markdown :modes '(markdown-mode gfm-mode))
(vsi-define-language 'toml :modes '(toml-ts-mode conf-toml-mode))
(vsi-define-language 'terraform :modes '(terraform-mode hcl-mode))

(provide 'vsi-langs)
;;; vsi-langs.el ends here
