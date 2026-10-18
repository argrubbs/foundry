;;; vsi-core.el --- Indentation engine ported from VS Code -*- lexical-binding: t; -*-

;; The algorithms here follow the VS Code editor (MIT licensed, Copyright (c) Microsoft
;; Corporation): src/vs/editor/common/languages/{autoIndent,enterAction}.ts and
;; supports/{onEnter,indentRules,indentationLineProcessor}.ts, and
;; contrib/indentation/common/indentation.ts.  Names and structure are kept close to the
;; originals so the two can be compared side by side.

;;; Commentary:
;; A language is a set of rules (`vsi-define-language'):
;;
;; - `:increase', `:decrease', `:indent-next', `:unindented' -- VS Code's indentationRules;
;; - `:on-enter' -- a list of plists (:before :after :prev :indent :append :remove);
;; - `:brackets' -- an alist of (OPEN . CLOSE) strings;
;; - `:auto-indent' -- one of none, keep, brackets, advanced, full.
;;
;; A rule is a regexp string or a predicate taking the line text.  Rules are tested against
;; the *processed* line: the text with every bracket inside a string, comment or regex
;; removed, so a `{' in a string never indents.
;;
;; Indentations are passed around as column counts; `vsi--make-indent' turns one into text.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup vscode-indent nil
  "Auto-indentation that behaves like VS Code."
  :group 'editing
  :prefix "vsi-")

(defcustom vsi-auto-indent 'full
  "How much the editor indents for you, like VS Code's `editor.autoIndent'.
`none'      never indent;
`keep'      keep the current line's indentation;
`brackets'  also honour brackets;
`advanced'  also run the language's on-enter rules;
`full'      also run its indentation rules and re-indent as you type.
A language can set its own default (YAML uses `advanced'); see
`vsi-auto-indent-alist'."
  :type '(choice (const none) (const keep) (const brackets) (const advanced) (const full)))

(defcustom vsi-auto-indent-alist nil
  "Per-language override of `vsi-auto-indent': an alist of (LANGUAGE . LEVEL)."
  :type '(alist :key-type symbol :value-type symbol))

(defcustom vsi-indent-size nil
  "Columns per indentation level, or nil to use `tab-width'."
  :type '(choice (const :tag "Use tab-width" nil) integer))
(make-variable-buffer-local 'vsi-indent-size)

(defvar-local vsi-language nil
  "Name of the rule set to use in this buffer.
Nil means pick one from the major mode.")

;;;; Language registry ---------------------------------------------------------------------

(cl-defstruct (vsi-lang (:constructor vsi--make-lang))
  name auto-indent offset-vars increase decrease indent-next unindented on-enter
  brackets)

(defconst vsi--default-brackets '(("(" . ")") ("{" . "}") ("[" . "]"))
  "Brackets VS Code assumes when a language declares none.")

(defvar vsi--languages nil "Alist of (NAME . `vsi-lang').")
(defvar vsi--mode-languages nil "Alist of (MAJOR-MODE . NAME).")
(defvar vsi--fallback-lang (vsi--make-lang :name 'default :brackets vsi--default-brackets))

(defun vsi-define-language (name &rest props)
  "Define the rule set NAME.
PROPS: :modes :auto-indent :offset-vars :increase :decrease :indent-next
:unindented :on-enter :brackets (see the Commentary).  :offset-vars lists the
variables in which the language's modes keep their indentation width."
  (let ((lang (vsi--make-lang
               :name name
               :auto-indent (plist-get props :auto-indent)
               :offset-vars (plist-get props :offset-vars)
               :increase (plist-get props :increase)
               :decrease (plist-get props :decrease)
               :indent-next (plist-get props :indent-next)
               :unindented (plist-get props :unindented)
               :on-enter (plist-get props :on-enter)
               :brackets (or (plist-get props :brackets) vsi--default-brackets))))
    (setf (alist-get name vsi--languages) lang)
    (dolist (mode (plist-get props :modes))
      (setf (alist-get mode vsi--mode-languages) name))
    lang))

(defun vsi--known-mode-p ()
  "Non-nil when the major mode (or one it derives from) has a rule set."
  (cl-some (lambda (entry) (derived-mode-p (car entry))) vsi--mode-languages))

(defun vsi--lang ()
  "The rule set for the current buffer."
  (or (and vsi-language (alist-get vsi-language vsi--languages))
      (cl-loop for (mode . name) in vsi--mode-languages
               when (derived-mode-p mode) return (alist-get name vsi--languages))
      vsi--fallback-lang))

(defun vsi--level ()
  "The effective auto-indent level as a number: 0 none ... 4 full."
  (let* ((lang (vsi--lang))
         (level (or (alist-get (vsi-lang-name lang) vsi-auto-indent-alist)
                    (vsi-lang-auto-indent lang)
                    vsi-auto-indent)))
    (pcase level ('none 0) ('keep 1) ('brackets 2) ('advanced 3) (_ 4))))

(defun vsi--has-indent-rules-p ()
  "Non-nil when the language has any indentation rule.
This is VS Code's `indentRulesSupport'."
  (let ((l (vsi--lang)))
    (or (vsi-lang-increase l) (vsi-lang-decrease l)
        (vsi-lang-indent-next l) (vsi-lang-unindented l))))

;;;; Indentation arithmetic ----------------------------------------------------------------

(defun vsi--size ()
  "Columns in one indentation level.
That is `vsi-indent-size', else the language's own offset variable, else
`tab-width'."
  (max 1 (or vsi-indent-size
             (cl-loop for var in (vsi-lang-offset-vars (vsi--lang))
                      for val = (and (boundp var) (symbol-value var))
                      when (and (integerp val) (> val 0)) return val)
             tab-width)))

(defun vsi--lead (text)
  "The leading whitespace of TEXT."
  (if (string-match "\\`[ \t]*" text) (match-string 0 text) ""))

(defun vsi--cols (ws)
  "The width in columns of the whitespace string WS."
  (let ((col 0))
    (dotimes (i (length ws))
      (setq col (if (eq (aref ws i) ?\t) (* (1+ (/ col tab-width)) tab-width) (1+ col))))
    col))

(defun vsi--make-indent (cols)
  "Whitespace COLS columns wide, using tabs only when `indent-tabs-mode' is on."
  (if indent-tabs-mode
      (concat (make-string (/ cols tab-width) ?\t) (make-string (% cols tab-width) ?\s))
    (make-string cols ?\s)))

(defun vsi--normalize (text)
  "TEXT with its leading whitespace rewritten by `vsi--make-indent'."
  (let ((ws (vsi--lead text)))
    (concat (vsi--make-indent (vsi--cols ws)) (substring text (length ws)))))

(defun vsi--shift (cols)
  "COLS moved to the next indentation stop."
  (let ((size (vsi--size))) (* (1+ (/ cols size)) size)))

(defun vsi--unshift (cols)
  "COLS moved to the previous indentation stop."
  (let ((size (vsi--size))) (if (<= cols 0) 0 (* (/ (1- cols) size) size))))

(defun vsi--expand-append (text)
  "TEXT with each tab made into spaces.
A tab in an on-enter `:append' stands for one indentation level."
  (replace-regexp-in-string "\t" (make-string (vsi--size) ?\s) (or text "")))

;;;; The line model ------------------------------------------------------------------------
;; Lines are addressed by number, as in VS Code.  `vsi--with-model' sets up caches and
;; relative line movement; `vsi--overrides' lets a caller substitute the text of a line.

(defvar vsi--cur nil
  "Cons (LINE . POS) of the last line visited, to move relative to it.")
(defvar vsi--pcache nil "Hash of processed line text.")
(defvar vsi--rcache nil "Hash of raw line text.")
(defvar vsi--overrides nil
  "Alist of (LINE . TEXT) replacing a line's raw and processed text.")

(defmacro vsi--with-model (&rest body)
  "Run BODY with the whole buffer visible and fresh line caches."
  (declare (indent 0) (debug t))
  `(let ((vsi--pcache (make-hash-table))
         (vsi--rcache (make-hash-table))
         (vsi--cur nil))
     (save-excursion
       (save-restriction
         (widen)
         (setq vsi--cur (cons 1 (point-min)))
         ,@body))))

(defun vsi--bol (n)
  "Position of the start of line N."
  (save-excursion
    (goto-char (cdr vsi--cur))
    (forward-line (- n (car vsi--cur)))
    (setq vsi--cur (cons n (point)))
    (point)))

(defun vsi--raw (n)
  "The text of line N."
  (or (cdr (assq n vsi--overrides))
      (gethash n vsi--rcache)
      (puthash n (let ((b (vsi--bol n)))
                   (buffer-substring-no-properties b (save-excursion (goto-char b) (line-end-position))))
               vsi--rcache)))

(defun vsi--line-indent (n)
  "Indentation of line N in columns."
  (vsi--cols (vsi--lead (vsi--raw n))))

(declare-function treesit-node-at "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-parser-list "treesit")

(defconst vsi--string-node-re "string\\|comment\\|regex\\|scalar\\|fragment"
  "Tree-sitter node types whose text is not code.")

(defun vsi--ts-node-matches-p (pos regexp)
  "Whether the tree-sitter node at POS has a type matching REGEXP.
Some grammars (YAML's quoted strings) have no leaf for a string's contents;
`treesit-node-at' then returns a neighbouring token, and the parent is the string."
  (let ((node (treesit-node-at pos)))
    (when (and node (or (> (treesit-node-start node) pos)
                        (<= (treesit-node-end node) pos)))
      (setq node (treesit-node-parent node)))
    (and node (string-match-p regexp (treesit-node-type node)))))

(defun vsi--in-string-or-comment-p (pos)
  "Non-nil when the character at POS is in a string, comment or regex."
  (if (and (fboundp 'treesit-parser-list) (treesit-parser-list))
      (vsi--ts-node-matches-p pos vsi--string-node-re)
    (save-excursion (nth 8 (syntax-ppss pos)))))

(defun vsi--bracket-re ()
  "A regexp matching any bracket of the current language."
  (regexp-opt (cl-loop for (open . close) in (vsi-lang-brackets (vsi--lang))
                       collect open collect close)))

(defun vsi--process-range (beg end)
  "The text from BEG to END without brackets in strings, comments or regexes."
  (let ((re (vsi--bracket-re)) (pos beg) pieces)
    (save-excursion
      (goto-char beg)
      (while (re-search-forward re end t)
        (let ((mb (match-beginning 0)) (me (match-end 0)))
          (when (vsi--in-string-or-comment-p mb)
            (push (buffer-substring-no-properties pos mb) pieces)
            (setq pos me)))))
    (push (buffer-substring-no-properties pos end) pieces)
    (apply #'concat (nreverse pieces))))

(defun vsi--processed (n)
  "The text of line N with brackets in strings and comments removed."
  (or (cdr (assq n vsi--overrides))
      (gethash n vsi--pcache)
      (puthash n (let ((b (vsi--bol n)))
                   (vsi--process-range b (save-excursion (goto-char b) (line-end-position))))
               vsi--pcache)))

;;;; Rules ---------------------------------------------------------------------------------

(defun vsi--test (rule text)
  "Non-nil when RULE (a regexp, a predicate, or nil) accepts TEXT."
  (cond ((null rule) nil)
        ((functionp rule) (funcall rule text))
        (t (let ((case-fold-search nil)) (string-match-p rule text)))))

(defun vsi--rule (kind)
  "The rule of KIND (`increase', `decrease', `indent-next' or `unindented')."
  (let ((l (vsi--lang)))
    (pcase kind
      ('increase (vsi-lang-increase l))
      ('decrease (vsi-lang-decrease l))
      ('indent-next (vsi-lang-indent-next l))
      ('unindented (vsi-lang-unindented l)))))

(defun vsi--should (kind n &optional cols)
  "Whether line N matches the rule KIND.
With COLS, test it as if it were indented that many columns."
  (let ((text (vsi--processed n)))
    (when cols
      (setq text (concat (make-string cols ?\s) (substring text (length (vsi--lead text))))))
    (vsi--test (vsi--rule kind) text)))

(defun vsi--meta (text)
  "The rules the raw TEXT matches, a list of `increase', `decrease', `indent-next'."
  (cl-loop for kind in '(increase decrease indent-next)
           when (vsi--test (vsi--rule kind) text) collect kind))

;;;; On-enter rules ------------------------------------------------------------------------

(defun vsi--bracket-open-re (open)
  (concat (if (string-match-p "\\`[A-Za-z0-9_]" open) "\\b" "") (regexp-quote open) "[ \t]*$"))

(defun vsi--bracket-close-re (close)
  (concat "^[ \t]*" (regexp-quote close) (if (string-match-p "[A-Za-z0-9_]\\'" close) "\\b" "")))

(defun vsi--on-enter (prev before after)
  "The enter action for PREV (the previous line), BEFORE and AFTER the cursor.
A plist (:indent none|indent|indent-outdent|outdent :append STRING :remove N),
or nil."
  (let ((level (vsi--level)) (lang (vsi--lang)))
    (or
     (and (>= level 3)
          (cl-loop for rule in (vsi-lang-on-enter lang)
                   when (and (or (null (plist-get rule :before)) (vsi--test (plist-get rule :before) before))
                             (or (null (plist-get rule :after)) (vsi--test (plist-get rule :after) after))
                             (or (null (plist-get rule :prev)) (vsi--test (plist-get rule :prev) prev)))
                   return rule))
     (and (>= level 2)
          (or (and (> (length before) 0) (> (length after) 0)
                   (cl-loop for (open . close) in (vsi-lang-brackets lang)
                            when (and (let ((case-fold-search nil))
                                        (string-match-p (vsi--bracket-open-re open) before))
                                      (string-match-p (vsi--bracket-close-re close) after))
                            return (list :indent 'indent-outdent)))
              (and (> (length before) 0)
                   (cl-loop for (open . _close) in (vsi-lang-brackets lang)
                            when (string-match-p (vsi--bracket-open-re open) before)
                            return (list :indent 'indent))))))))

(defun vsi--enter-action (pos)
  "VS Code's `getEnterAction' for point POS.
The on-enter result plus the line's indentation, as a plist
(:indent :append :remove :indentation), or nil."
  (vsi--with-model
    (goto-char pos)
    (let* ((n (line-number-at-pos nil t))
           (bol (line-beginning-position))
           (prev (if (> n 1) (vsi--processed (1- n)) ""))
           (before (vsi--process-range bol pos))
           (after (vsi--process-range pos (line-end-position)))
           (res (vsi--on-enter prev before after)))
      (when res
        (let* ((action (plist-get res :indent))
               (append (plist-get res :append))
               (remove (or (plist-get res :remove) 0))
               (indentation (min (vsi--cols (vsi--lead (buffer-substring-no-properties bol pos)))
                                 (vsi--cols (vsi--lead (vsi--raw n))))))
          (cond ((null append)
                 (setq append (if (memq action '(indent indent-outdent)) "\t" "")))
                ((eq action 'indent) (setq append (concat "\t" append))))
          (list :indent action :append append :remove remove
                :indentation (max 0 (- indentation remove))))))))

;;;; Inherited indentation -----------------------------------------------------------------

(defun vsi--preceding-valid-line (n)
  "Nearest line above N that is not blank and not an unindented line, or -1."
  (cl-loop for p from (1- n) downto 1
           for text = (vsi--raw p)
           unless (or (vsi--should 'unindented p) (string-match-p "\\`[ \t]*\\'" text))
           return p
           finally return -1))

(defun vsi--inherit-indent (n &optional no-honor)
  "VS Code's `getInheritIndentForLine' for line N.
A plist (:indent COLS :action `indent'|nil :line LINE), or nil when the
indentation is not to be touched.  Non-nil NO-HONOR ignores deliberately odd
indentation above."
  (when (and (>= (vsi--level) 4) (vsi--has-indent-rules-p))
    (catch 'ret
      (when (<= n 1) (throw 'ret (list :indent 0 :action nil)))
      (cl-loop for p from (1- n) downto 1
               do (unless (string= (vsi--raw p) "") (cl-return))
               (when (= p 1) (throw 'ret (list :indent 0 :action nil))))
      (let ((pre (vsi--preceding-valid-line n)))
        (cond
         ((< pre 0) nil)
         ((< pre 1) (list :indent 0 :action nil))
         ((or (vsi--should 'increase pre) (vsi--should 'indent-next pre))
          (list :indent (vsi--line-indent pre) :action 'indent :line pre))
         ((vsi--should 'decrease pre)
          (list :indent (vsi--line-indent pre) :action nil :line pre))
         ((= pre 1)
          (list :indent (vsi--line-indent 1) :action nil :line 1))
         (t
          (let* ((prev-line (1- pre))
                 (meta (vsi--meta (vsi--raw prev-line))))
            (cond
             ((and (not (or (memq 'increase meta) (memq 'decrease meta)))
                   (memq 'indent-next meta))
              (let ((stop 0))
                (cl-loop for i from (1- prev-line) downto 1
                         do (unless (vsi--should 'indent-next i) (setq stop i) (cl-return)))
                (list :indent (vsi--line-indent (1+ stop)) :action nil :line (1+ stop))))
             ((not no-honor)
              (list :indent (vsi--line-indent pre) :action nil :line pre))
             (t
              (catch 'found
                (cl-loop for i from pre downto 1
                         do (cond
                             ((vsi--should 'increase i)
                              (throw 'found (list :indent (vsi--line-indent i) :action 'indent :line i)))
                             ((vsi--should 'indent-next i)
                              ;; VS Code re-tests line I instead of J here, so STOP stays 0.
                              (throw 'found (list :indent (vsi--line-indent 1) :action nil :line 1)))
                             ((vsi--should 'decrease i)
                              (throw 'found (list :indent (vsi--line-indent i) :action nil :line i)))))
                (list :indent (vsi--line-indent 1) :action nil :line 1)))))))))))

(defun vsi--good-indent (n)
  "VS Code's `getGoodIndentForLine': the columns line N should have, or nil."
  (when (and (>= (vsi--level) 4) (vsi--has-indent-rules-p))
    (let ((indent (vsi--inherit-indent n)))
      (when indent
        (let* ((line (plist-get indent :line))
               (cols (plist-get indent :indent))
               (apply-enter (and line
                                 (cl-loop for b from line below (1- n)
                                          always (string-match-p "\\`[ \t]*\\'" (vsi--raw b)))))
               (enter (and apply-enter (vsi--on-enter "" (vsi--raw line) ""))))
          (if enter
              (let ((ind (vsi--line-indent line))
                    (action (plist-get enter :indent)))
                (when (plist-get enter :remove)
                  (setq ind (max 0 (- ind (plist-get enter :remove)))))
                (cond ((memq action '(indent indent-outdent)) (setq ind (vsi--shift ind)))
                      ((eq action 'outdent) (setq ind (vsi--unshift ind))))
                (when (vsi--should 'decrease n) (setq ind (vsi--unshift ind)))
                (vsi--cols (vsi--lead (concat (make-string ind ?\s)
                                              (vsi--expand-append (plist-get enter :append))))))
            (if (vsi--should 'decrease n)
                (if (eq (plist-get indent :action) 'indent) cols (vsi--unshift cols))
              (if (eq (plist-get indent :action) 'indent) (vsi--shift cols) cols))))))))

(defun vsi--enter-indent (pos)
  "VS Code's `getIndentForEnter' at POS.
Returns (BEFORE . AFTER), the columns of the two lines, or nil."
  (when (and (>= (vsi--level) 4) (vsi--has-indent-rules-p))
    (vsi--with-model
      (goto-char pos)
      (let* ((n (line-number-at-pos nil t))
             (before (vsi--process-range (line-beginning-position) pos))
             (after (vsi--process-range pos (line-end-position)))
             (before-indent (vsi--cols (vsi--lead before)))
             (vsi--overrides (list (cons n before)))
             (act (vsi--inherit-indent (1+ n))))
        (if (null act)
            (cons before-indent before-indent)
          (let ((a (plist-get act :indent)))
            (when (eq (plist-get act :action) 'indent) (setq a (vsi--shift a)))
            (when (vsi--test (vsi--rule 'decrease) after) (setq a (vsi--unshift a)))
            (cons before-indent a)))))))

;;;; Indent on type ------------------------------------------------------------------------

(defun vsi--type-indent (pos ch)
  "VS Code's `getIndentActionForType'.
Returns the columns the line should have after CH was typed at POS (CH is
already inserted), or nil when the indentation should stay."
  (when (and (>= (vsi--level) 4) (vsi--has-indent-rules-p))
    (vsi--with-model
      (goto-char pos)
      (let* ((n (line-number-at-pos nil t))
             (before (vsi--process-range (line-beginning-position) pos))
             (after (vsi--process-range (1+ pos) (line-end-position)))
             (around (concat before after))
             (with-ch (concat before (string ch) after))
             (decrease (vsi--rule 'decrease)))
        (cond
         ((and (not (vsi--test decrease around)) (vsi--test decrease with-ch))
          (let ((r (vsi--inherit-indent n t)))
            (when r
              (if (eq (plist-get r :action) 'indent)
                  (plist-get r :indent)
                (vsi--unshift (plist-get r :indent))))))
         ((and (> n 1)
               (vsi--test (vsi--rule 'indent-next) (vsi--raw (1- n)))
               (vsi--test (vsi--rule 'increase) with-ch))
          (let* ((r (vsi--inherit-indent n t))
                 (inherited (plist-get r :indent)))
            (when (and inherited
                       (= (vsi--shift inherited) (vsi--line-indent n))
                       (string-match-p "\\`[ \t]*\\'" around)
                       (cl-some (lambda (b) (eq (aref (car b) (1- (length (car b)))) ch))
                                (vsi-lang-brackets (vsi--lang))))
              inherited))))))))

;;;; Reindent ------------------------------------------------------------------------------

(defun vsi--starts-in-string-p (n)
  "Non-nil when line N begins inside a string."
  (let ((b (vsi--bol n)))
    (if (and (fboundp 'treesit-parser-list) (treesit-parser-list))
        (vsi--ts-node-matches-p b "string\\|scalar\\|fragment")
      (save-excursion (nth 3 (syntax-ppss b))))))

(defun vsi-reindent-edits (beg-line end-line)
  "VS Code's `getReindentEditOperations'.
Returns an alist of (LINE . COLUMNS) for BEG-LINE to END-LINE."
  (when (vsi--has-indent-rules-p)
    (vsi--with-model
      (let* ((last (min end-line (line-number-at-pos (point-max) t)))
             (start beg-line)
             edits)
        (while (and (<= start last) (vsi--should 'unindented start)) (cl-incf start))
        (unless (> start (1- last))
          (let* ((global (vsi--line-indent start))
                 (ideal global))
            (cond ((vsi--should 'increase start)
                   (setq ideal (vsi--shift ideal) global (vsi--shift global)))
                  ((vsi--should 'indent-next start)
                   (setq ideal (vsi--shift ideal))))
            (cl-loop
             for n from (1+ start) to last
             unless (vsi--starts-in-string-p n)
             do (let ((old (vsi--line-indent n))
                      (current ideal))
                  (when (vsi--should 'decrease n current)
                    (setq ideal (vsi--unshift ideal) global (vsi--unshift global)))
                  (unless (= old ideal) (push (cons n ideal) edits))
                  (cond ((vsi--should 'unindented n))
                        ((vsi--should 'increase n current)
                         (setq global (vsi--shift global) ideal global))
                        ((vsi--should 'indent-next n current)
                         (setq ideal (vsi--shift ideal)))
                        (t (setq ideal global)))))))
        (nreverse edits)))))

(provide 'vsi-core)
;;; vsi-core.el ends here
