;;; vscode-indent.el --- Indent like VS Code -*- lexical-binding: t; -*-

;; Author: Foundry
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, languages

;; The indentation behaviour is a port of the VS Code editor (MIT licensed, Copyright (c)
;; Microsoft Corporation); see vsi-core.el.

;;; Commentary:
;; `vscode-indent-mode' replaces Emacs' line-oriented indentation with VS Code's:
;;
;; - RET opens a line indented from the line above, from the language's rules and from
;;   brackets: `{|}' becomes three lines, `/** |' continues a doc comment, `if x:|' indents.
;; - TAB indents a blank line to where code belongs there and otherwise moves to the next
;;   indentation stop; with a selection it shifts every selected line.  S-TAB (backtab)
;;   shifts left.  TAB never re-indents the line you are on.
;; - Typing a character that makes the line match the language's outdent rule re-indents it
;;   (`}', `end', `else:' ...).  A closing bracket otherwise lines up with its opener.
;; - Indentation is guessed from the file when it is opened (`vsi-detect-indentation').
;; - `vsi-reindent-lines' re-indents a region or the buffer from the rules.
;; - Pasting can be re-indented too (`vsi-auto-indent-on-paste', off as in VS Code).
;;
;; How much happens depends on the language's level, see `vsi-auto-indent'.  The rule sets
;; are in vsi-langs.el.  Indentation is always inserted as spaces unless the buffer has
;; `indent-tabs-mode' on.
;;
;; Not ported: embedded languages (a `<script>' in HTML is indented by the HTML rules) and
;; multiple cursors.

;;; Code:

(require 'cl-lib)
(require 'vsi-core)
(require 'vsi-langs)
(require 'vsi-guess)

(defcustom vsi-detect-indentation t
  "When non-nil, guess a file's indentation size when `vscode-indent-mode' starts."
  :type 'boolean :group 'vscode-indent)

(defcustom vsi-auto-indent-on-paste nil
  "When non-nil, re-indent yanked text, like `editor.autoIndentOnPaste'."
  :type 'boolean :group 'vscode-indent)

;;;; Helpers -------------------------------------------------------------------------------

(defun vsi--blank-line-p ()
  "Non-nil when the current line has only whitespace."
  (save-excursion (beginning-of-line) (looking-at "[ \t]*$")))

(defun vsi--goto-line (n)
  "Move to the start of line N, counting from the start of the whole buffer."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun vsi--set-line-indent (cols)
  "Set the indentation of the current line to COLS columns, keeping point."
  (let* ((bol (line-beginning-position))
         (old-end (save-excursion (goto-char bol) (skip-chars-forward " \t") (point)))
         (from-end (- (line-end-position) (point))))
    (delete-region bol old-end)
    (goto-char bol)
    (insert (vsi--make-indent cols))
    (goto-char (- (line-end-position) from-end))))

(defun vsi--set-line-indent-at (line cols)
  "Set the indentation of LINE to COLS columns, if it differs."
  (save-excursion
    (vsi--goto-line line)
    (unless (= cols (vsi--cols (vsi--lead (buffer-substring-no-properties
                                           (line-beginning-position) (line-end-position)))))
      (vsi--set-line-indent cols))))

;;;; Enter ---------------------------------------------------------------------------------

(defvar-local vsi--auto-ws-marker nil
  "Start of a line holding only whitespace we inserted; trimmed if left blank.")

(defun vsi--note-auto-ws ()
  (setq vsi--auto-ws-marker
        (and (vsi--blank-line-p)
             (> (line-end-position) (line-beginning-position))
             (copy-marker (line-beginning-position)))))

(defun vsi--trim-auto-ws ()
  "Empty the line of auto-inserted whitespace that point has left."
  (when vsi--auto-ws-marker
    (let ((m vsi--auto-ws-marker))
      (unless (and (eq (marker-buffer m) (current-buffer))
                   (= (line-beginning-position) (marker-position m)))
        (when (eq (marker-buffer m) (current-buffer))
          (save-excursion
            (goto-char m)
            (when (looking-at "[ \t]+$")
              (delete-region (point) (line-end-position)))))
        (setq vsi--auto-ws-marker nil)))))

(defun vsi--newline-keep ()
  "Insert a newline carrying the indentation before point."
  (insert "\n" (vsi--make-indent
                (vsi--cols (vsi--lead (buffer-substring-no-properties (line-beginning-position) (point)))))))

(defun vsi--apply-enter-action (act)
  "Insert the newline(s) for the on-enter action ACT."
  (let* ((ind (plist-get act :indentation))
         (app (vsi--expand-append (plist-get act :append))))
    (pcase (plist-get act :indent)
      ('indent-outdent
       (insert "\n" (vsi--normalize (concat (make-string ind ?\s) app)))
       (save-excursion (insert "\n" (vsi--make-indent ind))))
      ('outdent
       (insert "\n" (vsi--normalize (concat (make-string (vsi--unshift ind) ?\s) app))))
      (_ (insert "\n" (vsi--normalize (concat (make-string ind ?\s) app)))))))

(defun vsi--insert-enter-indent (cols pos)
  "Insert a newline at POS and indent the new line COLS columns.
Indentation after POS is replaced."
  (let* ((bol (line-beginning-position))
         (eol (line-end-position))
         (fnw (save-excursion (goto-char bol) (and (re-search-forward "[^ \t]" eol t) (1- (point)))))
         (end (if fnw (max pos fnw) eol))
         (vcol (vsi--cols (buffer-substring-no-properties bol pos)))
         (new (vsi--make-indent cols)))
    (delete-region pos end)
    (insert "\n" new)
    ;; With the cursor inside the old indentation, keep it at the same column.
    (when (and fnw (<= pos fnw))
      (let ((v (if indent-tabs-mode (ceiling vcol (vsi--size)) vcol)))
        (goto-char (+ (line-beginning-position) (min v (length new))))))))

;;;###autoload
(defun vsi-newline ()
  "Insert a newline and indent it the way VS Code's Enter does."
  (interactive "*")
  (when (use-region-p) (delete-region (region-beginning) (region-end)))
  (let ((level (vsi--level)) (pos (point)))
    (cond
     ((= level 0) (insert "\n"))
     ((= level 1) (vsi--newline-keep))
     (t (let ((act (vsi--enter-action pos)))
          (if act
              (vsi--apply-enter-action act)
            (let ((ir (vsi--enter-indent pos)))
              (if ir (vsi--insert-enter-indent (cdr ir) pos) (vsi--newline-keep))))))))
  (vsi--note-auto-ws))

;;;; Tab -----------------------------------------------------------------------------------

(defun vsi--tab-good-indent (n)
  "VS Code's `_goodIndentForLine': the text to put on blank line N, or nil."
  (vsi--with-model
    (let ((r (vsi--inherit-indent n t)) (str ""))
      (cond
       (r (let ((cols (plist-get r :indent)))
            (when (eq (plist-get r :action) 'indent) (setq cols (vsi--shift cols)))
            (setq str (vsi--make-indent cols))))
       ((> n 1)
        (let ((last (cl-loop for l from (1- n) downto 1
                             unless (string-match-p "\\`[ \t]*\\'" (vsi--raw l)) return l)))
          (when last
            (let ((act (save-excursion (vsi--goto-line last) (end-of-line)
                                       (vsi--enter-action (point)))))
              (when act
                (setq str (concat (make-string (plist-get act :indentation) ?\s)
                                  (vsi--expand-append (plist-get act :append))))))))))
      (unless (string= str "") str))))

(defun vsi--jump-to-next-indent ()
  "Insert whitespace up to the next indentation stop."
  (if indent-tabs-mode
      (insert "\t")
    (let ((size (vsi--size)))
      (insert (make-string (- size (% (current-column) size)) ?\s)))))

(defun vsi--tab-point ()
  (let ((replaced
         (and (vsi--blank-line-p)
              (let* ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                     (good (vsi--tab-good-indent (line-number-at-pos nil t)))
                     (possible (vsi--normalize (or good (make-string (vsi--size) ?\s)))))
                (unless (string-prefix-p possible text)
                  (delete-region (line-beginning-position) (line-end-position))
                  (insert possible)
                  t)))))
    (unless replaced (vsi--jump-to-next-indent))))

(defun vsi--shift-lines (unshift)
  "Shift the selected lines, or the current one, one indentation stop.
Right, or left if UNSHIFT.  A port of VS Code's `ShiftCommand'."
  (let* ((use-reg (use-region-p))
         (beg (if use-reg (region-beginning) (point)))
         (end (if use-reg (region-end) (point)))
         (start-line (line-number-at-pos beg t))
         (end-line (line-number-at-pos end t))
         (pt (point)) (mk (and use-reg (mark)))
         (size (vsi--size))
         (prev-extra 0) edits)
    (when (and (/= start-line end-line) (save-excursion (goto-char end) (bolp)))
      (cl-decf end-line))
    (let ((single (= start-line end-line)))
      (vsi--with-model
        (vsi--goto-line start-line)
        (cl-loop
         for ln from start-line to end-line
         do (let* ((bol (line-beginning-position))
                   (text (buffer-substring-no-properties bol (line-end-position)))
                   (idx (string-match "[^ \t]" text))
                   (extra 0))
              (unless (or (and unshift (or (= (length text) 0) (eq idx 0)))
                          (and (not single) (not unshift) (= (length text) 0)))
                (let ((idx-end (or idx (length text))))
                  (when (and (> ln 1) (/= 0 (% (vsi--cols (substring text 0 idx-end)) size)))
                    (let ((act (save-excursion (forward-line -1) (end-of-line)
                                               (vsi--enter-action (point)))))
                      (when act
                        (setq extra prev-extra)
                        (let ((app (plist-get act :append)))
                          (when app
                            (cl-loop for c across app
                                     while (and (< extra size) (eq c ?\s))
                                     do (cl-incf extra))))
                        (when (plist-get act :remove)
                          (setq extra (max 0 (- extra (plist-get act :remove)))))
                        (cl-loop repeat extra
                                 while (and (> idx-end 0) (eq (aref text (1- idx-end)) ?\s))
                                 do (cl-decf idx-end)))))
                  (unless (and unshift (= idx-end 0))
                    (let* ((cols (vsi--cols (substring text 0 idx-end)))
                           (new (vsi--make-indent (if unshift (vsi--unshift cols) (vsi--shift cols)))))
                      (push (list bol idx-end new) edits)))))
              (setq prev-extra extra)
              (forward-line 1)))))
    ;; Where point and mark end up: after the content they were on, or at the end of the new
    ;; indentation; the start of a selection stays put when it was inside the indentation.
    (let* ((asc (reverse edits))
           (start-pos (and mk (min pt mk)))
           (adjust (lambda (pos stay)
                     (let ((acc 0) inside)
                       (cl-loop for (bol idx new) in asc
                                do (cond ((>= pos (+ bol idx)) (cl-incf acc (- (length new) idx)))
                                         ((> pos bol)
                                          (setq inside (+ bol acc (if stay (min (- pos bol) (length new))
                                                                    (length new))))
                                          (cl-return))
                                         (t (cl-return))))
                       (or inside (+ pos acc))))))
      (let ((new-pt (funcall adjust pt (and start-pos (= pt start-pos))))
            (new-mk (and mk (funcall adjust mk (= mk start-pos)))))
        (cl-loop for (bol idx new) in edits
                 do (goto-char bol) (delete-region bol (+ bol idx)) (insert new))
        (goto-char new-pt)
        (when new-mk (set-mark new-mk) (setq deactivate-mark nil))))))

;;;###autoload
(defun vsi-tab ()
  "Indent like VS Code's Tab.
On a blank line, go to where code belongs.  With a selection of several lines,
indent them all.  Anywhere else, move to the next indentation stop."
  (interactive "*")
  (cond
   ((and (use-region-p)
         (or (/= (line-number-at-pos (region-beginning)) (line-number-at-pos (region-end)))
             (save-excursion (goto-char (region-beginning))
                             (and (bolp) (= (region-end) (line-end-position))))))
    (vsi--shift-lines nil))
   ((use-region-p)
    (let ((b (region-beginning)))
      (delete-region b (region-end))
      (goto-char b)
      (vsi--jump-to-next-indent)))
   (t (vsi--tab-point))))

;;;###autoload
(defun vsi-backtab ()
  "Shift the selected lines, or the current one, one indentation stop left."
  (interactive "*")
  (vsi--shift-lines t))

;;;; Indent on type ------------------------------------------------------------------------

(defun vsi--electric-indent (pos ch)
  "Columns that line up the line with the opener of the bracket CH typed at POS."
  (when (and (>= (vsi--level) 2)
             (cl-find-if (lambda (b) (and (= (length (cdr b)) 1) (eq (aref (cdr b) 0) ch)))
                         (vsi-lang-brackets (vsi--lang)))
             (string-match-p "\\`[ \t]*\\'" (buffer-substring-no-properties (line-beginning-position) pos)))
    (let ((open (condition-case nil (scan-sexps (1+ pos) -1) (error nil))))
      (when (and open (< open (line-beginning-position)))
        (save-excursion
          (goto-char open)
          (vsi--cols (vsi--lead (buffer-substring-no-properties
                                 (line-beginning-position) (line-end-position)))))))))

(defun vsi--post-self-insert ()
  "Re-indent the line when the character just typed calls for it."
  (when (and (characterp last-command-event)
             (not (memq last-command-event '(?\s ?\t ?\n)))
             (eq (char-before) last-command-event)
             (not (use-region-p))
             (>= (vsi--level) 2))
    (let ((pos (1- (point))) (ch last-command-event))
      (unless (vsi--in-string-or-comment-p pos)
        (let ((target (or (vsi--type-indent pos ch) (vsi--electric-indent pos ch))))
          (when (and target
                     (/= target (vsi--cols (vsi--lead (buffer-substring-no-properties
                                                       (line-beginning-position) (line-end-position))))))
            (vsi--set-line-indent target)))))))

;;;; Reindent and paste --------------------------------------------------------------------

;;;###autoload
(defun vsi-reindent-lines (beg end)
  "Re-indent the lines from BEG to END from the language rules.
Interactively that is the region, or the whole buffer."
  (interactive (if (use-region-p) (list (region-beginning) (region-end)) (list (point-min) (point-max))))
  (let* ((first (line-number-at-pos beg t))
         (last (line-number-at-pos (if (and (> end beg) (save-excursion (goto-char end) (bolp))) (1- end) end) t))
         (edits (vsi-reindent-edits first last)))
    (save-excursion
      (dolist (e (reverse edits)) (vsi--set-line-indent-at (car e) (cdr e))))
    (message "Reindented %d line%s" (length edits) (if (= (length edits) 1) "" "s"))))

(defun vsi-indent-pasted (beg end)
  "Re-indent the text pasted between BEG and END, as VS Code does.
Lines that are empty are left empty."
  (unless (string-match-p "\\`[ \t\n]*\\'" (buffer-substring-no-properties beg end))
    (let (edits)
      (vsi--with-model
        (let* ((first-line (line-number-at-pos beg t))
               (end-line (line-number-at-pos end t))
               (first-text (vsi--raw first-line))
               (before (substring first-text 0 (min (length first-text) (- beg (vsi--bol first-line)))))
               (sl first-line) (stop nil))
          (when (string-match-p "\\`[ \t]*\\'" before)
            (let ((good (vsi--good-indent first-line)))
              (when good
                (if (/= good (vsi--cols (vsi--lead first-text)))
                    (progn
                      (push (cons first-line good) edits)
                      (setq first-text (concat (vsi--make-indent good)
                                               (substring first-text (length (vsi--lead first-text))))))
                  (unless (vsi--meta first-text) (setq stop t))))))
          (unless stop
            (while (and (< sl end-line) (string-match-p "\\`[ \t]*\\'" (vsi--raw (1+ sl))))
              (cl-incf sl))
            (when (/= sl end-line)
              (let ((good (let ((vsi--overrides (list (cons first-line first-text))))
                            (vsi--good-indent (1+ sl)))))
                (when good
                  (let ((offset (- good (vsi--line-indent (1+ sl)))))
                    (unless (= offset 0)
                      (cl-loop for i from (1+ sl) to end-line
                               for text = (vsi--raw i)
                               unless (string= text "")
                               do (let ((new (max 0 (+ (vsi--line-indent i) offset))))
                                    (unless (= new (vsi--line-indent i))
                                      (push (cons i new) edits))))))))))))
      (save-excursion
        (dolist (e (sort edits (lambda (a b) (> (car a) (car b)))))
          (vsi--set-line-indent-at (car e) (cdr e)))))))

(defvar vscode-indent-mode)

(defun vsi--after-yank (&rest _)
  (when (and vscode-indent-mode vsi-auto-indent-on-paste (>= (vsi--level) 4) (mark t))
    (vsi-indent-pasted (min (mark t) (point)) (max (mark t) (point)))))

;;;; Detecting the indentation -------------------------------------------------------------

;;;###autoload
(defun vsi-detect-indentation ()
  "Set `vsi-indent-size' from the buffer's contents."
  (interactive)
  (let ((guess (vsi-guess-indentation (vsi--size) t)))
    (setq vsi-indent-size (car guess))
    (when (called-interactively-p 'interactive)
      (message "Indentation: %d spaces%s" (car guess) (if (cdr guess) "" " (the file uses tabs)")))))

;;;; The mode ------------------------------------------------------------------------------

(defvar vscode-indent-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'vsi-newline)
    (define-key map (kbd "TAB") #'vsi-tab)
    (define-key map [backtab] #'vsi-backtab)
    map)
  "Keymap of `vscode-indent-mode'.")

(defvar-local vsi--electric-was nil
  "Whether `electric-indent-local-mode' was on before we turned it off.")

;;;###autoload
(define-minor-mode vscode-indent-mode
  "Indent like VS Code.
Enter, Tab, Shift-Tab and typing follow the language's rules."
  :lighter " VSI"
  :keymap vscode-indent-mode-map
  (if vscode-indent-mode
      (progn
        (setq vsi--electric-was (bound-and-true-p electric-indent-local-mode))
        (when (fboundp 'electric-indent-local-mode) (electric-indent-local-mode -1))
        (add-hook 'post-self-insert-hook #'vsi--post-self-insert nil t)
        (add-hook 'post-command-hook #'vsi--trim-auto-ws nil t)
        (advice-add 'yank :after #'vsi--after-yank)
        (when (and vsi-detect-indentation (not (local-variable-p 'vsi-indent-size)))
          (vsi-detect-indentation)))
    (remove-hook 'post-self-insert-hook #'vsi--post-self-insert t)
    (remove-hook 'post-command-hook #'vsi--trim-auto-ws t)
    (when (and vsi--electric-was (fboundp 'electric-indent-local-mode))
      (electric-indent-local-mode 1))))

(defun vsi--maybe-enable ()
  "Turn on `vscode-indent-mode' in buffers whose major mode has a rule set."
  (when (and (not (minibufferp)) (vsi--known-mode-p))
    (vscode-indent-mode 1)))

;;;###autoload
(define-globalized-minor-mode vscode-indent-global-mode vscode-indent-mode vsi--maybe-enable
  :group 'vscode-indent)

(provide 'vscode-indent)
;;; vscode-indent.el ends here
