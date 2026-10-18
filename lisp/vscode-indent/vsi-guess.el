;;; vsi-guess.el --- Guess a buffer's indentation, as VS Code does -*- lexical-binding: t; -*-

;; A port of VS Code's src/vs/editor/common/model/indentationGuesser.ts (MIT licensed,
;; Copyright (c) Microsoft Corporation).

;;; Commentary:
;; Looks at up to 10000 lines, compares the indentation of each non-blank line with the one
;; before it, and picks the indentation size that most of the differences agree on.  Tabs
;; versus spaces goes to whichever indents more lines.

;;; Code:

(require 'cl-lib)

(defun vsi--spaces-diff (a alen b blen)
  "Difference in indentation between lines A and B.
ALEN and BLEN are the lengths of their indentations.  Returns
\(DIFF . LOOKS-LIKE-ALIGNMENT)."
  (let ((i 0) (a-spaces 0) (a-tabs 0) (b-spaces 0) (b-tabs 0))
    (while (and (< i alen) (< i blen) (eq (aref a i) (aref b i))) (cl-incf i))
    (cl-loop for j from i below alen
             do (if (eq (aref a j) ?\s) (cl-incf a-spaces) (cl-incf a-tabs)))
    (cl-loop for j from i below blen
             do (if (eq (aref b j) ?\s) (cl-incf b-spaces) (cl-incf b-tabs)))
    (cond
     ((and (> a-spaces 0) (> a-tabs 0)) (cons 0 nil))
     ((and (> b-spaces 0) (> b-tabs 0)) (cons 0 nil))
     (t
      (let ((tabs-diff (abs (- a-tabs b-tabs)))
            (spaces-diff (abs (- a-spaces b-spaces))))
        (cond
         ((= tabs-diff 0)
          ;; A difference that comes from aligning with the line above is not an indent:
          ;;   const a = b + c,
          ;;         d = b - c;
          (cons spaces-diff
                (and (> spaces-diff 0)
                     (<= 0 (1- b-spaces)) (< (1- b-spaces) (length a)) (< b-spaces (length b))
                     (not (eq (aref b b-spaces) ?\s))
                     (eq (aref a (1- b-spaces)) ?\s)
                     (eq (aref a (1- (length a))) ?,))))
         ((= (% spaces-diff tabs-diff) 0) (cons (/ spaces-diff tabs-diff) nil))
         (t (cons 0 nil))))))))

(defun vsi-guess-indentation (default-size default-spaces)
  "Guess the indentation of the current buffer.
Returns (SIZE . INSERT-SPACES), falling back to DEFAULT-SIZE and
DEFAULT-SPACES."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((tab-lines 0) (space-lines 0)
            (prev-text "") (prev-indent 0)
            (counts (make-vector 9 0))
            (lines 0))
        (while (and (< lines 10000) (not (eobp)))
          (cl-incf lines)
          (let* ((text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                 (indent (and (string-match "[^ \t]" text) (match-beginning 0))))
            (when indent
              (let ((spaces 0) (tabs 0))
                (dotimes (k indent) (if (eq (aref text k) ?\t) (cl-incf tabs) (cl-incf spaces)))
                (cond ((> tabs 0) (cl-incf tab-lines))
                      ((> spaces 1) (cl-incf space-lines))))
              (let* ((d (vsi--spaces-diff prev-text prev-indent text indent))
                     (diff (car d)))
                (unless (and (cdr d) (not (and default-spaces (= default-size diff))))
                  (when (<= diff 8) (cl-incf (aref counts diff)))
                  (setq prev-text text prev-indent indent)))))
          (forward-line 1))
        (let ((insert-spaces default-spaces) (size default-size))
          (unless (= tab-lines space-lines)
            (setq insert-spaces (< tab-lines space-lines)))
          (when insert-spaces
            (let ((best 0))
              (dolist (k '(2 4 6 8 3 5 7))
                (when (> (aref counts k) best)
                  (setq best (aref counts k) size k))))
            ;; Let 2 win over 4 when it has at least 2/3 of its occurrences (YAML nesting).
            (when (and (= size 4) (> (aref counts 4) 0) (> (aref counts 2) 0)
                       (>= (aref counts 2) (/ (* (aref counts 4) 2.0) 3)))
              (setq size 2)))
          (cons size insert-spaces))))))

(provide 'vsi-guess)
;;; vsi-guess.el ends here
