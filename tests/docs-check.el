;;; docs-check.el --- Verify docs/index.html against the live configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/docs-check.el
;; Reads the JSON data embedded in docs/index.html and checks every claim it
;; can: keybindings, commands, menu entries, counts, files and make targets.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'seq)

(dolist (f '(transient magit devops-azdo devops-azdo-magit minuet corfu))
  (require f nil t))

(defvar docs--root (file-name-as-directory (expand-file-name user-emacs-directory)))
(defun docs--file (name) (expand-file-name name docs--root))
(defun docs--slurp (name)
  (with-temp-buffer (insert-file-contents (docs--file name)) (buffer-string)))

(defvar docs--html-file (or (getenv "DOCS_FILE") (docs--file "docs/index.html"))
  "The page under test; override with $DOCS_FILE (used to validate a candidate).")
(defvar docs--html (with-temp-buffer (insert-file-contents docs--html-file) (buffer-string)))
(defvar docs--data
  (let* ((start (string-match "<script type=\"application/json\" id=\"ide-data\">" docs--html))
         (open-end (match-end 0))
         (close (string-match "</script>" docs--html open-end)))
    (unless (and start close) (error "No embedded data block in docs/index.html"))
    (json-parse-string (replace-regexp-in-string "<\\\\/" "</" (substring docs--html open-end close))
                       :object-type 'alist :array-type 'list)))
(defun docs--get (key) (alist-get key docs--data))

(defun docs--keyspec (keys)
  (if (string-prefix-p "[" keys) (car (read-from-string keys)) (kbd keys)))

(ert-deftest docs-data-parses ()
  (should (> (length (docs--get 'keybinds)) 50))
  (should (docs--get 'menus))
  (should (docs--get 'workflows)))

(ert-deftest docs-keybindings-match-config ()
  "Every documented key resolves to the documented command."
  (let (bad)
    (dolist (k (docs--get 'keybinds))
      (let* ((keys (alist-get 'keys k)) (cmd (intern (alist-get 'cmd k)))
             (ctx (alist-get 'ctx k)) (spec (docs--keyspec keys))
             (actual (if (equal ctx "global")
                         (key-binding spec)
                       (let ((map (intern ctx)))
                         (if (boundp map) (lookup-key (symbol-value map) spec)
                           (list 'unbound-map map))))))
        (unless (eq actual cmd) (push (list keys ctx 'doc cmd 'actual actual) bad))))
    (should-not bad)))

(ert-deftest docs-all-commands-exist ()
  "Every command named anywhere on the page is defined."
  (let ((cmds nil) missing)
    (dolist (k (docs--get 'keybinds)) (push (alist-get 'cmd k) cmds))
    (dolist (f (docs--get 'features)) (dolist (c (alist-get 'cmds f)) (push c cmds)))
    (dolist (m (docs--get 'menus))
      (dolist (g (alist-get 'groups m))
        (dolist (i (alist-get 'items g)) (when-let* ((c (alist-get 'cmd i))) (push c cmds)))))
    (dolist (c (delete-dups cmds))
      (unless (fboundp (intern c)) (push c missing)))
    (should-not missing)))

(defun docs--layout-keys (prefix)
  "Return all suffix keys declared by transient PREFIX."
  (let (keys)
    (cl-labels ((walk (x)
                  (cond ((vectorp x) (seq-doseq (e x) (walk e)))
                        ((and (consp x) (proper-list-p x))
                         (if (and (symbolp (car x)) (plist-get (cdr x) :key))
                             (push (plist-get (cdr x) :key) keys)
                           (mapc #'walk x))))))
      (walk (get prefix 'transient--layout)))
    keys))

(ert-deftest docs-menus-match-transients ()
  "Menu entries have the documented key, label and command, and none are missing."
  (let (bad)
    (dolist (m (docs--get 'menus))
      (let ((prefix (alist-get 'prefix m)))
        (when (stringp prefix)
          (let ((sym (intern prefix)) (documented nil))
            (dolist (g (alist-get 'groups m))
              (dolist (i (alist-get 'items g))
                (let* ((key (alist-get 'key i))
                       (suffix (ignore-errors (transient-get-suffix sym key))))
                  (push key documented)
                  (cond
                   ((null suffix) (push (list prefix key 'missing) bad))
                   (t (let ((desc (plist-get (cdr suffix) :description))
                            (cmd (plist-get (cdr suffix) :command)))
                        (unless (equal desc (alist-get 'label i))
                          (push (list prefix key 'label desc (alist-get 'label i)) bad))
                        (when-let* ((c (alist-get 'cmd i)))
                          (unless (eq cmd (intern c))
                            (push (list prefix key 'command cmd c) bad)))))))))
            ;; Menus we fully own must be documented completely.
            (unless (equal prefix "magit-push")
              (dolist (k (docs--layout-keys sym))
                (unless (member k documented) (push (list prefix k 'undocumented) bad))))))))
    (should-not bad)))

(ert-deftest docs-vote-picker-matches-config ()
  (let ((doc (docs--get 'votes)))
    (should (equal (mapcar (lambda (v) (list (alist-get 'key v) (alist-get 'label v) (alist-get 'value v))) doc)
                   (mapcar (lambda (v) (list (char-to-string (nth 0 v)) (nth 1 v) (nth 2 v))) azdo--votes)))))

(ert-deftest docs-counts-are-true ()
  (let* ((stats (docs--get 'stats))
         (stat (lambda (label) (alist-get 'v (seq-find (lambda (s) (string-match-p label (alist-get 'l s))) stats))))
         (tests (let ((n 0))
                  (dolist (f (directory-files (docs--file "tests") t "\\.el\\'") n)
                    (with-temp-buffer (insert-file-contents f)
                                      (while (re-search-forward "^(ert-deftest " nil t) (cl-incf n))))))
         (modules (length (directory-files-recursively (docs--file "modules") "\\.el\\'")))
         (kbs (length (docs--get 'keybinds))))
    (should (equal (funcall stat "automated tests") (number-to-string tests)))
    (should (equal (funcall stat "Emacs Lisp modules") (number-to-string modules)))
    (should (equal (funcall stat "CLIs and servers") (number-to-string (length ide-mise-required-executables))))
    (should (equal (funcall stat "documented keybindings") (number-to-string kbs)))
    (should (string-match-p (format "declares %d CLIs" (length ide-mise-required-executables)) docs--html))))

(ert-deftest docs-tools-match-mise-manifest ()
  (let ((toml (docs--slurp "mise.toml"))
        (doc (docs--get 'tools)))
    (should (equal (sort (mapcar (lambda (tl) (alist-get 'exe tl)) doc) #'string<)
                   (sort (mapcar #'car ide-mise-required-executables) #'string<)))
    (dolist (tl doc)
      (should (string-match-p (regexp-quote (alist-get 'spec tl)) toml)))))

(ert-deftest docs-module-map-is-complete ()
  (let* ((listed (mapcar (lambda (m) (alist-get 'file m)) (docs--get 'modules)))
         (actual (append '("early-init.el" "init.el")
                         (mapcar (lambda (f) (file-relative-name f docs--root))
                                 (directory-files-recursively (docs--file "modules") "\\.el\\'")))))
    (should (equal (sort (copy-sequence listed) #'string<) (sort actual #'string<)))
    (dolist (f listed) (should (file-exists-p (docs--file f))))))

(ert-deftest docs-make-targets-exist ()
  (let ((mk (docs--slurp "Makefile")))
    (dolist (m (docs--get 'make))
      (should (string-match-p (format "^%s:" (regexp-quote (string-remove-prefix "make " (alist-get 'cmd m)))) mk)))))

(ert-deftest docs-workflow-keys-are-real ()
  "Modified or multi-key chords used in workflows are real bindings."
  (let ((known (mapcar (lambda (k) (alist-get 'keys k)) (docs--get 'keybinds))) bad)
    (dolist (w (docs--get 'workflows))
      (dolist (s (alist-get 'steps w))
        (let ((text (alist-get 'd s)) (pos 0))
          (while (string-match "{{\\(.+?\\)}}" text pos)
            (let ((seq (match-string 1 text)))
              (when (and (string-match-p "\\(^\\|[ ]\\)\\(C-\\|M-\\|S-<\\)" seq)
                         (not (member seq known)))
                (push seq bad)))
            (setq pos (match-end 0))))))
    (should-not bad)))

(ert-deftest docs-commands-are-covered ()
  "Every user-facing command is documented somewhere (keys, menus or a feature card)."
  (let* ((data (ide-docs--parse-data docs--html))
         (covered (ide-docs--data-commands data))
         (missing (cl-remove-if (lambda (c) (member (symbol-name c) covered)) (ide-docs--user-commands))))
    (should-not missing)))

(ert-deftest docs-examples-match-shipped-files ()
  "The user-config examples on the page are exactly the files in examples/."
  (let ((data (ide-docs--parse-data docs--html)))
    (should (equal (alist-get 'examples data) (vconcat (ide-docs--facts-examples))))
    (should (> (length (alist-get 'examples data)) 4))))

(ert-deftest docs-recipes-are-valid ()
  "Every recipe on the page reads as Lisp and macroexpands (or is a mise [tools] table)."
  (let ((data (ide-docs--parse-data docs--html)) bad)
    (should (> (length (alist-get 'recipes data)) 5))
    (cl-loop for r across (alist-get 'recipes data)
             for code = (alist-get 'code r)
             do (pcase (alist-get 'lang r)
                  ("elisp" (with-temp-buffer
                             (insert code) (goto-char (point-min))
                             (condition-case err (while t (macroexpand-all (read (current-buffer))))
                               (end-of-file nil)
                               (error (push (list (alist-get 'title r) err) bad)))))
                  ("toml" (unless (string-match-p "^\\[tools\\]" code) (push (alist-get 'title r) bad)))
                  (other (push (list (alist-get 'title r) 'unknown-lang other) bad))))
    (should-not bad)))

(ert-deftest docs-brand-is-foundry ()
  (should (string-match-p "<title>Foundry" docs--html))
  (should (string-match-p "<span>Foundry<br>" docs--html))
  (should-not (string-match-p "Emacs IDE (safe mode)" docs--html)))

(ert-deftest docs-anchors-resolve ()
  (let ((pos 0) bad)
    (while (string-match "href=\"#\\([a-z-]+\\)\"" docs--html pos)
      (let ((id (match-string 1 docs--html)))
        (unless (or (equal id "main") (string-match-p (format "id=\"%s\"" (regexp-quote id)) docs--html))
          (push id bad)))
      (setq pos (match-end 0)))
    (should-not bad)))

(ert-deftest docs-no-external-resources ()
  "The page must work offline: no CDN scripts, fonts or stylesheets."
  (should-not (string-match-p "\\(src\\|href\\)=\"https?://" docs--html))
  (should-not (string-match-p "@import\\|url(http" docs--html)))

(ert-run-tests-batch-and-exit)

;;; docs-check.el ends here
