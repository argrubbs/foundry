;;; ansible-xref.el --- Jump to Ansible variable definitions and references -*- lexical-binding: t; -*-

;;; Commentary:
;; An xref backend for Ansible YAML.  It searches the project's YAML files as
;; plain text and finds:
;;
;; - where a variable is defined: a `register:' or `loop_var:' name, a key in a
;;   `vars:' or `set_fact:' block, and a top-level key of a role's defaults and
;;   vars files or of group_vars and host_vars;
;; - where it is used: inside {{ }} and {% %}, and in `when:'-style conditions;
;; - where an include or role reference points: `include_tasks: x.yml',
;;   `import_role' with `name: x', and `roles:' items.
;;
;; There is no scope analysis: every definition of a name is listed.  The
;; backend answers only for a name it can find a definition for, or an include
;; or role it can resolve, so anything else (modules, keywords) goes on to Eglot.
;;
;; Limits: a list must be indented under its key, and a Jinja expression must
;; open and close on one line.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'subr-x)
(require 'xref)

(defconst ansible-xref--yaml-re "\\.ya?ml\\'"
  "Regexp matching the name of a YAML file.")

(defconst ansible-xref--vars-dir-re "/\\(?:defaults\\|vars\\|group_vars\\|host_vars\\)/"
  "Regexp matching the path of a file whose top-level keys are variables.")

(defconst ansible-xref--vars-blocks '("vars" "set_fact")
  "Keys whose child keys define variables.")

(defconst ansible-xref--conditional-keys '("when" "until" "failed_when" "changed_when")
  "Keys whose value is a Jinja expression written without braces.")

(defconst ansible-xref--conditional-re
  (concat "[ \t]*\\(?:-[ \t]+\\)?" (regexp-opt ansible-xref--conditional-keys) ":")
  "Regexp matching the start of a line that holds a conditional key.")

(defconst ansible-xref--include-re
  (concat "[ \t]*\\(?:-[ \t]+\\)?\\(?:[[:alnum:]_]+\\.\\)*"
          (regexp-opt '("include_tasks" "import_tasks" "include_vars" "import_playbook"))
          ":[ \t]*\\([^#\n]*[^# \t\n]\\)")
  "Regexp matching a line that includes a file, with or without a collection prefix.
Group 1 is the file name.")

(defconst ansible-xref--role-re
  "[ \t]*\\(-[ \t]+\\)?\\(?:\\(role\\|name\\):[ \t]*\\)?\\([[:alnum:]_.-]+\\)[ \t]*$"
  "Regexp matching a line that may name a role.
Group 1 is a list dash, group 2 a `role' or `name' key and group 3 the name.")

(defconst ansible-xref--name-re "[[:alpha:]_][[:alnum:]_]*"
  "Regexp matching a variable name.")

(defconst ansible-xref--register-re
  "^[ \t]*\\(?:register\\|loop_var\\):[ \t]*\\(%s\\)[ \t]*\\(?:#.*\\)?$"
  "Format string for a line that names a variable with `register' or `loop_var'.")

(defconst ansible-xref--key-re "^[ \t]*\\(%s\\):"
  "Format string for a line that starts with a mapping key; `%s' is the key.")

(defconst ansible-xref--use-re
  "\\(?:^\\|[^[:alnum:]_.]\\)\\(%s\\)\\(?:[^[:alnum:]_]\\|$\\)"
  "Format string for a whole-word occurrence of a name that is not a field.")

;;;; Reading the project

(defun ansible-xref--files ()
  "Return the readable YAML files of the current project or directory tree."
  (seq-filter
   (lambda (file)
     (and (string-match-p ansible-xref--yaml-re file) (file-readable-p file)))
   (if-let* ((project (project-current)))
       (project-files project)
     (directory-files-recursively default-directory ansible-xref--yaml-re nil
                                  (lambda (dir) (not (string-match-p "/\\.[^/]+\\'" dir)))))))

(defun ansible-xref--in-files (function)
  "Call FUNCTION on each project YAML file and return the appended results.
FUNCTION gets the file name and runs with the file's text as the current
buffer.  A file visited in a buffer is read from it, so unsaved edits count."
  (cl-loop for file in (ansible-xref--files)
           append (if-let* ((buffer (find-buffer-visiting file)))
                      (with-current-buffer buffer
                        (save-excursion
                          (save-restriction
                            (widen)
                            (funcall function file))))
                    (with-temp-buffer
                      (insert-file-contents file)
                      (funcall function file)))))

(defun ansible-xref--line-xref (file)
  "Return an xref to the line at point in FILE."
  (xref-make (string-trim (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
             (xref-make-file-location file (line-number-at-pos) (current-column))))

(defun ansible-xref--matches (file regexp accept-p)
  "Return xrefs to the matches of REGEXP in the current buffer, one per line.
FILE is the buffer's file.  Group 1 of REGEXP is the name.  ACCEPT-P is called
with point at the start of the name and says whether the match counts."
  (let (xrefs)
    (goto-char (point-min))
    (while (re-search-forward regexp nil t)
      (let ((start (match-beginning 1))
            (end (match-end 1)))
        (goto-char start)
        (if (funcall accept-p)
            (progn (push (ansible-xref--line-xref file) xrefs)
                   (forward-line 1))
          (goto-char end))))
    (nreverse xrefs)))

;;;; Reading the line at point

(defun ansible-xref--parent-key ()
  "Return the mapping key that encloses the current line, or nil.
A collection prefix is dropped, so `ansible.builtin.set_fact' gives `set_fact'."
  (save-excursion
    (let ((indent (current-indentation))
          key)
      (while (and (not key) (zerop (forward-line -1)))
        (when (and (< (current-indentation) indent)
                   (looking-at "[ \t]*\\(?:-[ \t]+\\)?\\([[:alnum:]_.]+\\):"))
          (setq key (car (last (split-string (match-string-no-properties 1) "\\."))))))
      key)))

(defun ansible-xref--inside-jinja-p ()
  "Non-nil if point is between Jinja delimiters on the current line."
  (save-excursion
    (and (re-search-backward "{{\\|{%\\|}}\\|%}" (line-beginning-position) t)
         (member (match-string 0) '("{{" "{%"))
         t)))

(defun ansible-xref--conditional-p ()
  "Non-nil if point is in the value of a `when:'-style key, on its line or below."
  (let ((start (point)))
    (or (save-excursion
          (beginning-of-line)
          (and (looking-at ansible-xref--conditional-re) (< (match-end 0) start)))
        (member (ansible-xref--parent-key) ansible-xref--conditional-keys))))

(defun ansible-xref--expression-p ()
  "Non-nil if point is in a Jinja expression, with or without braces."
  (or (ansible-xref--inside-jinja-p) (ansible-xref--conditional-p)))

(defun ansible-xref--definition-key-p (file)
  "Non-nil if the key at point in FILE defines a variable."
  (if (zerop (current-indentation))
      (string-match-p ansible-xref--vars-dir-re file)
    (member (ansible-xref--parent-key) ansible-xref--vars-blocks)))

(defun ansible-xref--variable-at-point ()
  "Return the variable name at point, or nil.
On a field such as `result.stdout' it is the base name, `result'."
  (save-excursion
    (skip-chars-backward "[:alnum:]_.")
    (when (looking-at ansible-xref--name-re)
      (match-string-no-properties 0))))

;;;; Includes and roles

(defun ansible-xref--include-file (value)
  "Return the existing file that the include VALUE names, or nil.
VALUE is taken relative to the current file; a templated VALUE has no target."
  (let ((name (string-trim value "[\"']" "[\"']")))
    (when (and buffer-file-name (not (string-match-p "{[{%]" name)))
      (let ((file (expand-file-name name (file-name-directory buffer-file-name))))
        (and (file-regular-p file) file)))))

(defun ansible-xref--role-name ()
  "Return the role that the current line names, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at ansible-xref--role-re)
      (let ((item (match-string 1))
            (key (match-string 2))
            (name (match-string-no-properties 3)))
        (when (pcase key
                ("role" t)
                ("name" (member (ansible-xref--parent-key) '("include_role" "import_role")))
                (_ (and item (equal (ansible-xref--parent-key) "roles"))))
          name)))))

(defun ansible-xref--role-file (role)
  "Return the tasks file of ROLE in the project, or nil."
  (let ((regexp (format "/roles/%s/tasks/main\\.ya?ml\\'" (regexp-quote role))))
    (seq-find (lambda (file) (string-match-p regexp file)) (ansible-xref--files))))

(defun ansible-xref--link-file ()
  "Return the file that the include or role on the current line points to, or nil."
  (save-excursion
    (beginning-of-line)
    (if (looking-at ansible-xref--include-re)
        (ansible-xref--include-file (match-string-no-properties 1))
      (when-let* ((role (ansible-xref--role-name)))
        (ansible-xref--role-file role)))))

;;;; Searching

(defun ansible-xref--definitions (name)
  "Return xrefs to the definitions of the variable NAME in the project."
  (let ((quoted (regexp-quote name)))
    (ansible-xref--in-files
     (lambda (file)
       (append
        (ansible-xref--matches file (format ansible-xref--register-re quoted) #'always)
        (ansible-xref--matches file (format ansible-xref--key-re quoted)
                               (lambda () (ansible-xref--definition-key-p file))))))))

(defun ansible-xref--references (name)
  "Return xrefs to the uses of the variable NAME in the project."
  (let ((regexp (format ansible-xref--use-re (regexp-quote name))))
    (ansible-xref--in-files
     (lambda (file)
       (ansible-xref--matches file regexp #'ansible-xref--expression-p)))))

;;;; The backend

(defun ansible-xref--identifier ()
  "Return the identifier at point, or nil.
An include or role is named by its target file, kept in the `ansible-xref-file'
property of the string; anything else is a variable name."
  (if-let* ((file (ansible-xref--link-file)))
      (propertize (file-name-nondirectory file) 'ansible-xref-file file)
    (ansible-xref--variable-at-point)))

;;;###autoload
(defun ansible-xref-backend ()
  "Return the `ansible-xref' backend when it can resolve what is at point.
Otherwise return nil, so the next backend answers."
  (when-let* ((identifier (ansible-xref--identifier))
              ((or (get-text-property 0 'ansible-xref-file identifier)
                   (ansible-xref--definitions identifier))))
    'ansible-xref))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql ansible-xref)))
  "Return the variable or link at point."
  (ansible-xref--identifier))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql ansible-xref)))
  "Offer no completion: identifiers come from the text at point."
  nil)

(cl-defmethod xref-backend-definitions ((_backend (eql ansible-xref)) identifier)
  "Return xrefs to where IDENTIFIER is defined, or to the file it links to."
  (if-let* ((file (get-text-property 0 'ansible-xref-file identifier)))
      (list (xref-make identifier (xref-make-file-location file 1 0)))
    (ansible-xref--definitions identifier)))

(cl-defmethod xref-backend-references ((_backend (eql ansible-xref)) identifier)
  "Return xrefs to the uses of the variable IDENTIFIER."
  (ansible-xref--references identifier))

(provide 'ansible-xref)

;;; ansible-xref.el ends here
