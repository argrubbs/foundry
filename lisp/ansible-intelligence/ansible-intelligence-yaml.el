;;; ansible-intelligence-yaml.el --- Verified YAML structural extraction -*- lexical-binding: t; -*-

;;; Commentary:
;; Structural extraction of Ansible outlines and static relationship links
;; using defensive source scanning.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'ansible-intelligence-model)

(defun ansible-intelligence-yaml--target-text (text)
  "Normalize a static relationship TEXT while preserving dynamic markers."
  (let ((trimmed (string-trim text)))
    (if (and (> (length trimmed) 1)
             (memq (aref trimmed 0) '(?\" ?\'))
             (= (aref trimmed 0) (aref trimmed (1- (length trimmed)))))
        (substring trimmed 1 -1)
      trimmed)))

(defun ansible-intelligence-yaml--outline
    (name kind start end category &optional confidence)
  "Create a source-local outline entry."
  (ansible-intelligence-outline-entry-create
   :name name :kind kind
   :range (ansible-intelligence-range-create :start start :end end)
   :category category :source "YAML scanner"
   :confidence (or confidence 'static) :file buffer-file-name))

(defun ansible-intelligence-yaml--link
    (kind target start end &optional confidence)
  "Create a source-local relationship link."
  (ansible-intelligence-link-create
   :kind kind :target target
   :range (ansible-intelligence-range-create :start start :end end)
   :confidence (or confidence 'static)))

(defun ansible-intelligence-yaml--fallback-role-item (line line-start)
  "Return a direct role target triple from sequence-item LINE.

The returned value is `(TARGET START END)'.  Mapping items contribute only
their direct `role' or `name' value; nested role variables are ignored."
  (when (string-match "^[ \t]*-[ \t]*\\(.+\\)[ \t]*\\'" line)
    (let* ((raw-start (match-beginning 1))
           (raw (match-string-no-properties 1 line))
           (content (string-trim raw))
           (content-start
            (+ raw-start (or (string-match (regexp-quote content) raw) 0))))
      (if (and (not (string-empty-p content))
               (not (string-match-p
                     "\\`[[:alnum:]_.-]+[ \t]*:" content)))
          (list (ansible-intelligence-yaml--target-text content)
                (+ line-start content-start)
                (+ line-start content-start (length content)))
        (when (string-match
               "\\`\\(?:role\\|name\\):[ \t]*\\(.+\\)\\'" content)
          (let* ((raw-target (match-string-no-properties 1 content))
                 (target (string-trim raw-target))
                 (target-start
                  (+ content-start (match-beginning 1)
                     (or (string-match (regexp-quote target) raw-target) 0))))
            (unless (string-empty-p target)
              (list (ansible-intelligence-yaml--target-text target)
                    (+ line-start target-start)
                    (+ line-start target-start (length target))))))))))

(defun ansible-intelligence-yaml--fallback-outline (kind)
  "Build a conservative outline from source lines."
  (ignore kind)
  (let (entries section list-kind list-indent list-item-indent)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (point))
               (indent (current-indentation))
               (raw-line
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)))
               (line (string-trim
                      raw-line)))
          (when (and list-kind list-indent
                     (not (string-empty-p line))
                     (<= indent list-indent)
                     (not (string-match-p
                           "\\`\\(?:- +\\)?roles:[ \t]*\\'" line)))
            (setq list-kind nil list-indent nil list-item-indent nil))
          (cond
           ((string-match
             "\\`\\(?:- +\\)?\\(tasks\\|handlers\\|block\\|rescue\\|always\\):"
             line)
            (setq section (match-string 1 line)))
           ((and (not (eq list-kind 'roles))
                 (string-match "\\`- +name:[ \t]*\\(.+\\)" line))
            (let* ((raw (string-trim (match-string 1 line)))
                   (name (ansible-intelligence-yaml--target-text raw))
                   (play-p
                    (and (zerop indent)
                         (save-excursion
                           (let ((limit (+ (point) 4096))
                                 found)
                             (forward-line 1)
                             (while (and (< (point) limit)
                                         (not (eobp))
                                         (not (looking-at "^- ")))
                               (when (looking-at "[ \t]*hosts:")
                                 (setq found t))
                               (forward-line 1))
                             found))))
                   (category (if play-p "Plays"
                               (if (string= section "handlers")
                                   "Handlers" "Tasks"))))
              (push (ansible-intelligence-yaml--outline
                     name (if play-p 'play
                           (if (string= section "handlers") 'handler 'task))
                     (+ line-start (string-match (regexp-quote raw)
                                                 (buffer-substring-no-properties
                                                  line-start (line-end-position))))
                     (line-end-position) category
                     'recovered)
                    entries)))
           ((string-match "\\`\\(?:- +\\)?\\(roles\\):[ \t]*\\(.+\\)" line)
            (let ((raw (string-trim (match-string 2 line))))
              (push (ansible-intelligence-yaml--outline
                     (ansible-intelligence-yaml--target-text raw) 'role
                     (+ line-start (string-match (regexp-quote raw)
                                                 (buffer-substring-no-properties
                                                  line-start (line-end-position))))
                     (line-end-position) "Roles" 'recovered)
                    entries)))
           ((string-match "\\`\\(?:- +\\)?\\(block\\|rescue\\|always\\):" line)
            (push (ansible-intelligence-yaml--outline
                   (match-string 1 line) 'block line-start (line-end-position)
                   "Blocks" 'recovered)
                  entries))
           ((string-match
             "\\`\\(?:- +\\)?\\(vars_files\\|import_tasks\\|include_tasks\\|include_vars\\|roles\\):[ \t]*\\(.+\\)"
             line)
            (setq list-kind (intern (replace-regexp-in-string
                                     "_" "-" (match-string 1 line)))))
           ((string-match
             "\\`\\(?:- +\\)?\\(roles\\):[ \t]*\\'" line)
            (setq list-kind 'roles list-indent indent list-item-indent nil))
           ((and (eq list-kind 'roles)
                 (> indent list-indent)
                 (string-match-p "^[ \t]*-" raw-line))
            (when-let* ((target
                        (ansible-intelligence-yaml--fallback-role-item
                         raw-line line-start)))
              (when (or (null list-item-indent)
                        (= indent list-item-indent))
                (push
                 (ansible-intelligence-yaml--outline
                  (nth 0 target) 'role (nth 1 target) (nth 2 target)
                  "Roles" 'recovered)
                 entries)
                (setq list-item-indent indent)))))
          (forward-line 1)))
      (nreverse entries))))

(defun ansible-intelligence-yaml--fallback-links ()
  "Build conservative static links from source lines."
  (let* ((links nil)
         (list-kind nil)
         (list-indent nil)
         (list-item-indent nil)
         (add-link
          (lambda (kind target start end)
            (unless (string-empty-p target)
              (push
               (ansible-intelligence-yaml--link
                kind target start end
                (if (string-match-p "{{\\|{%\\|{#" target)
                    'dynamic 'static))
               links)))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line-start (point))
               (indent (current-indentation))
               (line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (blank-p (string-match-p "^[ \t]*$" line)))
          (when (and list-kind (not blank-p) (<= indent list-indent))
            (setq list-kind nil list-indent nil list-item-indent nil))
          (cond
           ((string-match
             "^[ \t]*\\(?:-[ \t]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_tasks\\|include_tasks\\|vars_files\\|include_vars\\):[ \t]*\\([^#\n]+\\)"
             line)
            (let* ((kind
                    (intern (replace-regexp-in-string
                             "_" "-" (match-string 1 line))))
                   (raw (string-trim (match-string 2 line)))
                   (value-start (+ line-start (match-beginning 2)))
                   (mapping-file
                    (and (string-match-p "\\`{" raw)
                         (string-match
                          "[{,][ \t]*file:[ \t]*\\([^,}#]+\\)" raw))))
              (if mapping-file
                  (let* ((raw-target (match-string 1 raw))
                         (target (string-trim raw-target))
                         (target-start
                          (+ value-start (match-beginning 1)
                             (or (string-match (regexp-quote target)
                                               raw-target)
                                 0))))
                    (funcall add-link kind target target-start
                             (+ target-start (length target))))
                (funcall add-link kind
                         (ansible-intelligence-yaml--target-text raw)
                         value-start (+ line-start (match-end 2))))))
           ((string-match
             "^[ \t]*\\(?:-[ \t]*\\)?\\(?:[[:alnum:]_]+\\.\\)*\\(import_role\\|include_role\\|import_tasks\\|include_tasks\\|vars_files\\|include_vars\\|roles\\):[ \t]*$"
             line)
            (setq list-kind
                  (pcase (match-string 1 line)
                    ("roles" 'import-role)
                    ("vars_files" 'vars-files)
                    (_ (intern (replace-regexp-in-string
                                "_" "-" (match-string 1 line)))))
                  list-indent indent
                  list-item-indent nil))
           ((and list-kind (> indent list-indent) (not blank-p))
            (if (memq list-kind '(import-role include-role))
                (progn
                  (when (string-match-p "^[ \t]*-" line)
                    (when-let* ((target
                                (ansible-intelligence-yaml--fallback-role-item
                                 line line-start)))
                      (when (or (null list-item-indent)
                                (= indent list-item-indent))
                        (funcall add-link list-kind (nth 0 target)
                                 (nth 1 target) (nth 2 target))
                        (setq list-item-indent indent))))
                  (when (string-match
                         "^[ \t]*\\(?:name\\|role\\):[ \t]*\\([^#\n]+\\)"
                         line)
                    (when (or (null list-item-indent)
                              (= indent list-item-indent))
                      (funcall add-link list-kind
                               (ansible-intelligence-yaml--target-text
                                (string-trim (match-string 1 line)))
                               (+ line-start (match-beginning 1))
                               (+ line-start (match-end 1)))
                      (setq list-item-indent indent))))
              (progn
                (when (null list-item-indent)
                  (setq list-item-indent indent))
                (when (and (memq list-kind
                                 '(import-tasks include-tasks include-vars
                                   vars-files))
                           (= indent list-item-indent)
                           (string-match
                            "^[ \t]*\\(?:-[ \t]*\\)?file:[ \t]*\\([^#\n]+\\)"
                            line))
                  (let* ((target (string-trim (match-string 1 line)))
                         (target-start
                          (+ line-start (match-beginning 1)
                             (or (string-match (regexp-quote target)
                                               (match-string 1 line))
                                 0))))
                    (funcall add-link list-kind target target-start
                             (+ target-start (length target)))))
                (when (string-match
                       "^[ \t]*-[ \t]*\\([^#\n]+\\)"
                       line)
                  (let ((raw (string-trim (match-string 1 line))))
                    (unless (string-match-p
                             "\\`\\(?:file\\|name\\|role\\):[ \t]*"
                             raw)
                      (funcall add-link list-kind
                               (ansible-intelligence-yaml--target-text raw)
                               (+ line-start (match-beginning 1))
                               (+ line-start (match-end 1)))))))))))
          (forward-line 1))
      (nreverse links))))

(defun ansible-intelligence-yaml-fallback-extract (&optional kind)
  "Return defensive structural facts for KIND."
  (list :available t :source 'scanner :pairs nil
        :links (ansible-intelligence-yaml--fallback-links)
        :outline (ansible-intelligence-yaml--fallback-outline kind)))

(defun ansible-intelligence-yaml-query-extract (&optional _tree kind)
  "Extract structural facts, using defensive scanning."
  (ansible-intelligence-yaml-fallback-extract kind))

(provide 'ansible-intelligence-yaml)

;;; ansible-intelligence-yaml.el ends here
