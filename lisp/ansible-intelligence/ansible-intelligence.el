;;; ansible-intelligence.el --- Local semantic intelligence for Ansible -*- lexical-binding: t; -*-

;;; Commentary:
;; Completion and navigation use the last completed immutable document snapshot.

;;; Code:

(require 'ansible-intelligence-index)
(require 'ansible-intelligence-native)
(require 'xref)

(defcustom ansible-intelligence-analysis-idle-delay 0.15
  "Seconds to wait before parsing an edited buffer."
  :type 'number
  :group 'ansible-intelligence)

(defvar-local ansible-intelligence--document nil)
(defvar-local ansible-intelligence--analysis-timer nil)
(defvar-local ansible-intelligence--jinja-cache nil)
(defvar-local ansible-intelligence--analysis-generation 0)
(defvar ansible-intelligence-mode)

(defun ansible-intelligence--analyze (&optional expected-tick expected-generation)
  "Replace the current buffer snapshot when EXPECTED state is still current.

The parser is intentionally run only from this debounced analysis path or an
explicit navigation command, never from completion."
  (let* ((tick (or expected-tick (buffer-chars-modified-tick)))
         (generation
          (or expected-generation
              (setq ansible-intelligence--analysis-generation
                    (1+ ansible-intelligence--analysis-generation)))))
    (setq ansible-intelligence--analysis-timer nil)
    (unless ansible-intelligence--jinja-cache
      (setq ansible-intelligence--jinja-cache (make-hash-table :test #'equal)))
    (let ((document
           (ansible-intelligence-parse-buffer
            nil ansible-intelligence--jinja-cache)))
      (when (and ansible-intelligence-mode
                 (= generation ansible-intelligence--analysis-generation)
                 (= tick (buffer-chars-modified-tick)))
        (setq ansible-intelligence--document document)
        (when (and ansible-intelligence--project-index buffer-file-name)
          (ansible-intelligence-index-put-document
           ansible-intelligence--project-index document))
        (when (fboundp 'ansible-intelligence-native--analysis-published)
          (ansible-intelligence-native--analysis-published))))))

(defun ansible-intelligence--schedule-analysis (&rest _)
  "Schedule analysis without making editing or completion wait."
  (when (timerp ansible-intelligence--analysis-timer)
    (cancel-timer ansible-intelligence--analysis-timer))
  (setq ansible-intelligence--analysis-generation
        (1+ ansible-intelligence--analysis-generation))
  (setq ansible-intelligence--analysis-timer
        (run-with-idle-timer
         ansible-intelligence-analysis-idle-delay nil
         (lambda (buffer tick generation)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and ansible-intelligence-mode
                          (= generation ansible-intelligence--analysis-generation)
                          (= tick (buffer-chars-modified-tick)))
                 (ansible-intelligence--analyze tick generation)))))
         (current-buffer) (buffer-chars-modified-tick)
         ansible-intelligence--analysis-generation)))

(defun ansible-intelligence--local-visible-symbols (position)
  "Return current-document symbols visible at POSITION."
  (seq-filter
   (lambda (symbol)
     (and (<= (ansible-intelligence-symbol-visibility-start symbol) position)
          (or (null (ansible-intelligence-symbol-visibility-end symbol))
              (<= position (ansible-intelligence-symbol-visibility-end symbol)))))
   (and ansible-intelligence--document
        (ansible-intelligence-document-symbols ansible-intelligence--document))))

(defun ansible-intelligence--visible-symbols (position)
  "Return local and statically related symbols visible at POSITION."
  (let ((symbols (ansible-intelligence--local-visible-symbols position))
        external
        (seen (make-hash-table :test #'equal)))
    (dolist (symbol symbols)
      (puthash (ansible-intelligence-symbol-name symbol) t seen))
    (when (and ansible-intelligence--project-index ansible-intelligence--document)
      (dolist (document
               (append
                (ansible-intelligence-index-related-documents
                 ansible-intelligence--project-index
                 ansible-intelligence--document position)
                (ansible-intelligence-index-project-variable-documents
                 ansible-intelligence--project-index)))
        (unless (ansible-intelligence--same-file-p
                 (ansible-intelligence-document-path document) buffer-file-name)
          (dolist (symbol (ansible-intelligence-document-symbols document))
            (unless (gethash (ansible-intelligence-symbol-name symbol) seen)
              (puthash (ansible-intelligence-symbol-name symbol) t seen)
              (push symbol external))))))
    (append symbols (nreverse external))))

(defun ansible-intelligence--bounds ()
  "Return bounds of the complete dotted expression at point."
  (save-excursion
    (skip-chars-backward "[:alnum:]_.")
    (let ((start (point)))
      (skip-chars-forward "[:alnum:]_.")
      (cons start (point)))))

(defun ansible-intelligence--candidate (symbol)
  "Build a completion candidate for SYMBOL."
  (propertize (ansible-intelligence-symbol-name symbol)
              'annotation (format "  [%s]" (ansible-intelligence-symbol-kind symbol))
              'company-kind 'variable
              'documentation
              (ansible-intelligence-native-symbol-documentation symbol)))

(defun ansible-intelligence--annotation (candidate)
  "Return completion annotation stored on CANDIDATE."
  (get-text-property 0 'annotation candidate))

(defun ansible-intelligence--field-children (field)
  "Return nested fields for FIELD, or nil for a leaf or legacy field."
  (let ((metadata (cdr field)))
    (when (and (consp metadata) (keywordp (car metadata)))
      (plist-get metadata :fields))))

(defun ansible-intelligence--field-documentation (field)
  "Return documentation for FIELD, supporting legacy result-field alists."
  (let ((metadata (cdr field)))
    (if (and (consp metadata) (keywordp (car metadata)))
        (plist-get metadata :documentation)
      metadata)))

(defun ansible-intelligence--field-annotation (field)
  "Return the annotation for FIELD, supporting legacy result-field alists."
  (let ((metadata (cdr field)))
    (if (and (consp metadata) (keywordp (car metadata)))
        (or (plist-get metadata :annotation) "  [inferred field]")
      "  [result field]")))

(defun ansible-intelligence--fields-for-path (symbol path)
  "Return fields below PATH in SYMBOL's nested field shape."
  (let ((fields (ansible-intelligence-symbol-fields symbol)))
    (dolist (part path fields)
      (when fields
        (setq fields
              (ansible-intelligence--field-children
               (seq-find (lambda (field) (string= part (car field))) fields)))))))

(defun ansible-intelligence--field-candidate (field symbol path)
  "Build a completion candidate for FIELD below SYMBOL at PATH."
  (propertize (car field)
              'annotation (ansible-intelligence--field-annotation field)
              'documentation
              (string-join
               (delq nil
                     (list
                      (ansible-intelligence--field-documentation field)
                      (ansible-intelligence-native-symbol-documentation
                       symbol (append path (list (car field))))))
               "\n\n")))

(defun ansible-intelligence--completion-doc-buffer (candidate)
  "Return a documentation buffer for completion CANDIDATE."
  (when-let* ((documentation (get-text-property 0 'documentation candidate)))
    (with-current-buffer
        (get-buffer-create " *ansible-intelligence-completion-documentation*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert documentation)
        (goto-char (point-min))
        (special-mode))
      (current-buffer))))

(defun ansible-intelligence--jinja-field-completion (context)
  "Return field completion for snapshot-only Jinja CONTEXT."
  (when (and context (plist-get context :field-p))
    (let* ((base (plist-get context :base))
           (path (plist-get context :path))
           (start (plist-get context :start))
           (end (plist-get context :end))
           (symbol
            (seq-find
             (lambda (item)
               (string= base (ansible-intelligence-symbol-name item)))
             (ansible-intelligence--visible-symbols start)))
           (fields (and symbol
                        (ansible-intelligence--fields-for-path symbol path))))
      (when fields
        (list start end
              (mapcar
               (lambda (field)
                 (ansible-intelligence--field-candidate field symbol path))
               fields)
              :exclusive 'no
              :annotation-function #'ansible-intelligence--annotation
              :company-doc-buffer
              #'ansible-intelligence--completion-doc-buffer)))))

(defun ansible-intelligence-completion-at-point ()
  "Return non-exclusive semantic completion from the latest snapshot."
  (when (and ansible-intelligence-mode ansible-intelligence--document
             (ansible-intelligence-native-capf-owned-p))
    (or
     (ansible-intelligence--jinja-field-completion
      (ansible-intelligence-jinja-completion-context
       (ansible-intelligence-document-jinja ansible-intelligence--document)
       (point)))
     (pcase-let* ((`(,start . ,end) (ansible-intelligence--bounds))
                  (text (buffer-substring-no-properties start end))
                  (parts (split-string text "\\.")))
       (if (> (length parts) 1)
           (when-let* ((symbol (seq-find (lambda (item) (string= (car parts)
                                                                   (ansible-intelligence-symbol-name item)))
                                         (ansible-intelligence--visible-symbols start)))
                       (fields (ansible-intelligence--fields-for-path
                                symbol (butlast (cdr parts)))))
             (string-match "\\.[^.]*\\'" text)
             (list (+ start (match-beginning 0) 1) end
                   (mapcar
                    (lambda (field)
                      (ansible-intelligence--field-candidate
                       field symbol (butlast (cdr parts))))
                    fields)
                   :exclusive 'no
                   :annotation-function #'ansible-intelligence--annotation
                   :company-doc-buffer
                   #'ansible-intelligence--completion-doc-buffer))
         (list start end
               (mapcar #'ansible-intelligence--candidate
                       (ansible-intelligence--visible-symbols start))
               :exclusive 'no
               :annotation-function #'ansible-intelligence--annotation
               :company-doc-buffer
               #'ansible-intelligence--completion-doc-buffer))))))

(defun ansible-intelligence--name-at-point ()
  "Return the base identifier of the dotted expression at point."
  (or (ansible-intelligence-jinja-root-name-at-point
       (and ansible-intelligence--document
            (ansible-intelligence-document-jinja ansible-intelligence--document))
       (point))
      (let* ((bounds (ansible-intelligence--bounds))
             (name (car (split-string (buffer-substring-no-properties
                                       (car bounds) (cdr bounds)) "\\."))))
        (and name
             (string-match-p (concat "\\`" ansible-intelligence--identifier-re "\\'") name)
             name))))

(defun ansible-intelligence--symbol-at-point ()
  "Return the visible symbol whose name occurs at point."
  (when-let* ((name (ansible-intelligence--name-at-point)))
      ;; A deferred reparse can leave a snapshot one edit behind.  Definitions
      ;; remain safe to navigate when the identifier has exactly one source.
      (let* ((definition
              (seq-find
               (lambda (symbol)
                 (let ((range (ansible-intelligence-symbol-range symbol)))
                   (and (string= name (ansible-intelligence-symbol-name symbol))
                        (<= (ansible-intelligence-range-start range) (point))
                        (<= (point) (ansible-intelligence-range-end range)))))
               (and ansible-intelligence--document
                    (ansible-intelligence-document-symbols
                     ansible-intelligence--document))))
             (symbols (ansible-intelligence--visible-symbols (point)))
             (matches (seq-filter (lambda (symbol)
                                    (string= name (ansible-intelligence-symbol-name symbol)))
                                  symbols)))
        (or definition
            (and (= (length matches) 1) (car matches))))))

(defun ansible-intelligence--source-position (file offset &optional snapshot)
  "Return an xref location for FILE at character OFFSET."
  (ansible-intelligence-native--source-position file offset snapshot))

(defun ansible-intelligence--symbol-xref (symbol &optional snapshot)
  "Return an xref for SYMBOL."
  (let* ((file (or (ansible-intelligence-symbol-file symbol) buffer-file-name))
         (range (ansible-intelligence-symbol-range symbol))
         (snapshot (or snapshot
                       (ansible-intelligence-native--snapshot-for-file file)))
         (location
          (cond
           ((and file (file-exists-p file))
              (ansible-intelligence--source-position
               file (ansible-intelligence-range-start range) snapshot))
           (t
            (xref-make-buffer-location
             (current-buffer) (ansible-intelligence-range-start range))))))
    (when location
      (xref-make
       (format "%s [%s]" (ansible-intelligence-symbol-name symbol)
               (ansible-intelligence-symbol-kind symbol))
       location))))

(defun ansible-intelligence--link-at-point ()
  "Return the static include, vars, or role link at point."
  (and ansible-intelligence--document
       (seq-find
        (lambda (link)
          (let ((range (ansible-intelligence-link-range link)))
            (and (<= (ansible-intelligence-range-start range) (point))
                 (<= (point) (ansible-intelligence-range-end range)))))
        (ansible-intelligence-document-links ansible-intelligence--document))))

(defun ansible-intelligence--link-xrefs (link)
  "Return file xrefs for resolved static LINK."
  (when ansible-intelligence--project-index
    (mapcar
     (lambda (file)
       (xref-make
        (format "%s [%s]" (ansible-intelligence-link-target link)
                (ansible-intelligence-link-kind link))
        (xref-make-file-location file 1 0)))
     (ansible-intelligence--resolve-links
      ansible-intelligence--document link
      (ansible-intelligence-project-index-root
       ansible-intelligence--project-index)))))

(defun ansible-intelligence-find-definition ()
  "Jump to a variable definition or static include target at point."
  (interactive)
  (unless (and ansible-intelligence--document
               (numberp (ansible-intelligence-document-tick
                         ansible-intelligence--document))
               (= (ansible-intelligence-document-tick
                   ansible-intelligence--document)
                  (buffer-chars-modified-tick)))
    (ansible-intelligence--analyze))
  (let* ((link (ansible-intelligence--link-at-point))
         (link-xrefs (and link (ansible-intelligence--link-xrefs link)))
         (name (ansible-intelligence--name-at-point)))
    ;; Navigation is an explicit command, unlike CAPF.  Refreshing here makes
    ;; it reliable immediately after an edit, before the idle timer runs.
    (unless (ansible-intelligence--symbol-at-point)
      (ansible-intelligence--analyze)
      (setq name (ansible-intelligence--name-at-point)))
    (let ((definitions
           (if (and name ansible-intelligence--project-index)
               (ansible-intelligence-index-visible-definitions
                ansible-intelligence--project-index
                ansible-intelligence--document name (point))
             (and-let* ((symbol (ansible-intelligence--symbol-at-point)))
               (list symbol)))))
      (cond
       ((= (length link-xrefs) 1)
        (xref-push-marker-stack)
        (xref--show-location (xref-item-location (car link-xrefs)) nil))
       ((> (length link-xrefs) 1)
        (xref--show-xrefs (lambda () link-xrefs) nil))
       (t
        (pcase definitions
        ('nil (user-error "No Ansible definition at point"))
        (`(,definition)
          (let ((xref (ansible-intelligence--symbol-xref definition)))
            (if xref
                (progn
                  (xref-push-marker-stack)
                  (xref--show-location (xref-item-location xref) nil))
              (user-error "Ansible definition snapshot is stale; reindex required"))))
        (_
          (let ((xrefs (delq nil
                             (mapcar #'ansible-intelligence--symbol-xref
                                     definitions))))
            (if xrefs
                (xref--show-xrefs (lambda () xrefs) nil)
              (user-error "Ansible definition snapshots are stale; reindex required"))))))))))

(defun ansible-intelligence-describe-at-point ()
  "Display source, scope, fields, and confidence for the symbol at point."
  (interactive)
  (if-let* ((symbol (ansible-intelligence--symbol-at-point)))
      (message "%s: %s; %s; confidence %s%s%s"
               (ansible-intelligence-symbol-name symbol)
               (ansible-intelligence-symbol-kind symbol)
               (ansible-intelligence-symbol-source symbol)
               (ansible-intelligence-symbol-confidence symbol)
               (if (ansible-intelligence-symbol-fields symbol) "; generic result fields available" "")
               (if (and ansible-intelligence--document
                        (ansible-intelligence-document-jinja
                         ansible-intelligence--document))
                   (format "; Jinja regions %d%s"
                           (ansible-intelligence-jinja-snapshot-region-count
                            (ansible-intelligence-document-jinja
                             ansible-intelligence--document))
                           (if (ansible-intelligence-jinja-snapshot-incomplete
                                (ansible-intelligence-document-jinja
                                 ansible-intelligence--document))
                               " (partial)" ""))
                 ""))
    (user-error "No Ansible symbol at point")))

(defun ansible-intelligence-show-scope ()
  "Show visible symbols and their provenance in a help buffer."
  (interactive)
  (with-help-window "*ansible-intelligence-scope*"
    (princ (format "Ansible scope at line %d\n" (line-number-at-pos)))
    (when-let* ((jinja (and ansible-intelligence--document
                            (ansible-intelligence-document-jinja
                             ansible-intelligence--document))))
      (princ
       (format "Jinja: %s parser, %d regions, %d cache hits, %d misses%s\n\n"
               (ansible-intelligence-jinja-snapshot-parser jinja)
               (ansible-intelligence-jinja-snapshot-region-count jinja)
               (ansible-intelligence-jinja-snapshot-cache-hits jinja)
               (ansible-intelligence-jinja-snapshot-cache-misses jinja)
               (if (ansible-intelligence-jinja-snapshot-incomplete jinja)
                   " (partial)" ""))))
    (princ "\n")
    (dolist (symbol (ansible-intelligence--visible-symbols (point)))
      (princ (format "%-24s %-12s %s (%s)%s\n"
                     (ansible-intelligence-symbol-name symbol)
                     (ansible-intelligence-symbol-kind symbol)
                     (ansible-intelligence-symbol-source symbol)
                     (ansible-intelligence-symbol-confidence symbol)
                     (if-let* ((file (ansible-intelligence-symbol-file symbol)))
                         (format " — %s" (file-relative-name
                                          file
                                          (if ansible-intelligence--project-index
                                              (ansible-intelligence-project-index-root
                                               ansible-intelligence--project-index)
                                            default-directory)))
                       ""))))))

(defun ansible-intelligence-show-index-status ()
  "Display project indexing state and timing."
  (interactive)
  (unless ansible-intelligence--project-index
    (user-error "This buffer is not attached to an Ansible project index"))
  (let* ((index ansible-intelligence--project-index)
         (started (ansible-intelligence-project-index-started-at index))
         (completed (ansible-intelligence-project-index-completed-at index)))
    (message "Ansible index %s: %d/%d files%s%s"
             (ansible-intelligence-project-index-status index)
             (hash-table-count
              (ansible-intelligence-project-index-documents index))
             (length (ansible-intelligence-project-index-files index))
             (if (and started completed)
                 (format " in %.3fs" (- completed started))
               "")
             (if-let* ((jinja (and ansible-intelligence--document
                                   (ansible-intelligence-document-jinja
                                    ansible-intelligence--document))))
                 (format "; Jinja %d regions%s"
                         (ansible-intelligence-jinja-snapshot-region-count jinja)
                         (if (ansible-intelligence-jinja-snapshot-incomplete jinja)
                             " partial" ""))
               ""))))

(defun ansible-intelligence-reindex-project ()
  "Asynchronously rebuild the current Ansible project index."
  (interactive)
  (ansible-intelligence--analyze)
  (ansible-intelligence-index-start
   (or ansible-intelligence--project-index
       (ansible-intelligence-index-attach)))
  (message "Ansible Intelligence project reindex started"))

(defun ansible-intelligence-explain-diagnostic ()
  "Explain parser diagnostics affecting the current Ansible buffer."
  (interactive)
  (if (and (fboundp 'ansible-intelligence-native-explain-diagnostic)
           (ansible-intelligence-native-explain-diagnostic))
      nil
    (let ((errors (and ansible-intelligence--document
                       (ansible-intelligence-document-errors ansible-intelligence--document))))
      (if errors
          (message "%s" (string-join errors " "))
        (message "No Ansible Intelligence diagnostics at point")))))

(defun ansible-intelligence-find-references ()
  "Show project-wide indexed Jinja references to the symbol at point."
  (interactive)
  (let ((name (ansible-intelligence--name-at-point)))
    (unless name (user-error "No Ansible symbol at point"))
    (let ((matches
           (if ansible-intelligence--project-index
               (ansible-intelligence-index-references
                ansible-intelligence--project-index name)
             (seq-filter
              (lambda (reference)
                (string= name (ansible-intelligence-reference-name reference)))
              (ansible-intelligence-document-references ansible-intelligence--document)))))
      (unless matches
        (user-error "No indexed references to %s" name))
      (xref--show-xrefs
       (lambda ()
         (delq nil
               (mapcar
                (lambda (reference)
                  (let* ((file (ansible-intelligence-reference-file reference))
                         (range (ansible-intelligence-reference-range reference))
                         (location
                          (if (and file (file-exists-p file))
                              (ansible-intelligence--source-position
                               file (ansible-intelligence-range-start range)
                               (ansible-intelligence-native--snapshot-for-file
                                file))
                            (xref-make-buffer-location
                             (current-buffer)
                             (ansible-intelligence-range-start range)))))
                    (when location
                      (xref-make name location))))
                matches)))
       nil))))

(defun ansible-intelligence-rename ()
  "Refuse rename until project-wide scope analysis is available."
  (interactive)
  (user-error "Rename requires project-wide indexing and is not enabled"))

;;;###autoload
(define-minor-mode ansible-intelligence-mode
  "Provide conservative local Ansible semantic completion and navigation."
  :lighter " AI"
  (if ansible-intelligence-mode
      (progn
        (unless ansible-intelligence--jinja-cache
          (setq ansible-intelligence--jinja-cache
                (make-hash-table :test #'equal)))
        (when buffer-file-name
          (ansible-intelligence-index-attach))
        (ansible-intelligence--analyze)
        (add-hook 'after-change-functions #'ansible-intelligence--schedule-analysis nil t)
        ;; A positive depth keeps Eglot and other primary providers ahead of
        ;; this deliberately non-exclusive semantic supplement.
        (add-hook 'completion-at-point-functions
                  #'ansible-intelligence-completion-at-point 90 t)
        (ansible-intelligence-native-enable))
    (remove-hook 'after-change-functions #'ansible-intelligence--schedule-analysis t)
    (remove-hook 'completion-at-point-functions #'ansible-intelligence-completion-at-point t)
    (ansible-intelligence-native-disable)
    (when (timerp ansible-intelligence--analysis-timer)
      (cancel-timer ansible-intelligence--analysis-timer))
    (setq ansible-intelligence--analysis-timer nil
          ansible-intelligence--analysis-generation
          (1+ ansible-intelligence--analysis-generation))))

(defun ansible-intelligence-enable-maybe ()
  "Enable semantic support in YAML buffers identified as Ansible."
  (when (and (derived-mode-p 'yaml-ts-mode)
             (or (memq (ansible-intelligence-detect-document-kind)
                       '(role-default role-var role-task inventory-var vars-file))
                 (and (fboundp 'adam/ansible-buffer-p) (adam/ansible-buffer-p))
                 (save-excursion
                   (goto-char (point-min))
                   (re-search-forward "^[[:space:]-]*\\(?:hosts\\|tasks\\|roles\\):" (min (+ (point-min) 20000) (point-max)) t))))
    (ansible-intelligence-mode 1)))

(add-hook 'yaml-ts-mode-hook #'ansible-intelligence-enable-maybe)

(provide 'ansible-intelligence)

;;; ansible-intelligence.el ends here
