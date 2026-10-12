;;; ansible-intelligence-native.el --- Native Emacs integration adapters -*- lexical-binding: t; -*-

;;; Commentary:
;; This module is the routing boundary between immutable Ansible snapshots and
;; native Emacs interfaces (xref and imenu).  Adapters never parse YAML/Jinja or
;; discover project files synchronously; ownership is decided from the current
;; published document and in-memory project index.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'imenu)
(require 'ansible-intelligence-model)
(require 'ansible-intelligence-jinja)
(require 'ansible-intelligence-index)

(declare-function ansible-intelligence--visible-symbols
                  "ansible-intelligence")
(declare-function ansible-intelligence--resolve-links
                  "ansible-intelligence-index")
(declare-function ansible-intelligence--live-document-buffer
                  "ansible-intelligence-index")
(declare-function ansible-intelligence-index-document-current-p
                  "ansible-intelligence-index")
(declare-function ansible-intelligence-index-start
                  "ansible-intelligence-index")

(defvar-local ansible-intelligence-native--imenu-previous nil)

(defun ansible-intelligence-native--document ()
  "Return the current immutable document snapshot."
  (and (boundp 'ansible-intelligence--document)
       ansible-intelligence--document))

(defun ansible-intelligence-native--index ()
  "Return the attached in-memory project index."
  (and (boundp 'ansible-intelligence--project-index)
       ansible-intelligence--project-index))

(defun ansible-intelligence-native--position-in-range-p (position range)
  "Return non-nil when POSITION is within RANGE, including its end."
  (and range
       (<= (ansible-intelligence-range-start range) position)
       (<= position (ansible-intelligence-range-end range))))

(defun ansible-intelligence-native--reference-at-point (document position)
  "Return a Jinja reference in DOCUMENT at POSITION."
  (seq-find
   (lambda (reference)
     (or (ansible-intelligence-native--position-in-range-p
          position (ansible-intelligence-reference-range reference))
         (seq-some
          (lambda (range)
            (ansible-intelligence-native--position-in-range-p position range))
          (ansible-intelligence-reference-path-ranges reference))))
   (ansible-intelligence-document-references document)))

(defun ansible-intelligence-native--symbol-at-point (document position)
  "Return a definition symbol in DOCUMENT at POSITION."
  (seq-find
   (lambda (symbol)
     (ansible-intelligence-native--position-in-range-p
      position (ansible-intelligence-symbol-range symbol)))
   (ansible-intelligence-document-symbols document)))

(defun ansible-intelligence-native--link-at-point (document position)
  "Return a static link in DOCUMENT at POSITION."
  (seq-find
   (lambda (link)
     (and (eq (ansible-intelligence-link-confidence link) 'static)
          (ansible-intelligence-native--position-in-range-p
           position (ansible-intelligence-link-range link))))
   (ansible-intelligence-document-links document)))

(defun ansible-intelligence-native-context-at-point (&optional position)
  "Return the centralized local/Eglot ownership context at POSITION.

The result is nil outside local Ansible/Jinja ownership.  It contains
`:kind', `:name', `:path', and the relevant snapshot object when available."
  (let* ((position (or position (point)))
         (document (ansible-intelligence-native--document)))
    (when document
      (or
       (when-let* ((link (ansible-intelligence-native--link-at-point
                          document position)))
         (list :owned t :kind 'static-link
               :name (ansible-intelligence-link-target link)
               :link link :document document))
       (when-let* ((reference
                    (ansible-intelligence-native--reference-at-point
                     document position)))
         (list :owned t :kind 'variable
               :name (ansible-intelligence-reference-name reference)
               :path (ansible-intelligence-reference-path reference)
               :reference reference
               :region
               (seq-find
                (lambda (region)
                  (or
                   (ansible-intelligence-native--position-in-range-p
                    position (ansible-intelligence-jinja-region-range region))
                   (ansible-intelligence-native--position-in-range-p
                    position
                    (ansible-intelligence-jinja-region-content-range
                     region))))
                (ansible-intelligence-jinja-snapshot-regions
                 (ansible-intelligence-document-jinja document)))
               :document document))
       (when-let* ((symbol (ansible-intelligence-native--symbol-at-point
                            document position)))
         (list :owned t :kind 'definition
               :name (ansible-intelligence-symbol-name symbol)
               :symbol symbol :document document))))))

(defun ansible-intelligence-native--current-symbols (name &optional position)
  "Return visible snapshot symbols named NAME at POSITION."
  (let ((document (ansible-intelligence-native--document))
        (position (or position (point))))
    (seq-filter
     (lambda (symbol)
       (and (string= name (ansible-intelligence-symbol-name symbol))
            (or (null document)
                (and (<= (ansible-intelligence-symbol-visibility-start symbol)
                         position)
                     (or (null (ansible-intelligence-symbol-visibility-end symbol))
                         (<= position
                             (ansible-intelligence-symbol-visibility-end
                              symbol)))))))
     (or (and (fboundp 'ansible-intelligence--visible-symbols)
              (ansible-intelligence--visible-symbols position))
         (and document (ansible-intelligence-document-symbols document))
         nil))))

(defun ansible-intelligence-native--live-snapshot-current-p
    (buffer snapshot)
  "Return non-nil when SNAPSHOT matches BUFFER's current published tick."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((document (ansible-intelligence-native--document)))
           (and document
                (eq document snapshot)
                (numberp (ansible-intelligence-document-tick document))
                (= (ansible-intelligence-document-tick document)
                   (buffer-chars-modified-tick)))))))

(defun ansible-intelligence-native--disk-snapshot-current-p
    (file snapshot)
  "Return non-nil when SNAPSHOT still matches FILE on disk."
  (let ((identity (and snapshot
                       (ansible-intelligence-document-source-identity
                        snapshot))))
    (and file snapshot
         (file-regular-p file)
         identity
         (not (ansible-intelligence-source-identity-live-p identity))
         (ansible-intelligence-source-identity-equal-p
          identity (ansible-intelligence-source-identity-for-file file)))))

(defun ansible-intelligence-native--recommend-reindex (file)
  "Invalidate FILE's indexed entry and schedule a project refresh."
  (let ((index (ansible-intelligence-native--index)))
    (when index
      (remhash (ansible-intelligence--canonical-path file)
               (ansible-intelligence-project-index-documents index))
      (unless (memq (ansible-intelligence-project-index-status index)
                    '(discovering indexing))
        (ansible-intelligence-index-start index))))
  (message "Ansible Intelligence: source changed for %s; reindex scheduled"
           (or (and file (file-name-nondirectory file)) file)))

(defun ansible-intelligence-native--recommend-stale-index-result (name)
  "Recommend reindexing when NAME is supplied by a stale indexed document."
  (when-let* ((index (ansible-intelligence-native--index))
              (documents
               (ansible-intelligence-project-index-documents index))
              (stale-file
               (catch 'stale
                 (maphash
                  (lambda (_path document)
                    (when (and (ansible-intelligence-document-path document)
                               (not (ansible-intelligence-index-document-current-p
                                     document))
                               (seq-some
                                (lambda (symbol)
                                  (string= name
                                           (ansible-intelligence-symbol-name
                                            symbol)))
                                (ansible-intelligence-document-symbols
                                 document)))
                      (throw 'stale
                             (ansible-intelligence-document-path document))))
                  documents)
                 nil)))
    (ansible-intelligence-native--recommend-reindex stale-file)))

(defun ansible-intelligence-native--snapshot-for-file
    (file &optional preferred)
  "Return a current published snapshot available for FILE.

Stale live buffers are deliberately not replaced by a disk parse.  Closed
file snapshots are accepted only while their source identity matches disk."
  (let* ((live (and file (ansible-intelligence--live-document-buffer file)))
         (live-document
          (and live
               (with-current-buffer live
                 (ansible-intelligence-native--document))))
         (live-current
          (and live live-document
               (ansible-intelligence-native--live-snapshot-current-p
                live live-document))))
    (cond
     (live
      (and live-current live-document))
     (t
      (let ((candidate
             (or
              (and preferred
                   (ansible-intelligence--same-file-p
                    file (ansible-intelligence-document-path preferred))
                   preferred)
              (and file
                   (ansible-intelligence-native--index)
                   (ansible-intelligence-index-current-document
                    (ansible-intelligence-native--index) file)))))
        (if (and candidate
                 (ansible-intelligence-native--disk-snapshot-current-p
                  file candidate))
            candidate
          (when candidate
            (ansible-intelligence-native--recommend-reindex file))
          nil))))))

(defun ansible-intelligence-native--source-position
    (file offset &optional snapshot)
  "Return an xref location for FILE at character OFFSET.

Visiting buffers are used only when SNAPSHOT is the currently published
document for that buffer; otherwise the indexed offset is resolved in FILE."
  (let ((live (and file (ansible-intelligence--live-document-buffer file))))
    (cond
     ((null file)
      (xref-make-buffer-location (current-buffer)
                                 (min offset (point-max))))
     ((and live snapshot
           (ansible-intelligence-native--live-snapshot-current-p
            live snapshot))
      (with-current-buffer live
        (xref-make-buffer-location
         live (min offset (point-max)))))
     ((and (null live)
           (ansible-intelligence-native--disk-snapshot-current-p file snapshot))
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (min offset (point-max)))
            (xref-make-file-location file (line-number-at-pos) (current-column)))
        (error nil)))
     (t
      (when (and file snapshot)
        (ansible-intelligence-native--recommend-reindex file))
      nil))))

(defun ansible-intelligence-native--reference-for-snapshot
    (reference snapshot source-document)
  "Return REFERENCE's ordinal match in SNAPSHOT when available."
  (if (or (null snapshot)
          (null source-document)
          (not (ansible-intelligence--same-file-p
                (ansible-intelligence-reference-file reference)
                (ansible-intelligence-document-path snapshot))))
      reference
    (let* ((key-name (ansible-intelligence-reference-name reference))
           (key-path (ansible-intelligence-reference-path reference))
           (ordinal
            (let ((index 0)
                  result)
              (dolist (candidate
                       (ansible-intelligence-document-references
                        source-document))
                (when (and (string= key-name
                                    (ansible-intelligence-reference-name
                                     candidate))
                           (equal key-path
                                  (ansible-intelligence-reference-path
                                   candidate)))
                  (if (eq candidate reference)
                      (setq result index)
                    (setq index (1+ index)))))
              result))
           (candidates
            (seq-filter
             (lambda (candidate)
               (and (string= key-name
                             (ansible-intelligence-reference-name candidate))
                    (equal key-path
                           (ansible-intelligence-reference-path candidate))))
             (ansible-intelligence-document-references snapshot))))
      (or (and ordinal (nth ordinal candidates)) reference))))

(defun ansible-intelligence-native--symbol-for-snapshot
    (symbol snapshot source-document)
  "Return SYMBOL's ordinal match in SNAPSHOT when available."
  (if (or (null snapshot)
          (null source-document)
          (not (ansible-intelligence--same-file-p
                (ansible-intelligence-symbol-file symbol)
                (ansible-intelligence-document-path snapshot))))
      symbol
    (let* ((name (ansible-intelligence-symbol-name symbol))
           (kind (ansible-intelligence-symbol-kind symbol))
           (ordinal
            (let ((index 0)
                  result)
              (dolist (candidate
                       (ansible-intelligence-document-symbols
                        source-document))
                (when (and (string= name
                                    (ansible-intelligence-symbol-name
                                     candidate))
                           (eq kind
                               (ansible-intelligence-symbol-kind candidate)))
                  (if (eq candidate symbol)
                      (setq result index)
                    (setq index (1+ index)))))
              result))
           (candidates
            (seq-filter
             (lambda (candidate)
               (and (string= name
                             (ansible-intelligence-symbol-name candidate))
                    (eq kind (ansible-intelligence-symbol-kind candidate))))
             (ansible-intelligence-document-symbols snapshot))))
      (or (and ordinal (nth ordinal candidates)) symbol))))

(defun ansible-intelligence-native--symbol-xref (symbol &optional snapshot)
  "Build an xref item for SYMBOL."
  (let* ((file (or (ansible-intelligence-symbol-file symbol)
                   buffer-file-name))
         (snapshot (or snapshot
                       (ansible-intelligence-native--snapshot-for-file file)))
         (source-document
          (or (and (ansible-intelligence-native--index)
                   (ansible-intelligence-index-current-document
                    (ansible-intelligence-native--index) file))
              (let ((document (ansible-intelligence-native--document)))
                (and document
                     (ansible-intelligence--same-file-p
                      file (ansible-intelligence-document-path document))
                     document))))
         (live-snapshot
          (ansible-intelligence-native--snapshot-for-file file snapshot))
         (symbol (ansible-intelligence-native--symbol-for-snapshot
                  symbol live-snapshot source-document))
         (range (ansible-intelligence-symbol-range symbol)))
    (when-let* ((location
                 (ansible-intelligence-native--source-position
                  file (ansible-intelligence-range-start range)
                  live-snapshot)))
      (xref-make
       (format "%s [%s]" (ansible-intelligence-symbol-name symbol)
               (ansible-intelligence-symbol-kind symbol))
       location))))

(defun ansible-intelligence-native--link-xrefs (link document)
  "Return xrefs for static LINK from DOCUMENT."
  (let* ((index (ansible-intelligence-native--index))
         (root (and index
                    (ansible-intelligence-project-index-root index))))
    (when (and index root (eq (ansible-intelligence-link-confidence link)
                              'static))
      (mapcar
       (lambda (file)
         (xref-make
          (format "%s [%s]" (ansible-intelligence-link-target link)
                  (ansible-intelligence-link-kind link))
          (xref-make-file-location file 1 0)))
       (ansible-intelligence--resolve-links document link root)))))

(defun ansible-intelligence-native--definitions (name context)
  "Return xrefs for NAME according to local CONTEXT."
  (let ((document (plist-get context :document))
        (link (plist-get context :link))
        (index (ansible-intelligence-native--index)))
    (when (and index name)
      (ansible-intelligence-native--recommend-stale-index-result name))
    (cond
     (link
      (ansible-intelligence-native--link-xrefs link document))
     ((and index document)
      (delq nil
            (mapcar
             (lambda (symbol)
               (ansible-intelligence-native--symbol-xref
                symbol
                (ansible-intelligence-native--snapshot-for-file
                 (ansible-intelligence-symbol-file symbol) document)))
             (ansible-intelligence-index-visible-definitions
              index document name (point)))))
     (document
      (delq nil
            (mapcar (lambda (symbol)
                      (ansible-intelligence-native--symbol-xref
                       symbol document))
                    (ansible-intelligence-native--current-symbols name))))
     (t nil))))

(defun ansible-intelligence-native--references (name)
  "Return xrefs for indexed local references named NAME."
  (let* ((document (ansible-intelligence-native--document))
         (index (ansible-intelligence-native--index))
         (references
          (if index
              (ansible-intelligence-index-references index name)
            (seq-filter
             (lambda (reference)
               (string= name
                        (ansible-intelligence-reference-name reference)))
             (and document
                  (ansible-intelligence-document-references document))))))
    (when (and index name)
      (ansible-intelligence-native--recommend-stale-index-result name))
    (setq references
          (sort references
                (lambda (left right)
                  (let ((left-file
                         (or (ansible-intelligence-reference-file left)
                             buffer-file-name
                             ""))
                        (right-file
                         (or (ansible-intelligence-reference-file right)
                             buffer-file-name
                             "")))
                    (if (equal left-file right-file)
                        (< (ansible-intelligence-range-start
                            (ansible-intelligence-reference-range left))
                           (ansible-intelligence-range-start
                            (ansible-intelligence-reference-range right)))
                      (string-lessp left-file right-file))))))
    (delq nil
          (mapcar
           (lambda (reference)
             (let* ((file (or (ansible-intelligence-reference-file reference)
                              buffer-file-name))
                    (snapshot
                     (ansible-intelligence-native--snapshot-for-file file))
                    (source-document
                     (or (and index
                              (ansible-intelligence-index-current-document
                               index file))
                         (and document
                              (ansible-intelligence--same-file-p
                               file
                               (ansible-intelligence-document-path document))
                              document)))
                    (reference
                     (ansible-intelligence-native--reference-for-snapshot
                      reference snapshot source-document))
                    (range (ansible-intelligence-reference-range reference))
                    (location
                     (ansible-intelligence-native--source-position
                      file (ansible-intelligence-range-start range)
                      snapshot)))
               (when location
                 (xref-make name location))))
           references))))

(defun ansible-intelligence-native-xref-backend ()
  "Select the local xref backend only for owned Ansible/Jinja contexts."
  (when-let* ((context (ansible-intelligence-native-context-at-point))
              ((plist-get context :owned)))
    'ansible-intelligence-native))

(cl-defmethod xref-backend-identifier-at-point
  ((_backend (eql ansible-intelligence-native)))
  "Return the local identifier at point, or nil outside ownership."
  (when-let* ((context (ansible-intelligence-native-context-at-point))
              (name (plist-get context :name)))
    name))

(cl-defmethod xref-backend-definitions
  ((_backend (eql ansible-intelligence-native)) identifier)
  "Return local definitions for IDENTIFIER."
  (when-let* ((context (ansible-intelligence-native-context-at-point))
              ((or (string= identifier (plist-get context :name))
                   (plist-get context :owned))))
    (ansible-intelligence-native--definitions identifier context)))

(cl-defmethod xref-backend-references
  ((_backend (eql ansible-intelligence-native)) identifier)
  "Return local indexed references for IDENTIFIER."
  (when-let* ((context (ansible-intelligence-native-context-at-point))
              ((plist-get context :owned)))
    (ansible-intelligence-native--references identifier)))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql ansible-intelligence-native)))
  "Do not provide a second identifier completion table."
  nil)

(defun ansible-intelligence-native--imenu-marker (range)
  "Return an Imenu marker for RANGE in the current buffer."
  (copy-marker (min (point-max) (ansible-intelligence-range-start range))))

(defun ansible-intelligence-native--symbol-category (symbol)
  "Return an Imenu category for SYMBOL."
  (let ((kind (ansible-intelligence-symbol-kind symbol)))
    (cond
     ((eq kind 'register) "Registers")
     ((memq kind '(set-fact play-var task-var role-default role-var
                          inventory-var vars-file))
      "Variables")
     ((memq kind '(loop-var jinja-loop-var)) "Loops")
     ((eq kind 'jinja-set) "Jinja set")
     (t "Variables"))))

(defun ansible-intelligence-native--semantic-imenu ()
  "Build semantic Imenu categories from the published snapshot."
  (when-let* ((document (ansible-intelligence-native--document)))
    (let (categories)
      (cl-labels
          ((add (category name range)
             (let ((entry (cons name
                                (ansible-intelligence-native--imenu-marker
                                 range)))
                   (bucket (assoc category categories)))
               (if bucket
                   (setcdr bucket (append (cdr bucket) (list entry)))
                 (push (list category entry) categories)))))
        (dolist (outline (ansible-intelligence-document-outline document))
          (add (ansible-intelligence-outline-entry-category outline)
               (ansible-intelligence-outline-entry-name outline)
               (ansible-intelligence-outline-entry-range outline)))
        (dolist (symbol (ansible-intelligence-document-symbols document))
          (add (ansible-intelligence-native--symbol-category symbol)
               (ansible-intelligence-symbol-name symbol)
               (ansible-intelligence-symbol-range symbol)))
        (dolist (link (ansible-intelligence-document-links document))
          (when (eq (ansible-intelligence-link-confidence link) 'static)
            (add "Static includes"
                 (ansible-intelligence-link-target link)
                 (ansible-intelligence-link-range link))))
        (nreverse categories)))))

(defun ansible-intelligence-native--merge-imenu (semantic base)
  "Merge SEMANTIC entries with existing YAML Imenu BASE entries."
  (let ((result (copy-tree semantic)))
    (dolist (entry base)
      (if (and (consp entry) (stringp (car entry))
               (listp (cdr entry)))
          (let ((bucket (assoc (car entry) result)))
            (if bucket
                (setcdr bucket (append (cdr bucket) (list entry)))
              (setq result (append result (list entry)))))
        (setq result (append result (list entry)))))
    result))

(defun ansible-intelligence-native-create-imenu-index ()
  "Create a merged semantic and YAML Imenu index without parsing."
  (let* ((previous
          (unless (eq ansible-intelligence-native--imenu-previous
                      #'ansible-intelligence-native-create-imenu-index)
            ansible-intelligence-native--imenu-previous))
         (generator (or previous
                        (and (fboundp 'imenu-default-create-index-function)
                             #'imenu-default-create-index-function)))
         (base
          (when generator
            (condition-case nil
                (funcall generator)
              (error nil)))))
    (ansible-intelligence-native--merge-imenu
     (or (ansible-intelligence-native--semantic-imenu) nil)
     base)))

(defun ansible-intelligence-native-enable ()
  "Install native adapters in the current buffer."
  (add-hook 'xref-backend-functions #'ansible-intelligence-native-xref-backend
            -90 t)
  (unless (eq imenu-create-index-function
              #'ansible-intelligence-native-create-imenu-index)
    (setq-local ansible-intelligence-native--imenu-previous
                imenu-create-index-function)
    (setq-local imenu-create-index-function
                #'ansible-intelligence-native-create-imenu-index)))

(defun ansible-intelligence-native-disable ()
  "Remove native adapters from the current buffer."
  (remove-hook 'xref-backend-functions #'ansible-intelligence-native-xref-backend t)
  (when (eq imenu-create-index-function
            #'ansible-intelligence-native-create-imenu-index)
    (setq-local imenu-create-index-function
                ansible-intelligence-native--imenu-previous))
  (setq-local ansible-intelligence-native--imenu-previous nil))

(provide 'ansible-intelligence-native)

;;; ansible-intelligence-native.el ends here
