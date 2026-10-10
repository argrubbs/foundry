;;; ansible-intelligence-native.el --- Native Emacs integration adapters -*- lexical-binding: t; -*-

;;; Commentary:
;; This module is the routing boundary between immutable Ansible snapshots and
;; native Emacs interfaces.  Adapters never parse YAML/Jinja or discover
;; project files synchronously; ownership is decided from the current
;; published document and in-memory project index.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'eldoc)
(require 'imenu)
(require 'ansible-intelligence-model)
(require 'ansible-intelligence-jinja)
(require 'ansible-intelligence-index)
(require 'flymake nil t)

(declare-function ansible-intelligence--visible-symbols
                  "ansible-intelligence")
(declare-function ansible-intelligence--fields-for-path
                  "ansible-intelligence")
(declare-function ansible-intelligence--same-file-p
                  "ansible-intelligence-index")
(declare-function ansible-intelligence--resolve-links
                  "ansible-intelligence-index")
(declare-function ansible-intelligence--live-document-buffer
                  "ansible-intelligence-index")
(declare-function ansible-intelligence-index-document-current-p
                  "ansible-intelligence-index")
(declare-function ansible-intelligence-index-start
                  "ansible-intelligence-index")

(defgroup ansible-intelligence-native nil
  "Native Emacs adapters for Ansible Intelligence."
  :group 'ansible-intelligence)

(defcustom ansible-intelligence-native-flymake-idle-delay 0.15
  "Seconds to debounce local Flymake reporting."
  :type 'number
  :group 'ansible-intelligence-native)

(defvar-local ansible-intelligence-native--imenu-previous nil)
(defvar-local ansible-intelligence-native--flymake-timer nil)
(defvar-local ansible-intelligence-native--flymake-generation 0)
(defvar-local ansible-intelligence-native--flymake-owned nil)
(defvar-local ansible-intelligence-native--flymake-pending nil)
(defvar-local ansible-intelligence-native--flymake-state-captured nil)
(defvar-local ansible-intelligence-native--flymake-prior-mode nil)
(defvar-local ansible-intelligence-native--flymake-prior-backends nil)
(defvar-local ansible-intelligence-native--flymake-report-functions nil)
(defvar-local ansible-intelligence-native--eldoc-state-captured nil)
(defvar-local ansible-intelligence-native--eldoc-prior-mode nil)
(defvar-local ansible-intelligence-native--eldoc-prior-functions nil)
(defvar-local ansible-intelligence-native--eldoc-enabled-by-ai nil)

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
               :symbol symbol :document document))
       (when-let* ((jinja (ansible-intelligence-document-jinja document))
                   (completion
                    (ansible-intelligence-jinja-completion-context
                     jinja position))
                   (base (plist-get completion :base))
                   (known (or (ansible-intelligence-native--current-symbols
                               base position)
                              (plist-get completion :field-p))))
         ;; This branch owns a syntactically valid Jinja identifier even when
         ;; its definition is not indexed; that is what enables a precise
         ;; unresolved-local diagnostic without stealing module completion.
         (list :owned t :kind 'variable :name base
               :path (plist-get completion :path)
               :completion completion
               :region (plist-get completion :region)
               :document document))))))

(defun ansible-intelligence-native-capf-owned-p ()
  "Return non-nil when local semantic completion owns point."
  (and (bound-and-true-p ansible-intelligence-mode)
       (let ((context (ansible-intelligence-native-context-at-point)))
         (or context
             (and (not (ansible-intelligence-native--module-key-at-point-p))
                  (ansible-intelligence-native--capf-fallback-context-p))))))

(defun ansible-intelligence-native--capf-fallback-context-p ()
  "Return non-nil for a safe non-Jinja CAPF context."
  (let* ((bounds (save-excursion
                   (skip-chars-backward "[:alnum:]_")
                   (cons (point) (progn
                                   (skip-chars-forward "[:alnum:]_")
                                   (point)))))
         (text (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (or (string-empty-p text)
        (seq-some
         (lambda (symbol)
           (string= text (ansible-intelligence-symbol-name symbol)))
         (and (fboundp 'ansible-intelligence--visible-symbols)
              (ansible-intelligence--visible-symbols (car bounds))))
        (save-excursion
          (let ((position (point)))
            (beginning-of-line)
            (when (re-search-forward
                   "^[ \t-]*\\(?:var\\|when\\|failed_when\\|changed_when\\|until\\|loop\\)[ \t]*:"
                   (line-end-position) t)
              (<= (match-end 0) position)))))))

(defun ansible-intelligence-native--module-key-at-point-p ()
  "Return non-nil when point is on a YAML mapping key.

This small routing check uses only the current line and never invokes a
parser.  Jinja references are handled first by the snapshot context."
  (let ((position (point)))
    (save-excursion
      (beginning-of-line)
      (when (re-search-forward
             "^[ \t-]*\\([[:alnum:]_.]+\\)[ \t]*:"
             (line-end-position) t)
        (and (<= (match-beginning 1) position)
             (<= position (match-end 1)))))))

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

(defun ansible-intelligence-native--field-summary (symbol)
  "Return a concise field summary for SYMBOL."
  (when-let* ((fields (ansible-intelligence-symbol-fields symbol)))
    (string-join
     (mapcar #'car fields) ", ")))

(defun ansible-intelligence-native--definition-label (symbol)
  "Return a source label for SYMBOL."
  (let ((file (ansible-intelligence-symbol-file symbol))
        (range (ansible-intelligence-symbol-range symbol)))
    (if (and file (ansible-intelligence--same-file-p file buffer-file-name))
        (format "%s:%d" (file-name-nondirectory file)
                (save-excursion
                  (goto-char (min (point-max)
                                  (ansible-intelligence-range-start range)))
                  (line-number-at-pos)))
      (if file
          (if-let* ((line (ansible-intelligence-symbol-line symbol)))
              (format "%s:%d" (file-name-nondirectory file) line)
            (format "%s@%d" (file-name-nondirectory file)
                    (ansible-intelligence-range-start range)))
        "current buffer"))))

(defun ansible-intelligence-native-symbol-documentation (symbol &optional path)
  "Return provenance documentation for SYMBOL and optional field PATH."
  (let ((fields (ansible-intelligence-native--field-summary symbol)))
    (string-join
     (delq nil
           (list
            (format "%s: %s"
                    (ansible-intelligence-symbol-name symbol)
                    (symbol-name (ansible-intelligence-symbol-kind symbol)))
            (format "source %s; confidence %s"
                    (ansible-intelligence-symbol-source symbol)
                    (ansible-intelligence-symbol-confidence symbol))
            (format "definition %s"
                    (ansible-intelligence-native--definition-label symbol))
            (and fields (format "fields: %s" fields))
            (and path
                 (format "Jinja path: .%s" (string-join path ".")))))
     "; ")))

(defun ansible-intelligence-native--eldoc-text (context)
  "Build snapshot-only Eldoc text for CONTEXT."
  (let* ((document (plist-get context :document))
         (name (plist-get context :name))
         (path (plist-get context :path))
  (index (ansible-intelligence-native--index))
  (symbol
          (or (plist-get context :symbol)
              (car (ansible-intelligence-native--current-symbols name))
              (and index document
                   (car (ansible-intelligence-index-visible-definitions
                         index document name (point))))))
         (jinja (ansible-intelligence-document-jinja document))
         (partial
          (or (ansible-intelligence-jinja-snapshot-incomplete jinja)
              (ansible-intelligence-document-errors document)))
         (ambiguous
          (and index
               (> (length
                   (ansible-intelligence-index-visible-definitions
                    index document name (point)))
                  1)))
         (region (plist-get context :region))
         (region-kind (and region
                           (ansible-intelligence-jinja-region-kind region)))
         (stale (not (= (ansible-intelligence-document-tick document)
                        (buffer-chars-modified-tick)))))
    (when (or symbol
              (eq (plist-get context :kind) 'static-link)
              (eq (plist-get context :kind) 'variable))
      (string-join
       (delq nil
             (list
              (cond
                (symbol
                 (ansible-intelligence-native-symbol-documentation symbol path))
                ((eq (plist-get context :kind) 'static-link)
                 (format "%s: static %s" name
                         (ansible-intelligence-link-kind
                          (plist-get context :link))))
                (t (format "%s: unresolved local Jinja variable" name)))
               (and (null symbol) "source snapshot reference; confidence unknown")
               (and region-kind (format "Jinja %s" region-kind))
              (and ambiguous "ambiguous: multiple visible definitions")
              (and index
                   (eq (ansible-intelligence-project-index-status index)
                       'ready)
                   "project index ready")
              (and index
                   (eq (ansible-intelligence-project-index-status index)
                       'indexing)
                   "project index partial")
              (and stale "snapshot stale")
              (and partial "snapshot partial")))
       "; "))))

(defun ansible-intelligence-native-eldoc (&optional callback)
  "Provide local provenance Eldoc, or nil outside owned contexts."
  (when-let* ((context (ansible-intelligence-native-context-at-point))
              (text (ansible-intelligence-native--eldoc-text context)))
    (if callback
        (progn
          (funcall callback text :thing (plist-get context :name))
          nil)
      text)))

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

(defun ansible-intelligence-native--diagnostic
    (beg end type message reason &optional symbol)
  "Create a local diagnostic with explanatory metadata."
  (when (< end beg)
    (setq end beg))
  (flymake-make-diagnostic
   (current-buffer) beg (max beg end) type
   (list "ansible-intelligence" (symbol-name reason) message)
   (list :source 'ansible-intelligence
         :reason reason :symbol symbol
         :generation ansible-intelligence-native--flymake-generation)))

(defconst ansible-intelligence-native--magic-names
  '("ansible_facts" "hostvars" "group_names" "groups" "inventory_hostname"
    "inventory_hostname_short" "play_hosts" "omit" "role_name" "role_path"
    "vars" "environment")
  "Names that are runtime-provided rather than local project definitions.")

(defun ansible-intelligence-native--diagnostics (&optional expected-tick)
  "Compute conservative diagnostics from published snapshots only.

When EXPECTED-TICK is non-nil, return no diagnostics unless the published
document has exactly that tick."
  (let* ((document (ansible-intelligence-native--document))
         (index (ansible-intelligence-native--index))
         (jinja (and document (ansible-intelligence-document-jinja document)))
         (diagnostics nil)
         (uncertain
          (or (null document)
              (ansible-intelligence-jinja-snapshot-incomplete jinja)
              (ansible-intelligence-document-errors document))))
    (when (and document
               (or (null expected-tick)
                   (and (numberp
                         (ansible-intelligence-document-tick document))
                        (= expected-tick
                           (ansible-intelligence-document-tick document)))))
      (dolist (region (ansible-intelligence-jinja-snapshot-regions jinja))
        (let ((range (ansible-intelligence-jinja-region-range region)))
          (when (not (ansible-intelligence-jinja-region-complete region))
            (push
             (ansible-intelligence-native--diagnostic
              (ansible-intelligence-range-start range)
              (ansible-intelligence-range-end range) 'error
              "Incomplete Jinja region" 'malformed-jinja)
             diagnostics))
          (when (and (ansible-intelligence-jinja-region-complete region)
                     (ansible-intelligence-jinja-region-error region))
            (push
             (ansible-intelligence-native--diagnostic
              (ansible-intelligence-range-start range)
              (ansible-intelligence-range-end range) 'warning
              (ansible-intelligence-jinja-region-error region)
              'partial-jinja)
             diagnostics))))
      (when (and index
                 (eq (ansible-intelligence-project-index-status index) 'ready)
                 (not uncertain))
        (dolist (reference (ansible-intelligence-document-references document))
          (let ((name (ansible-intelligence-reference-name reference)))
            (unless (or (member name ansible-intelligence-native--magic-names)
                        (ansible-intelligence-index-visible-definitions
                         index document name
                         (ansible-intelligence-range-start
                          (ansible-intelligence-reference-range reference))))
              (let ((range (ansible-intelligence-reference-range reference)))
                (push
                 (ansible-intelligence-native--diagnostic
                  (ansible-intelligence-range-start range)
                  (ansible-intelligence-range-end range) 'error
                  (format "Unresolved local Ansible reference `%s'" name)
                  'unresolved-reference name)
                 diagnostics)))))
      (dolist (link (ansible-intelligence-document-links document))
        (let ((range (ansible-intelligence-link-range link)))
          (cond
           ((eq (ansible-intelligence-link-confidence link) 'dynamic)
            (push
             (ansible-intelligence-native--diagnostic
              (ansible-intelligence-range-start range)
              (ansible-intelligence-range-end range) 'warning
              "Dynamic include target is not indexed"
              'dynamic-include)
             diagnostics))
           ((and index
                 (eq (ansible-intelligence-project-index-status index) 'ready))
            (let* ((target (ansible-intelligence-link-target link))
                   (root (ansible-intelligence-project-index-root index))
                   (absolute (expand-file-name
                              target
                              (file-name-directory
                               (or buffer-file-name root)))))
              (if (not (file-in-directory-p absolute root))
                  (push
                   (ansible-intelligence-native--diagnostic
                    (ansible-intelligence-range-start range)
                    (ansible-intelligence-range-end range) 'error
                    "Static include path escapes the Ansible project"
                    'escaping-include)
                   diagnostics)
                (unless (ansible-intelligence--resolve-links
                         document link root)
                  (push
                   (ansible-intelligence-native--diagnostic
                    (ansible-intelligence-range-start range)
                    (ansible-intelligence-range-end range) 'error
                    (format "Static include target not found: %s" target)
                    'missing-include)
                   diagnostics))))))))))
    (nreverse diagnostics)))

(defun ansible-intelligence-native--analysis-published ()
  "Restart a deferred Flymake request after a matching analysis publish."
  (when-let* ((pending ansible-intelligence-native--flymake-pending)
              (report-fn (nth 0 pending))
              (tick (nth 1 pending))
              (document (ansible-intelligence-native--document)))
    (when (and (numberp (ansible-intelligence-document-tick document))
               (= tick (ansible-intelligence-document-tick document)))
      (setq ansible-intelligence-native--flymake-pending nil)
      (if (and (fboundp 'flymake-start)
               (bound-and-true-p flymake-mode))
          (flymake-start nil t)
        (funcall report-fn
                 (ansible-intelligence-native--diagnostics tick))))))

(defun ansible-intelligence-native-flymake-backend (report-fn &rest _args)
  "Report debounced conservative local diagnostics."
  (cl-pushnew report-fn ansible-intelligence-native--flymake-report-functions)
  (when (timerp ansible-intelligence-native--flymake-timer)
    (cancel-timer ansible-intelligence-native--flymake-timer))
  (setq ansible-intelligence-native--flymake-generation
        (1+ ansible-intelligence-native--flymake-generation))
  (setq ansible-intelligence-native--flymake-pending nil)
  (let ((buffer (current-buffer))
        (generation ansible-intelligence-native--flymake-generation)
        (tick (buffer-chars-modified-tick)))
    ;; Clearing this backend's previous report does not affect Eglot's
    ;; independent Flymake backend.
    (funcall report-fn nil)
    (setq ansible-intelligence-native--flymake-timer
          (run-with-idle-timer
           ansible-intelligence-native-flymake-idle-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (and (= generation
                              ansible-intelligence-native--flymake-generation)
                            (= tick (buffer-chars-modified-tick)))
                   (setq ansible-intelligence-native--flymake-timer nil)
                   (let ((document (ansible-intelligence-native--document)))
                     (if (and document
                              (numberp
                               (ansible-intelligence-document-tick document))
                              (= tick
                                 (ansible-intelligence-document-tick
                                  document)))
                       (funcall report-fn
                                (ansible-intelligence-native--diagnostics tick))
                       (unless ansible-intelligence-native--flymake-pending
                         (setq ansible-intelligence-native--flymake-pending
                               (list report-fn tick generation))
                         (when (fboundp
                                'ansible-intelligence--schedule-analysis)
                           (ansible-intelligence--schedule-analysis)))))))))
           ))))

(defun ansible-intelligence-native-explain-diagnostic ()
  "Display metadata for the local Flymake diagnostic at point."
  (when (fboundp 'flymake-diagnostics)
    (when-let* ((diagnostic
                (car (flymake-diagnostics (point) (point))))
                (data (flymake-diagnostic-data diagnostic)))
      (when (eq (plist-get data :source) 'ansible-intelligence)
        (message "Ansible Intelligence: %s (%s)"
                 (flymake-diagnostic-text diagnostic)
                 (plist-get data :reason))
        t))))

(defun ansible-intelligence-native--eldoc-consumer-remains-p ()
  "Return non-nil when Eldoc has a consumer beyond this adapter.

The prior hook list is treated as an existing consumer except for the
standard global Eldoc defaults.  New functions added after AI enabled Eldoc
also keep the mode enabled."
  (let* ((prior ansible-intelligence-native--eldoc-prior-functions)
         (current
          (and (boundp 'eldoc-documentation-functions)
               (remove #'ansible-intelligence-native-eldoc
                       eldoc-documentation-functions)))
         (defaults (and (boundp 'eldoc-documentation-functions)
                        (default-value 'eldoc-documentation-functions))))
    (or (seq-some (lambda (function)
                    (not (memq function defaults)))
                  prior)
        (seq-some (lambda (function)
                    (not (member function prior)))
                  current)
        (and (boundp 'eldoc-documentation-function)
             eldoc-documentation-function
             (not (eq eldoc-documentation-function
                     'eldoc-documentation-default))))))

(defun ansible-intelligence-native--clear-flymake-reports ()
  "Clear every retained report function for the local backend."
  (dolist (report-fn ansible-intelligence-native--flymake-report-functions)
    (condition-case nil
        (funcall report-fn nil)
      (error nil)))
  (setq ansible-intelligence-native--flymake-report-functions nil))

(defun ansible-intelligence-native--other-flymake-backend-p ()
  "Return non-nil when another Flymake backend remains installed."
  (and (boundp 'flymake-diagnostic-functions)
       (seq-some
        (lambda (backend)
          (not (eq backend #'ansible-intelligence-native-flymake-backend)))
        flymake-diagnostic-functions)))

(defun ansible-intelligence-native-enable ()
  "Install native adapters in the current buffer."
  (unless ansible-intelligence-native--eldoc-state-captured
    (setq ansible-intelligence-native--eldoc-state-captured t
          ansible-intelligence-native--eldoc-prior-mode
          (bound-and-true-p eldoc-mode)
          ansible-intelligence-native--eldoc-prior-functions
          (and (boundp 'eldoc-documentation-functions)
               (copy-sequence eldoc-documentation-functions))))
  (unless ansible-intelligence-native--flymake-state-captured
    (setq ansible-intelligence-native--flymake-state-captured t
          ansible-intelligence-native--flymake-prior-mode
          (bound-and-true-p flymake-mode)
          ansible-intelligence-native--flymake-prior-backends
          (and (boundp 'flymake-diagnostic-functions)
               (copy-sequence flymake-diagnostic-functions))))
  (add-hook 'xref-backend-functions #'ansible-intelligence-native-xref-backend
            -90 t)
  (add-hook 'eldoc-documentation-functions #'ansible-intelligence-native-eldoc
            90 t)
  (unless (eq imenu-create-index-function
              #'ansible-intelligence-native-create-imenu-index)
    (setq-local ansible-intelligence-native--imenu-previous
                imenu-create-index-function)
    (setq-local imenu-create-index-function
                #'ansible-intelligence-native-create-imenu-index))
  (when (boundp 'flymake-diagnostic-functions)
    (add-hook 'flymake-diagnostic-functions
              #'ansible-intelligence-native-flymake-backend 90 t))
  (when (and (fboundp 'flymake-mode)
             (not (bound-and-true-p flymake-mode)))
    (flymake-mode 1)
    (setq ansible-intelligence-native--flymake-owned t))
  (when (fboundp 'eldoc-mode)
    (unless (bound-and-true-p eldoc-mode)
      (eldoc-mode 1)
      (setq ansible-intelligence-native--eldoc-enabled-by-ai t))))

(defun ansible-intelligence-native-disable ()
  "Remove native adapters from the current buffer."
  (remove-hook 'xref-backend-functions #'ansible-intelligence-native-xref-backend t)
  (remove-hook 'eldoc-documentation-functions #'ansible-intelligence-native-eldoc t)
  (when (eq imenu-create-index-function
            #'ansible-intelligence-native-create-imenu-index)
    (setq-local imenu-create-index-function
                ansible-intelligence-native--imenu-previous))
  (setq-local ansible-intelligence-native--imenu-previous nil)
  (remove-hook 'flymake-diagnostic-functions
               #'ansible-intelligence-native-flymake-backend t)
  (ansible-intelligence-native--clear-flymake-reports)
  (setq ansible-intelligence-native--flymake-generation
        (1+ ansible-intelligence-native--flymake-generation))
  (setq ansible-intelligence-native--flymake-pending nil)
  (when (timerp ansible-intelligence-native--flymake-timer)
    (cancel-timer ansible-intelligence-native--flymake-timer)
    (setq ansible-intelligence-native--flymake-timer nil))
  (when (and ansible-intelligence-native--flymake-owned
             (not ansible-intelligence-native--flymake-prior-mode)
             (not (ansible-intelligence-native--other-flymake-backend-p))
             (fboundp 'flymake-mode)
             (bound-and-true-p flymake-mode))
    (flymake-mode -1))
  (setq ansible-intelligence-native--flymake-owned nil
        ansible-intelligence-native--flymake-state-captured nil
        ansible-intelligence-native--flymake-prior-mode nil
        ansible-intelligence-native--flymake-prior-backends nil)
  (when (and ansible-intelligence-native--eldoc-enabled-by-ai
             (not ansible-intelligence-native--eldoc-prior-mode)
             (not (ansible-intelligence-native--eldoc-consumer-remains-p))
             (fboundp 'eldoc-mode)
             (bound-and-true-p eldoc-mode))
    (eldoc-mode -1))
  (setq ansible-intelligence-native--eldoc-enabled-by-ai nil
        ansible-intelligence-native--eldoc-state-captured nil
        ansible-intelligence-native--eldoc-prior-mode nil
        ansible-intelligence-native--eldoc-prior-functions nil)
  nil)

(provide 'ansible-intelligence-native)

;;; ansible-intelligence-native.el ends here
