;;; ansible-intelligence-index.el --- Incremental Ansible project index -*- lexical-binding: t; -*-

;;; Code:

(require 'ansible-intelligence-parser)
(require 'project)

(defconst ansible-intelligence-project-file-limit 2000
  "Maximum YAML files retained in one project index.")

(defconst ansible-intelligence-index-refresh-seconds 30
  "Age after which attaching a buffer refreshes a completed project index.")

(defconst ansible-intelligence-index-batch-size 4
  "Number of files parsed during one scheduled indexing slice.")

(defvar ansible-intelligence--project-indexes (make-hash-table :test #'equal))
(defvar-local ansible-intelligence--project-index nil)

(defun ansible-intelligence-project-root (&optional file)
  "Find a conservative Ansible project root for FILE."
  (let* ((path (file-name-directory (or file buffer-file-name default-directory)))
         (ansible-root (locate-dominating-file path "ansible.cfg"))
         (project (project-current nil path)))
    (file-name-as-directory
     (expand-file-name
      (or ansible-root
          (and project (project-root project))
          (locate-dominating-file path "roles")
          path)))))

(defun ansible-intelligence--project-index (root &optional project)
  "Return or create the index for ROOT, associating PROJECT when available."
  (let ((index
         (or (gethash root ansible-intelligence--project-indexes)
             (puthash
              root
              (ansible-intelligence-project-index-create
               :root root :documents (make-hash-table :test #'equal)
               :files nil :pending nil :generation 0 :status 'new
               :jinja-cache (make-hash-table :test #'equal))
              ansible-intelligence--project-indexes))))
    (when project
      (setf (ansible-intelligence-project-index-project index) project))
    index))

(defun ansible-intelligence-index-put-document (index document)
  "Atomically publish DOCUMENT in INDEX."
  (when-let* ((path (ansible-intelligence-document-path document)))
    (puthash (ansible-intelligence--canonical-path path) document
             (ansible-intelligence-project-index-documents index))))

(defun ansible-intelligence-index-current-document (index file)
  "Return indexed document for FILE in INDEX."
  (and file (gethash (ansible-intelligence--canonical-path file)
                     (ansible-intelligence-project-index-documents index))))

(defun ansible-intelligence--live-document-buffer (file)
  "Return a live buffer with a snapshot for FILE, including symlink aliases."
  (seq-find
   (lambda (buffer)
     (and (buffer-live-p buffer)
          (with-current-buffer buffer
            (and (bound-and-true-p ansible-intelligence--document)
                 (ansible-intelligence--same-file-p buffer-file-name file)))))
   (delete-dups (cons (find-buffer-visiting file) (buffer-list)))))

(defun ansible-intelligence-index-document-current-p (document)
  "Return non-nil when DOCUMENT still describes usable source content.

An exact live-buffer snapshot is authoritative while its modification tick is
current.  Otherwise only a disk snapshot whose source identity still matches
the file on disk is usable."
  (let* ((file (and document
                    (ansible-intelligence-document-path document)))
         (live-buffer (and file
                           (ansible-intelligence--live-document-buffer file))))
    (if live-buffer
        (with-current-buffer live-buffer
          (let ((live-document
                 (and (boundp 'ansible-intelligence--document)
                      ansible-intelligence--document)))
            (and live-document
                 (numberp (ansible-intelligence-document-tick live-document))
                 (= (ansible-intelligence-document-tick live-document)
                    (buffer-chars-modified-tick))
                 (or (eq document live-document)
                     (let ((live-identity
                            (ansible-intelligence-document-source-identity
                             live-document))
                           (document-identity
                            (ansible-intelligence-document-source-identity
                             document)))
                       (and live-identity document-identity
                            (not (ansible-intelligence-source-identity-live-p
                                  live-identity))
                            (not (ansible-intelligence-source-identity-live-p
                                  document-identity))
                            (ansible-intelligence-source-identity-equal-p
                             document-identity
                             (ansible-intelligence-source-identity-for-file
                              file))))))))
      (let ((identity
             (ansible-intelligence-document-source-identity document)))
        (and (file-regular-p file)
             identity
             (not (ansible-intelligence-source-identity-live-p identity))
             (ansible-intelligence-source-identity-equal-p
              identity (ansible-intelligence-source-identity-for-file file)))))))

(defun ansible-intelligence--parse-file (file &optional kind jinja-cache)
  "Parse FILE as optional KIND without enabling modes or project configuration."
  (with-temp-buffer
    (let (parser)
      (unwind-protect
          (progn
            (setq buffer-file-name file)
            (insert-file-contents file)
            (when (and (fboundp 'treesit-language-available-p)
                       (treesit-language-available-p 'yaml)
                       (fboundp 'treesit-parser-create))
              (setq parser (treesit-parser-create 'yaml)))
            (set-buffer-modified-p nil)
            (ansible-intelligence-parse-buffer kind jinja-cache))
        (when (and parser (fboundp 'treesit-parser-delete))
          (condition-case nil
              (treesit-parser-delete parser)
            (error nil)))
        ;; Some user configurations install global kill hooks that inspect
        ;; file-visiting buffers.  This internal buffer must remain inert.
        (setq buffer-file-name nil)
        (set-buffer-modified-p nil)))))

(defun ansible-intelligence--index-next-batch (index generation)
  "Parse the next INDEX batch belonging to GENERATION."
  (when (= generation (ansible-intelligence-project-index-generation index))
    (let ((pending (ansible-intelligence-project-index-pending index))
          (count 0))
      (while (and pending (< count ansible-intelligence-index-batch-size))
        (let ((file (pop pending)))
          (when (and (file-regular-p file)
                     (< (file-attribute-size (file-attributes file)) (* 2 1024 1024)))
            (condition-case error
                (let ((live-buffer
                       (ansible-intelligence--live-document-buffer file)))
                  (if live-buffer
                    (let ((document
                           (with-current-buffer live-buffer
                             (and (boundp 'ansible-intelligence--document)
                                  (symbol-value
                                   'ansible-intelligence--document)))))
                      ;; Never replace a completed disk entry with a live
                      ;; snapshot that is already behind the buffer.  The
                      ;; debounce path will publish the next matching tick;
                      ;; parsing disk here would discard unsaved edits.
                      (when (and document
                                 (numberp
                                  (ansible-intelligence-document-tick
                                   document))
                                 (with-current-buffer live-buffer
                                   (= (ansible-intelligence-document-tick
                                       document)
                                      (buffer-chars-modified-tick))))
                        (ansible-intelligence-index-put-document
                         index document)))
                  (ansible-intelligence-index-put-document
                   index
                   (ansible-intelligence--parse-file
                    file nil
                    (ansible-intelligence-project-index-jinja-cache index)))))
              (error
               (message "Ansible Intelligence: could not index %s: %s"
                        file (error-message-string error)))))
          (setq count (1+ count))))
      (setf (ansible-intelligence-project-index-pending index) pending)
      (if pending
          (setf (ansible-intelligence-project-index-timer index)
                (run-at-time
                 0.05 nil #'ansible-intelligence--index-next-batch index generation))
        (ansible-intelligence--reclassify-linked-variable-files index)
        (let ((files (ansible-intelligence-project-index-files index))
              stale)
          (maphash
           (lambda (path _document)
             (unless (or (member path files)
                         (find-buffer-visiting path)
                         (ansible-intelligence--same-file-p
                          path (ansible-intelligence-project-index-focus-file index)))
               (push path stale)))
           (ansible-intelligence-project-index-documents index))
          (dolist (path stale)
            (remhash path (ansible-intelligence-project-index-documents index))))
        (setf (ansible-intelligence-project-index-status index) 'ready
              (ansible-intelligence-project-index-completed-at index) (float-time)
              (ansible-intelligence-project-index-timer index) nil)))))

(defun ansible-intelligence--prioritize-files (index files)
  "Put directly related and conventional variable FILES first for INDEX."
  (let* ((documents (ansible-intelligence-project-index-documents index))
         (current
          (and-let*
              ((focus (ansible-intelligence-project-index-focus-file index)))
            (gethash (file-truename focus) documents)))
         (related
          (and current
               (mapcan
                (lambda (link)
                  (ansible-intelligence--resolve-links
                   current link (ansible-intelligence-project-index-root index)))
                (ansible-intelligence-document-links current)))))
    (sort files
          (lambda (left right)
            (< (cond ((member left related) 0)
                     ((string-match-p "/\\(?:group_vars\\|host_vars\\|defaults\\|vars\\)/" left) 1)
                     (t 2))
               (cond ((member right related) 0)
                     ((string-match-p "/\\(?:group_vars\\|host_vars\\|defaults\\|vars\\)/" right) 1)
                     (t 2)))))))

(defun ansible-intelligence--accept-discovered-files (index generation discovered)
  "Queue YAML files from DISCOVERED for INDEX and GENERATION."
  (when (= generation (ansible-intelligence-project-index-generation index))
    (setf (ansible-intelligence-project-index-process index) nil)
    (let* ((root (ansible-intelligence-project-index-root index))
           (files
            (ansible-intelligence--prioritize-files
             index
             (seq-filter
              (lambda (file)
                (and (file-in-directory-p file root)
                     (string-match-p "\\.ya?ml\\'" file)))
              (mapcar (lambda (file)
                        (file-truename
                         (if (file-name-absolute-p file)
                             file
                           (expand-file-name file root))))
                      discovered))))
           (files (seq-take files ansible-intelligence-project-file-limit)))
      (setf (ansible-intelligence-project-index-files index) files
            (ansible-intelligence-project-index-pending index) files
            (ansible-intelligence-project-index-status index) 'indexing)
      (ansible-intelligence--index-next-batch index generation))))

(defun ansible-intelligence--discover-project-files (index generation)
  "Discover files through `project.el' for INDEX and GENERATION."
  (when (= generation (ansible-intelligence-project-index-generation index))
    (condition-case error
        (let ((project (ansible-intelligence-project-index-project index))
              (focus (ansible-intelligence-project-index-focus-file index)))
          (ansible-intelligence--accept-discovered-files
           index generation
           (if project
               (project-files project)
             (and focus (list focus)))))
      (error
       (setf (ansible-intelligence-project-index-status index) 'failed
             (ansible-intelligence-project-index-timer index) nil)
       (message "Ansible Intelligence project discovery failed: %s"
                (error-message-string error))))))

(defun ansible-intelligence--start-project-discovery (index generation)
  "Run `project.el' discovery for INDEX in a worker thread."
  (setf
   (ansible-intelligence-project-index-process index)
   (make-thread
    (lambda ()
      (condition-case error
          (let ((project (ansible-intelligence-project-index-project index))
                (focus (ansible-intelligence-project-index-focus-file index)))
            (run-at-time
             0 nil #'ansible-intelligence--accept-discovered-files
             index generation
             (if project (project-files project) (and focus (list focus)))))
        (error
         (run-at-time
          0 nil
          (lambda ()
            (when (= generation
                     (ansible-intelligence-project-index-generation index))
              (setf (ansible-intelligence-project-index-status index) 'failed
                    (ansible-intelligence-project-index-process index) nil)
              (message "Ansible Intelligence project discovery failed: %s"
                       (error-message-string error))))))))
    "ansible-intelligence-discovery")))

(defun ansible-intelligence-index-start (index)
  "Start or supersede scheduled `project.el' discovery and indexing for INDEX."
  (when-let* ((timer (ansible-intelligence-project-index-timer index)))
    (cancel-timer timer))
  (let* ((generation
          (1+ (or (ansible-intelligence-project-index-generation index) 0)))
         (timer (run-at-time
                 0 nil #'ansible-intelligence--start-project-discovery
                 index generation)))
    (setf (ansible-intelligence-project-index-generation index) generation
          (ansible-intelligence-project-index-status index) 'discovering
          (ansible-intelligence-project-index-started-at index) (float-time)
          (ansible-intelligence-project-index-timer index) timer)
    index))

(defun ansible-intelligence-index-attach ()
  "Attach the current buffer to its project index."
  (let* ((path (file-name-directory (or buffer-file-name default-directory)))
         (project (project-current nil path))
         (root (ansible-intelligence-project-root buffer-file-name)))
    (setq ansible-intelligence--project-index
          (ansible-intelligence--project-index root project)))
  (setf (ansible-intelligence-project-index-focus-file
         ansible-intelligence--project-index)
        buffer-file-name)
  (unless (memq (ansible-intelligence-project-index-status
                 ansible-intelligence--project-index)
                '(discovering indexing))
    (when (or (not (eq (ansible-intelligence-project-index-status
                        ansible-intelligence--project-index)
                       'ready))
              (null (ansible-intelligence-project-index-completed-at
                     ansible-intelligence--project-index))
              (> (- (float-time)
                    (ansible-intelligence-project-index-completed-at
                     ansible-intelligence--project-index))
                 ansible-intelligence-index-refresh-seconds))
      (ansible-intelligence-index-start ansible-intelligence--project-index)))
  ansible-intelligence--project-index)

(defun ansible-intelligence--resolve-links (document link root)
  "Resolve LINK from DOCUMENT within ROOT without allowing traversal."
  (let* ((target (ansible-intelligence-link-target link))
         (kind (ansible-intelligence-link-kind link))
         (base (file-name-directory
                (or (ansible-intelligence-document-path document)
                    root)))
         (candidates
          (pcase kind
            ((or 'import-role 'include-role)
             (list (expand-file-name (format "roles/%s/tasks/main.yml" target) root)
                   (expand-file-name (format "roles/%s/defaults/main.yml" target) root)
                   (expand-file-name (format "roles/%s/vars/main.yml" target) root)))
            (_
             (let ((relative (expand-file-name target base))
                   (root-relative (expand-file-name target root)))
               (append
                (list relative root-relative)
                (unless (file-name-extension target)
                  (list (concat relative ".yml") (concat relative ".yaml")
                        (concat root-relative ".yml")
                        (concat root-relative ".yaml")))))))))
    (delete-dups
     (seq-filter (lambda (candidate)
                   (and (file-in-directory-p candidate root)
                        (file-regular-p candidate)))
                 candidates))))

(defun ansible-intelligence--reclassify-linked-variable-files (index)
  "Classify files reached through vars links in INDEX as variable documents."
  (let ((root (ansible-intelligence-project-index-root index))
        targets)
    (maphash
     (lambda (_path document)
       (dolist (link (ansible-intelligence-document-links document))
         (when (memq (ansible-intelligence-link-kind link)
                      '(vars-files include-vars))
           (setq targets
                  (append (ansible-intelligence--resolve-links document link root)
                          targets)))))
     (ansible-intelligence-project-index-documents index))
    (dolist (file (delete-dups targets))
      (unless (find-buffer-visiting file)
        (condition-case error
            (ansible-intelligence-index-put-document
             index
             (ansible-intelligence--parse-file
              file 'vars-file
              (ansible-intelligence-project-index-jinja-cache index)))
          (error
           (message "Ansible Intelligence: could not classify vars file %s: %s"
                     file (error-message-string error))))))))

(defun ansible-intelligence--role-companion-documents (index document)
  "Return defaults and vars documents for DOCUMENT's containing role."
  (let ((path (ansible-intelligence-document-path document))
        (documents (ansible-intelligence-project-index-documents index))
        result)
    (when (and path (string-match "\\(.*/roles/[^/]+/\\)" path))
      (let ((role-root (match-string 1 path)))
        (dolist (relative '("defaults/main.yml" "defaults/main.yaml"
                             "vars/main.yml" "vars/main.yaml"))
          (let ((file (expand-file-name relative role-root)))
            (when-let* ((candidate (and (file-exists-p file)
                                        (gethash
                                         (ansible-intelligence--canonical-path file)
                                         documents))))
              (push candidate result))))))
    result))

(defun ansible-intelligence-index-related-documents (index document &optional position)
  "Return documents statically related to DOCUMENT before optional POSITION."
  (let ((root (ansible-intelligence-project-index-root index))
        (documents (ansible-intelligence-project-index-documents index))
        (seen (make-hash-table :test #'equal))
        (queue (append (list document)
                       (ansible-intelligence--role-companion-documents
                        index document)))
        result)
    (while queue
      (let ((current (pop queue)))
        (let ((current-path
               (ansible-intelligence--canonical-path
                (ansible-intelligence-document-path current))))
          (unless (or (gethash current-path seen)
                      (and (not (eq current document))
                           (not (ansible-intelligence-index-document-current-p
                                 current))))
            (puthash current-path t seen)
            (push current result)
            (dolist (link (ansible-intelligence-document-links current))
              (when (or (null position)
                        (not (ansible-intelligence--same-file-p
                              (ansible-intelligence-document-path current)
                              (ansible-intelligence-document-path document)))
                        (< (ansible-intelligence-range-start
                            (ansible-intelligence-link-range link))
                           position))
                (dolist (path (ansible-intelligence--resolve-links current link root))
                  (when-let* ((linked
                               (gethash (ansible-intelligence--canonical-path path)
                                        documents)))
                    (push linked queue)))))))))
    (nreverse result)))

(defun ansible-intelligence-index-project-variable-documents (index)
  "Return broadly visible inventory variable documents from INDEX."
  (let (result)
    (maphash
     (lambda (_path document)
       (when (and (eq (ansible-intelligence-document-kind document) 'inventory-var)
                  (ansible-intelligence-index-document-current-p document))
         (push document result)))
     (ansible-intelligence-project-index-documents index))
    result))

(defun ansible-intelligence-index-definitions (index name)
  "Return all indexed definitions named NAME."
  (let (result)
    (maphash
     (lambda (_path document)
       (when (ansible-intelligence-index-document-current-p document)
         (dolist (symbol (ansible-intelligence-document-symbols document))
           (when (string= name (ansible-intelligence-symbol-name symbol))
             (push symbol result)))))
     (ansible-intelligence-project-index-documents index))
    (nreverse result)))

(defun ansible-intelligence-index-visible-definitions (index document name
                                                              &optional position)
  "Return definitions of NAME visible through DOCUMENT before optional POSITION."
  (let (result)
    (dolist (candidate
             (append (ansible-intelligence-index-related-documents
                      index document position)
                     (ansible-intelligence-index-project-variable-documents index)))
      (dolist (symbol (ansible-intelligence-document-symbols candidate))
        (when (and
                   (string= name (ansible-intelligence-symbol-name symbol))
                   (or
                    (not (ansible-intelligence--same-file-p
                          (ansible-intelligence-document-path candidate)
                          (ansible-intelligence-document-path document)))
                    (null position)
                    (let ((range (ansible-intelligence-symbol-range symbol)))
                      (or
                       (and (<= (ansible-intelligence-symbol-visibility-start symbol)
                                position)
                            (or (null (ansible-intelligence-symbol-visibility-end symbol))
                                (<= position
                                    (ansible-intelligence-symbol-visibility-end symbol))))
                       (and (<= (ansible-intelligence-range-start range) position)
                            (<= position
                                (ansible-intelligence-range-end range))))))
                   (not (memq symbol result)))
          (push symbol result))))
    (nreverse result)))

(defun ansible-intelligence-index-references (index name)
  "Return all indexed references named NAME."
  (let (result)
    (maphash
     (lambda (_path document)
       (when (ansible-intelligence-index-document-current-p document)
         (dolist (reference (ansible-intelligence-document-references document))
           (when (string= name (ansible-intelligence-reference-name reference))
             (push reference result)))))
     (ansible-intelligence-project-index-documents index))
    (nreverse result)))

(provide 'ansible-intelligence-index)

;;; ansible-intelligence-index.el ends here
