;;; ansible-intelligence-model.el --- Semantic data contracts -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

(defgroup ansible-intelligence nil
  "Fast, conservative Ansible intelligence."
  :group 'languages)

(cl-defstruct (ansible-intelligence-range
               (:constructor ansible-intelligence-range-create))
  start end)

(defconst ansible-intelligence-source-identity-max-hash-bytes
  (* 2 1024 1024)
  "Maximum file size for bounded source identity hashing.")

(cl-defstruct (ansible-intelligence-source-identity
               (:constructor ansible-intelligence-source-identity-create))
  "Stable identity for the source content represented by a document.

`path', `mtime', and `size' identify the current disk source.  `hash' is
computed for bounded files when possible.  A live buffer with unsaved edits
or content that diverges from disk sets `live-p' so it can only be used while
its buffer snapshot is current."
  path mtime size hash live-p)

(defun ansible-intelligence--source-canonical-path (file)
  "Return the canonical path for FILE without signalling on missing files."
  (when file
    (condition-case nil
        (file-truename file)
      (file-error
       (expand-file-name file)))))

(defun ansible-intelligence--source-file-hash (file size)
  "Return a bounded SHA-256 hash for FILE when appropriate."
  (when (and (fboundp 'secure-hash)
             (integerp size)
             (<= size ansible-intelligence-source-identity-max-hash-bytes)
             (file-readable-p file))
    (condition-case nil
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (secure-hash 'sha256 (current-buffer)))
      (error nil))))

(defun ansible-intelligence--source-buffer-hash ()
  "Return a bounded SHA-256 hash for the current buffer when possible."
  (when (and (fboundp 'secure-hash)
             (<= (buffer-size) ansible-intelligence-source-identity-max-hash-bytes))
    (condition-case nil
        (secure-hash 'sha256 (current-buffer))
      (error nil))))

(defun ansible-intelligence-source-identity-for-file (file)
  "Return the current disk identity for FILE.

The returned identity remains useful for a file that has disappeared, but
callers validating a disk snapshot must still require the file to exist."
  (when file
    (let* ((path (ansible-intelligence--source-canonical-path file))
           (attributes (and path (file-attributes path)))
           (size (and attributes (file-attribute-size attributes)))
           (mtime (and attributes
                       (file-attribute-modification-time attributes))))
      (ansible-intelligence-source-identity-create
       :path path :mtime mtime :size size
       :hash (and attributes
                  (ansible-intelligence--source-file-hash path size))
       :live-p nil))))

(defun ansible-intelligence-source-identity-for-buffer (&optional buffer)
  "Return the source identity represented by BUFFER.

Bounded buffers are compared with their current disk content even when
Emacs considers them unmodified.  A mismatch, like an unsaved edit, remains
live so its snapshot cannot later be routed to a disk file after the buffer
disappears.  Files larger than the bounded hashing limit retain the
conservative modified-p fallback."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((file buffer-file-name)
           (disk (ansible-intelligence-source-identity-for-file file))
           (disk-hash (and disk
                           (ansible-intelligence-source-identity-hash disk)))
           (buffer-hash (and disk-hash
                             (ansible-intelligence--source-buffer-hash)))
           (same-bounded-content
            (and buffer-hash
                 (equal buffer-hash disk-hash))))
      (if (or (null file)
              same-bounded-content
              (and (null disk-hash)
                   (not (buffer-modified-p))))
          disk
        (ansible-intelligence-source-identity-create
         :path (or (and disk
                        (ansible-intelligence-source-identity-path disk))
                   (ansible-intelligence--source-canonical-path file))
         :mtime (and disk
                      (ansible-intelligence-source-identity-mtime disk))
         :size (and disk
                     (ansible-intelligence-source-identity-size disk))
         :hash nil :live-p t)))))

(defun ansible-intelligence-source-identity-equal-p (left right)
  "Return non-nil when LEFT and RIGHT identify the same source content."
  (and left right
       (equal (ansible-intelligence-source-identity-path left)
              (ansible-intelligence-source-identity-path right))
       (equal (ansible-intelligence-source-identity-mtime left)
              (ansible-intelligence-source-identity-mtime right))
       (equal (ansible-intelligence-source-identity-size left)
              (ansible-intelligence-source-identity-size right))
       (or (null (ansible-intelligence-source-identity-hash left))
           (null (ansible-intelligence-source-identity-hash right))
           (equal (ansible-intelligence-source-identity-hash left)
                  (ansible-intelligence-source-identity-hash right)))))

(cl-defstruct (ansible-intelligence-symbol
               (:constructor ansible-intelligence-symbol-create))
  name kind range visibility-start visibility-end source confidence
  fields documentation file line)

(cl-defstruct (ansible-intelligence-reference
               (:constructor ansible-intelligence-reference-create))
  name range file kind path path-ranges)

(cl-defstruct (ansible-intelligence-jinja-node
               (:constructor ansible-intelligence-jinja-node-create))
  "Buffer-independent representation of a Jinja tree-sitter node."
  type range text children named)

(cl-defstruct (ansible-intelligence-jinja-analysis
               (:constructor ansible-intelligence-jinja-analysis-create))
  "Cached, region-relative Jinja syntax and semantic data."
  key parser tree references definitions errors complete work)

(cl-defstruct (ansible-intelligence-jinja-region
               (:constructor ansible-intelligence-jinja-region-create))
  "A Jinja delimiter region embedded in one YAML scalar."
  kind range content-range scalar-range text key analysis tree references definitions
  complete error)

(cl-defstruct (ansible-intelligence-jinja-snapshot
               (:constructor ansible-intelligence-jinja-snapshot-create))
  "Immutable Jinja analysis for one YAML document."
  parser regions references definitions errors incomplete scalar-count region-count
  bytes work cache-hits cache-misses)

(cl-defstruct (ansible-intelligence-link
               (:constructor ansible-intelligence-link-create))
  kind target range confidence)

(cl-defstruct (ansible-intelligence-outline-entry
               (:constructor ansible-intelligence-outline-entry-create))
  "A source-ordered semantic entry suitable for native Imenu."
  name kind range category source confidence file)

(cl-defstruct (ansible-intelligence-shell-range
               (:constructor ansible-intelligence-shell-range-create))
  "A block scalar owned by an Ansible shell-like action.

This is deliberately structural metadata.  Shell identifiers are never added
to the Ansible semantic symbol or reference index."
  kind range action parser)

(cl-defstruct (ansible-intelligence-document
               (:constructor ansible-intelligence-document-create))
  path kind tick parsed-at symbols references links errors tree jinja
  outline shell-ranges source-identity)

(cl-defstruct (ansible-intelligence-project-index
               (:constructor ansible-intelligence-project-index-create))
  root documents files pending generation status started-at completed-at timer process
  focus-file project jinja-cache)

(defconst ansible-intelligence-generic-result-fields
  '(("changed" . "Whether the task changed the managed host.")
    ("failed" . "Whether the task failed.")
    ("msg" . "Human-readable result message.")
    ("rc" . "Command return code, when applicable.")
    ("stdout" . "Standard output, when applicable.")
    ("stderr" . "Standard error, when applicable.")
    ("stdout_lines" . "Standard output split into lines.")
    ("stderr_lines" . "Standard error split into lines.")
    ("results" . "Per-item results for a registered loop."))
  "Conservative fields common to registered task results.")

(provide 'ansible-intelligence-model)

;;; ansible-intelligence-model.el ends here
