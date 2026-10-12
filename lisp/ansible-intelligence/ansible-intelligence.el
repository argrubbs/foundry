;;; ansible-intelligence.el --- Local semantic intelligence for Ansible -*- lexical-binding: t; -*-

;;; Commentary:
;; Variable navigation and references for Ansible using immutable document snapshots.

;;; Code:

(require 'ansible-intelligence-index)
(require 'ansible-intelligence-native)
(require 'xref)

(defconst ansible-intelligence-analysis-idle-delay 0.15
  "Seconds to wait before parsing an edited buffer.")

(defvar-local ansible-intelligence--document nil)
(defvar-local ansible-intelligence--analysis-timer nil)
(defvar-local ansible-intelligence--jinja-cache nil)
(defvar-local ansible-intelligence--analysis-generation 0)
(defvar ansible-intelligence-mode)

(defun ansible-intelligence--analyze (&optional expected-tick expected-generation)
  "Replace the current buffer snapshot when EXPECTED state is still current."
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
           ansible-intelligence--project-index document))))))

(defun ansible-intelligence--schedule-analysis (&rest _)
  "Schedule analysis without making editing wait."
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

(defun ansible-intelligence--name-at-point ()
  "Return the base identifier of the dotted expression at point."
  (let* ((bounds (ansible-intelligence--bounds))
         (name (car (split-string (buffer-substring-no-properties
                                   (car bounds) (cdr bounds)) "\\."))))
    (and name
         (string-match-p (concat "\\`" ansible-intelligence--identifier-re "\\'") name)
         name)))

(defun ansible-intelligence--symbol-at-point ()
  "Return the visible symbol whose name occurs at point."
  (when-let* ((name (ansible-intelligence--name-at-point)))
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

(defun ansible-intelligence--link-at-point ()
  "Return the static include, vars, or role link at point."
  (ansible-intelligence-native--link-at-point ansible-intelligence--document (point)))

(defun ansible-intelligence--link-xrefs (link)
  "Return file xrefs for resolved static LINK."
  (and ansible-intelligence--document
       (ansible-intelligence-native--link-xrefs link ansible-intelligence--document)))

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
  (let* ((backend 'ansible-intelligence-native)
         (id (xref-backend-identifier-at-point backend))
         (defs (and id (xref-backend-definitions backend id))))
    (cond
     ((null defs)
      (user-error "No Ansible definition at point"))
     ((= (length defs) 1)
      (xref-push-marker-stack)
      (xref--show-location (xref-item-location (car defs)) nil))
     (t
      (xref--show-xrefs (lambda () defs) nil)))))

(defun ansible-intelligence-find-references ()
  "Show project-wide indexed Jinja references to the symbol at point."
  (interactive)
  (let* ((backend 'ansible-intelligence-native)
         (id (xref-backend-identifier-at-point backend))
         (refs (and id (xref-backend-references backend id))))
    (unless refs
      (user-error "No indexed references to %s" (or id "symbol at point")))
    (xref--show-xrefs (lambda () refs) nil)))

(defun ansible-intelligence-reindex-project ()
  "Asynchronously rebuild the current Ansible project index."
  (interactive)
  (ansible-intelligence--analyze)
  (ansible-intelligence-index-start
   (or ansible-intelligence--project-index
       (ansible-intelligence-index-attach)))
  (message "Ansible Intelligence project reindex started"))

;;;###autoload
(define-minor-mode ansible-intelligence-mode
  "Provide conservative local Ansible semantic navigation."
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
        (ansible-intelligence-native-enable))
    (remove-hook 'after-change-functions #'ansible-intelligence--schedule-analysis t)
    (ansible-intelligence-native-disable)
    (when (timerp ansible-intelligence--analysis-timer)
      (cancel-timer ansible-intelligence--analysis-timer))
    (setq ansible-intelligence--analysis-timer nil
          ansible-intelligence--analysis-generation
          (1+ ansible-intelligence--analysis-generation))))

(provide 'ansible-intelligence)

;;; ansible-intelligence.el ends here
