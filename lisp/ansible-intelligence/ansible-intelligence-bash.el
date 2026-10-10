;;; ansible-intelligence-bash.el --- Optional structural shell ranges -*- lexical-binding: t; -*-

;;; Commentary:
;; Bash is an optional guest grammar.  This module records only ownership and
;; parser availability; shell variables never enter the Ansible index.

;;; Code:

(require 'ansible-intelligence-model)
(require 'treesit nil t)

(defcustom ansible-intelligence-bash-enable-parser t
  "Whether to probe the optional Bash grammar for owned block scalars."
  :type 'boolean
  :group 'ansible-intelligence)

(defun ansible-intelligence-bash-grammar-available-p ()
  "Return non-nil when the installed Bash tree-sitter grammar is usable."
  (and ansible-intelligence-bash-enable-parser
       (fboundp 'treesit-language-available-p)
       (condition-case nil
           (treesit-language-available-p 'bash)
         (error nil))))

(defun ansible-intelligence-bash--body (text)
  "Return the shell body from YAML block scalar TEXT."
  (if-let* ((newline (string-match "\n" text)))
      (substring text (1+ newline))
    ""))

(defun ansible-intelligence-bash-parse-text (text)
  "Probe Bash parsing for TEXT and return copied structural status.

No tree-sitter node is retained after this function returns, and no semantic
shell identifiers are extracted."
  (when (ansible-intelligence-bash-grammar-available-p)
    (condition-case nil
        (with-temp-buffer
          (insert (ansible-intelligence-bash--body text))
          (treesit-parser-create 'bash)
          (list :language 'bash
                :root-type
                (treesit-node-type (treesit-buffer-root-node 'bash))))
      (error nil))))

(defun ansible-intelligence-bash-materialize-ranges (ranges)
  "Annotate structural RANGES with optional Bash parser availability."
  (mapcar
   (lambda (range)
     (if (and (ansible-intelligence-shell-range-p range)
              (ansible-intelligence-bash-grammar-available-p))
         (let* ((bounds (ansible-intelligence-shell-range-range range))
                (text (buffer-substring-no-properties
                       (ansible-intelligence-range-start bounds)
                       (ansible-intelligence-range-end bounds)))
                (parsed (ansible-intelligence-bash-parse-text text)))
           (setf (ansible-intelligence-shell-range-parser range)
                 (and parsed (plist-get parsed :language)))
           range)
       range))
   ranges))

(provide 'ansible-intelligence-bash)

;;; ansible-intelligence-bash.el ends here
