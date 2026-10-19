;;; 15-docs.el --- Documentation viewer -*- lexical-binding: t; -*-

;;; Commentary:
;; Opens docs/index.html from Emacs and searches offline keybindings.
;;
;;   C-c h d   open documentation in browser / xwidget
;;   C-c h s   search keybinding explorer
;;   C-c h u   update documentation from live configuration (loads docs-update)
;;
;; Heavy generator and updater logic is split into `lisp/docs-update.el' and
;; loaded lazily when `ide-docs-update' is called or during `make docs'.

;;; Code:

(require 'url-util)

(declare-function xwidget-webkit-browse-url "xwidget")
(defvar ide-keys-mode-map)

(defgroup ide-docs nil "Documentation viewer." :group 'tools)

(defcustom ide-docs-file (expand-file-name "docs/index.html" user-emacs-directory)
  "The documentation page." :type 'file)

(defcustom ide-docs-viewer 'auto
  "Where to show the documentation.
`auto' uses an in-Emacs xwidget WebKit view when Emacs supports it, otherwise
`browse-url'.  `xwidget' and `browser' force one or the other."
  :type '(choice (const auto) (const xwidget) (const browser)))

(defconst ide-docs-sections
  '("overview" "quickstart" "layout" "keys" "menus" "workflows" "features"
    "languages" "architecture" "tools" "setup" "status"))

;;;; Viewing ------------------------------------------------------------------

(defun ide-docs--url (&optional section query)
  "Return the file URL for the page, at SECTION and filtered by QUERY."
  (let ((query (if (and query (symbolp query)) (symbol-name query) query)))
    (concat "file://" (url-hexify-string (expand-file-name ide-docs-file) url-path-allowed-chars)
            (when (and query (not (string-empty-p query))) (concat "?q=" (url-hexify-string query)))
            (when (and section (not (string-empty-p section))) (concat "#" section)))))

(defun ide-docs--use-xwidget-p ()
  (pcase ide-docs-viewer
    ('xwidget t)
    ('browser nil)
    (_ (and (display-graphic-p) (featurep 'xwidget-internal) (fboundp 'xwidget-webkit-browse-url)))))

(defun ide-docs--show (url)
  (if (ide-docs--use-xwidget-p) (xwidget-webkit-browse-url url) (browse-url url)))

;;;###autoload
(defun ide-docs-open (&optional section)
  "Open the documentation page.  With a prefix argument, choose a SECTION."
  (interactive (list (when current-prefix-arg (completing-read "Section: " ide-docs-sections nil t))))
  (unless (file-exists-p ide-docs-file)
    (user-error "No documentation at %s; run M-x ide-docs-update" ide-docs-file))
  (ide-docs--show (ide-docs--url section)))

;;;###autoload
(defun ide-docs-search (query)
  "Open the documentation's keybinding explorer filtered by QUERY."
  (interactive (list (read-string "Search documentation: " (thing-at-point 'symbol t))))
  (unless (file-exists-p ide-docs-file) (user-error "No documentation at %s" ide-docs-file))
  (ide-docs--show (ide-docs--url "keys" query)))

;; The updater is lisp/docs-update.el; it defines both commands.
(autoload 'ide-docs-update "docs-update" nil t)
(autoload 'ide-docs-update-facts-only "docs-update" nil t)

;;;; Keys ---------------------------------------------------------------------

(with-eval-after-load '10-keybindings
  (define-key ide-keys-mode-map (kbd "C-c h d") #'ide-docs-open)
  (define-key ide-keys-mode-map (kbd "C-c h s") #'ide-docs-search)
  (define-key ide-keys-mode-map (kbd "C-c h u") #'ide-docs-update))

(provide '15-docs)
;;; 15-docs.el ends here
