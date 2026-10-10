;;; 00-lang-lib.el --- Shared helpers for the language modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads first among the language modules (the name sorts before lang-*).  What
;; every language needs, written once:
;;
;; - `ide-lang-run'              run a command in the bottom drawer
;; - `ide-lang-define-linter'    an external linter as a Flymake backend (Eglot manages
;;                               one server per buffer; Flymake merges any number of linters)
;; - `ide-lang-node-exe'         project-local node_modules/.bin tool, else PATH
;; - `ide-lang-ancestor'         nearest tree-sitter ancestor of a given type
;; - `ide-lang-fetch-extract'    download an archive into the sandbox
;; - `ide-lang-browse'           show a URL or file in the in-Emacs browser

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'compile)
(require 'project)
(require 'flymake)

(defvar ide-var-dir)
(declare-function ide-add-drawer-buffer "02-layout")
(declare-function ide-docs--use-xwidget-p "15-docs")
(declare-function xwidget-webkit-browse-url "xwidget")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-type "treesit")

;;;; Running ------------------------------------------------------------------------

(defconst ide-lang-run-buffer-regexp "\\*run: .*\\*"
  "Buffers made by `ide-lang-run' dock into the bottom drawer.")

(when (fboundp 'ide-add-drawer-buffer)
  (ide-add-drawer-buffer ide-lang-run-buffer-regexp))

(defun ide-lang-root (&optional dir)
  "Project root for DIR (default `default-directory'), or DIR itself."
  (let ((default-directory (file-name-as-directory (or dir default-directory))))
    (if-let* ((p (project-current))) (project-root p) default-directory)))

(defun ide-lang--command-string (command)
  (if (stringp command) command (mapconcat #'shell-quote-argument (delq nil command) " ")))

(defun ide-lang-run (name command &optional dir comint)
  "Run COMMAND (a string, or a list of arguments) in the drawer as *run: NAME*.
It runs in DIR (default: the project root).  Modified project buffers are saved
first.  With COMINT the buffer accepts input, for programs that read stdin."
  (let ((default-directory (file-name-as-directory (or dir (ide-lang-root))))
        (compilation-buffer-name-function (lambda (_mode) (format "*run: %s*" name)))
        (compilation-ask-about-save nil))
    (compile (ide-lang--command-string command) comint)))

(defun ide-lang-exe-or-error (name &optional hint)
  "Path to executable NAME, or a `user-error' mentioning HINT."
  (or (executable-find name)
      (user-error "%s not found%s" name (if hint (concat "; " hint) "; run M-x ide-mise-setup"))))

(defun ide-lang-node-exe (name &optional dir)
  "Executable NAME from the nearest node_modules/.bin above DIR, else on PATH."
  (or (when-let* ((root (locate-dominating-file (or dir default-directory) "node_modules"))
                  (f (expand-file-name (concat "node_modules/.bin/" name) root)))
        (and (file-executable-p f) f))
      (executable-find name)))

;;;; Flymake linters ----------------------------------------------------------------------

(defvar-local ide-lang--lint-procs nil "Alist (LINTER . PROCESS) of the running lint processes.")

(defun ide-lang-severity (string)
  "Flymake diagnostic type for the severity STRING a tool printed."
  (let ((s (downcase (or string ""))))
    (cond ((string-match-p "\\`\\(error\\|err\\|fatal\\|e\\|2\\)\\'" s) :error)
          ((string-match-p "\\`\\(warn\\|warning\\|w\\|1\\)\\'" s) :warning)
          (t :note))))

(defun ide-lang-diag (buffer linter line col severity message &optional end-col)
  "A Flymake diagnostic in BUFFER from LINTER at LINE and COL (1-based; COL may be nil).
SEVERITY is a string or a Flymake type; END-COL, when given, ends the region."
  (let* ((type (if (keywordp severity) severity (ide-lang-severity severity)))
         (region (with-current-buffer buffer
                   (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (forward-line (max 0 (1- line)))
                       (let* ((bol (line-beginning-position)) (eol (line-end-position))
                              (beg (if (and col (> col 0)) (min eol (+ bol (1- col))) bol))
                              (end (cond ((and end-col (> end-col col)) (min eol (+ bol (1- end-col))))
                                         ((and col (< beg eol)) (min eol (1+ beg)))
                                         (t eol))))
                         (cons beg (if (= beg end) (min (point-max) (1+ end)) end))))))))
    (flymake-make-diagnostic buffer (car region) (cdr region) type
                             (format "%s: %s" linter (string-trim message)))))

(defun ide-lang-parse-regexp (linter regexp &rest groups)
  "A parser for LINTER output where each match of REGEXP is one diagnostic.
GROUPS is a plist of regexp group numbers: :line :col :severity :message
(:severity may be omitted, giving warnings)."
  (lambda (output buffer)
    (let (diags)
      (with-temp-buffer
        (insert output)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (let ((line (string-to-number (or (match-string (plist-get groups :line)) "1")))
                (col (when-let* ((g (plist-get groups :col)) (m (match-string g)))
                       (string-to-number m)))
                (sev (if-let* ((g (plist-get groups :severity))) (match-string g) "warning"))
                (msg (match-string (plist-get groups :message))))
            (push (ide-lang-diag buffer linter line col sev msg) diags))))
      (nreverse diags))))

(cl-defun ide-lang-define-linter (name &key modes program args parse (stdin t) (saved-only nil))
  "Define Flymake backend `ide-lang-flymake-NAME' for an external linter and
enable it in the mode hooks of MODES.

PROGRAM is an executable name, or a function returning a path (or nil).
ARGS is a list of strings (\"%f\" stands for the file name) or a function of the
file name.  PARSE is called with the output and the source buffer and returns
diagnostics.  STDIN non-nil sends the buffer to the process, so unsaved edits are
linted; otherwise the saved file is read, and with SAVED-ONLY the backend stays
quiet while the buffer has unsaved changes."
  (let ((fn (intern (format "ide-lang-flymake-%s" name))))
    (defalias fn
      (lambda (report-fn &rest _)
        (let ((exe (if (functionp program) (funcall program) (executable-find program)))
              (src (current-buffer)))
          (when-let* ((old (alist-get name ide-lang--lint-procs)))
            (when (process-live-p old) (kill-process old)))
          (cond
           ((not exe) (funcall report-fn nil))
           ((and saved-only (buffer-modified-p)) (funcall report-fn nil))
           (t
            (let* ((file (buffer-file-name))
                   (argv (cons exe (if (functionp args) (funcall args file)
                                     (mapcar (lambda (a) (if (equal a "%f") (or file "-") a)) args))))
                   (input (and stdin (buffer-substring-no-properties (point-min) (point-max))))
                   (out (generate-new-buffer (format " *lint-%s*" name)))
                   (default-directory (ide-lang-root))
                   proc)
              (setq proc
                    (make-process
                     :name (format "lint-%s" name) :buffer out :command argv
                     :connection-type 'pipe :noquery t
                     :sentinel
                     (lambda (p _event)
                       (when (memq (process-status p) '(exit signal))
                         (unwind-protect
                             (when (and (buffer-live-p src)
                                        (eq p (alist-get name (buffer-local-value 'ide-lang--lint-procs src))))
                               (funcall report-fn
                                        (condition-case err
                                            (funcall parse (with-current-buffer out (buffer-string)) src)
                                          (error (flymake-log :warning "%s parser: %s" name (error-message-string err))
                                                 nil))))
                           (kill-buffer out))))))
              (setf (alist-get name ide-lang--lint-procs) proc)
              (when stdin
                (process-send-string proc input)
                (process-send-eof proc))))))))
    (dolist (mode modes)
      (let ((hook (intern (format "%s-hook" mode))))
        (add-hook hook (lambda ()
                         (add-hook 'flymake-diagnostic-functions fn nil t)
                         (unless (bound-and-true-p flymake-mode) (flymake-mode 1))))))
    fn))

;;;; Tree-sitter helpers ----------------------------------------------------------------------

(defun ide-lang-ancestor (types &optional node)
  "Nearest ancestor-or-self of NODE (default: the node at point) whose type is in TYPES."
  (let ((n (or node (treesit-node-at (point)))))
    (while (and n (not (member (treesit-node-type n) types)))
      (setq n (treesit-node-parent n)))
    n))

;;;; Downloads and browsing -------------------------------------------------------------------

(defun ide-lang-tools-dir (&rest parts)
  "A path under var/tools/ in the sandbox (created on demand)."
  (let ((dir (apply #'expand-file-name (string-join parts "/")
                    (list (expand-file-name "tools/" ide-var-dir)))))
    (make-directory (file-name-directory (directory-file-name dir)) t)
    dir))

(defun ide-lang-github-asset-url (repo regexp)
  "URL of the latest release asset of REPO (owner/name) whose name matches REGEXP."
  (with-temp-buffer
    (unless (zerop (call-process "curl" nil t nil "-fsSL" "--max-time" "20"
                                 (format "https://api.github.com/repos/%s/releases/latest" repo)))
      (user-error "Could not query the latest release of %s" repo))
    (goto-char (point-min))
    (let ((assets (alist-get 'assets (json-parse-buffer :object-type 'alist :array-type 'list))))
      (or (cl-loop for a in assets
                   when (string-match-p regexp (alist-get 'name a))
                   return (alist-get 'browser_download_url a))
          (user-error "No asset matching %s in the latest %s release" regexp repo)))))

(defun ide-lang-fetch-extract (name url dir &optional kind)
  "Download URL and extract it into DIR, showing progress in the drawer.
KIND is `tar' (a .tar.gz, the default) or `zip'."
  (make-directory dir t)
  (ide-lang-run (format "install %s" name)
                (if (eq kind 'zip)
                    (format "curl -fsSL -o %s.zip %s && unzip -qo %s.zip -d %s && rm %s.zip && echo done"
                            (shell-quote-argument (expand-file-name "download" dir)) (shell-quote-argument url)
                            (shell-quote-argument (expand-file-name "download" dir)) (shell-quote-argument dir)
                            (shell-quote-argument (expand-file-name "download" dir)))
                  (format "curl -fsSL %s | tar -xz -C %s && echo done"
                          (shell-quote-argument url) (shell-quote-argument dir)))
                dir))

(defun ide-lang-browse (url)
  "Show URL in the in-Emacs browser (xwidget WebKit) beside the code, else the system browser."
  (if (and (fboundp 'ide-docs--use-xwidget-p) (ide-docs--use-xwidget-p))
      (let ((display-buffer-overriding-action
             '(display-buffer-in-side-window (side . right) (slot . 2) (window-width . 0.45))))
        (xwidget-webkit-browse-url url))
    (browse-url url)))

(provide 'lang-lib)
;;; 00-lang-lib.el ends here
