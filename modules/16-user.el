;;; 16-user.el --- User extension point: your packages, tools and config -*- lexical-binding: t; -*-

;;; Commentary:
;; Everything in user/*.el is loaded after the core modules, in alphabetical
;; order, each file isolated so an error in one cannot break the IDE.  Put
;; ordinary `use-package' forms and settings there; packages install into the
;; sandbox like the built-in ones.  user/mise.toml adds CLI tools to the mise
;; install.  Start with IDE_NO_USER=1 to skip all user files.
;;
;; Commands: `ide-user-add-package', `ide-user-install-example',
;; `ide-user-new-file', `ide-user-open', `ide-user-reload'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function elpaca-process-queues "elpaca")
(declare-function ide-mise-setup "00-mise")
(defvar ide-keys-mode-map)

(defvar ide-user-dir (expand-file-name "user/" user-emacs-directory)
  "Directory for the user's own packages and configuration.")
(defvar ide-examples-dir (expand-file-name "examples/" user-emacs-directory)
  "Shipped examples that can be copied into `ide-user-dir'.")
(defvar ide-user-eval-new-forms t
  "When non-nil, `ide-user-add-package' evaluates the new form right away (set nil in tests).")
(defvar ide-user--core-keymap nil
  "Copy of `ide-keys-mode-map' taken just before the first user file loads.")
(defvar ide-user-load-errors nil "Alist of (FILE . MESSAGE) for user files that failed to load.")

;;;; Loading -----------------------------------------------------------------

(defun ide-user-files ()
  "User configuration files in load order."
  (when (file-directory-p ide-user-dir)
    (sort (directory-files ide-user-dir t "\\`[^.#].*\\.el\\'") #'string<)))

(defun ide-user--load (file)
  "Load FILE; report, but never propagate, errors."
  (condition-case err
      (progn (load (file-name-sans-extension file) nil 'nomessage) t)
    (error
     (push (cons file (error-message-string err)) ide-user-load-errors)
     (display-warning 'ide (format "user/%s failed: %s" (file-name-nondirectory file)
                                   (error-message-string err))
                      :error)
     nil)))

(defun ide-user-core-keymap ()
  "The IDE keymap as the core defines it, without keys the user added or changed."
  (or ide-user--core-keymap ide-keys-mode-map))

(defun ide-user-safe-mode-p ()
  "Non-nil when user files are skipped ($IDE_NO_USER is set)."
  (let ((v (getenv "IDE_NO_USER"))) (and v (not (string-empty-p v)))))

(defun ide-user-load-all ()
  "Load every user file unless $IDE_NO_USER is set.  Return the number loaded."
  (setq ide-user-load-errors nil)
  (unless ide-user--core-keymap (setq ide-user--core-keymap (copy-keymap ide-keys-mode-map)))
  (if (ide-user-safe-mode-p)
      (progn (add-hook 'emacs-startup-hook
                       (lambda () (message "Safe mode: user/ files were not loaded (IDE_NO_USER is set)")))
             0)
    (cl-count-if #'ide-user--load (ide-user-files))))

;;;###autoload
(defun ide-user-reload ()
  "Reload the user files and start installing any newly declared packages."
  (interactive)
  (let ((n (ide-user-load-all)))
    (when (fboundp 'elpaca-process-queues) (elpaca-process-queues))
    (message "Loaded %d user file(s)%s" n
             (if ide-user-load-errors (format "; %d failed (see *Warnings*)" (length ide-user-load-errors)) ""))))

;;;###autoload
(defun ide-user-open ()
  "Open the user configuration directory."
  (interactive)
  (make-directory ide-user-dir t)
  (dired ide-user-dir))

;;;; Adding packages -------------------------------------------------------------

(defun ide-user--package-form (name &optional repo demand)
  "Return the `use-package' form text for package NAME.
REPO is \"owner/name\" (GitHub) or a full git URL for packages that are not
on MELPA/ELPA; DEMAND loads the package at startup instead of on first use."
  (concat (format "(use-package %s\n" name)
          (cond ((null repo) "")
                ((string-match-p "\\`[[:alnum:]_.-]+/[[:alnum:]_.-]+\\'" repo)
                 (format "  :ensure (:host github :repo %S)\n" repo))
                (t (format "  :ensure (:repo %S)\n" repo)))
          (if demand "  :demand t\n" "")
          "  ;; :bind ((\"C-c x y\" . some-command))\n"
          "  ;; :init (setq some-package-dir (expand-file-name \"some-package/\" ide-var-dir))\n"
          "  )\n"))

(defun ide-user--valid-package-name-p (name)
  (let ((case-fold-search nil))
    (string-match-p "\\`[a-z0-9][a-z0-9+_.-]*\\'" name)))

;;;###autoload
(defun ide-user-add-package (name &optional repo demand)
  "Add package NAME to user/packages.el and start installing it.
REPO (optional) is \"owner/name\" or a git URL for packages not on MELPA/ELPA.
With a prefix argument, load the package at startup instead of on first use."
  (interactive
   (list (string-trim (read-string "Package name: "))
         (let ((r (string-trim (read-string "Git repo (owner/name or URL; empty = MELPA/ELPA): "))))
           (and (not (string-empty-p r)) r))
         current-prefix-arg))
  (unless (ide-user--valid-package-name-p name) (user-error "Invalid package name: %S" name))
  (let ((file (expand-file-name "packages.el" ide-user-dir)))
    (make-directory ide-user-dir t)
    ;; Never leave Emacs asking "file changed on disk": refuse to clobber unsaved
    ;; edits, and silently refresh an unmodified visiting buffer afterwards.
    (when-let* ((buf (find-buffer-visiting file)))
      (when (buffer-modified-p buf)
        (user-error "Save or revert %s first; it has unsaved changes" (file-name-nondirectory file))))
    (when (and (file-exists-p file)
               (string-match-p (format "(use-package %s\\([ \n)]\\)" (regexp-quote name))
                               (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (user-error "%s is already configured in %s" name file))
    (let ((form (ide-user--package-form name repo demand)))
      (with-temp-buffer
        (when (file-exists-p file) (insert-file-contents file))
        (when (zerop (buffer-size))
          (insert ";;; packages.el --- My packages -*- lexical-binding: t; -*-\n\n;;; Code:\n"))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n" form)
        (write-region nil nil file nil 'silent))
      (when ide-user-eval-new-forms
        (eval (car (read-from-string form)) t)
        (when (fboundp 'elpaca-process-queues) (elpaca-process-queues)))
      (when-let* ((buf (find-buffer-visiting file)))
        (with-current-buffer buf (revert-buffer t t t)))
      (message "Added %s to %s; installing in the background (M-x elpaca-log to watch)" name file)
      file)))

;;;; New files and examples ---------------------------------------------------------

;;;###autoload
(defun ide-user-new-file (name)
  "Create user/NAME.el from a template and open it."
  (interactive "sName for the new file (for example rust): ")
  (unless (string-match-p "\\`[a-zA-Z0-9][a-zA-Z0-9_.-]*\\'" name) (user-error "Invalid file name: %S" name))
  (let ((file (expand-file-name (concat (string-remove-suffix ".el" name) ".el") ide-user-dir)))
    (make-directory ide-user-dir t)
    (when (file-exists-p file) (user-error "%s already exists" file))
    (with-temp-file file
      (insert (format ";;; %s --- My %s configuration -*- lexical-binding: t; -*-\n\n;;; Commentary:\n;; Loaded after the core modules, in alphabetical order.\n\n;;; Code:\n\n"
                      (file-name-nondirectory file) (file-name-sans-extension (file-name-nondirectory file)))))
    (find-file file)
    (goto-char (point-max))
    file))

(defun ide-user--example-names ()
  "Base names of the shipped examples (rust for rust.el.example and rust.mise.example)."
  (when (file-directory-p ide-examples-dir)
    (sort (delete-dups
           (mapcar (lambda (f) (car (split-string (file-name-nondirectory f) "\\.")))
                   (directory-files ide-examples-dir t "\\.example\\'")))
          #'string<)))

(defun ide-user--merge-mise-tools (example-text)
  "Add the tools of EXAMPLE-TEXT to user/mise.toml, keeping what is already there."
  (let* ((file (expand-file-name "mise.toml" ide-user-dir))
         (new (cl-loop for line in (split-string example-text "\n")
                       when (string-match "\\`\\(\"[^\"]+\"\\|[A-Za-z0-9_:.@/-]+\\) *= *" line) collect line))
         (existing (if (file-exists-p file) (with-temp-buffer (insert-file-contents file) (buffer-string)) ""))
         (have (cl-loop for line in (split-string existing "\n")
                        when (string-match "\\`\\(\"[^\"]+\"\\|[A-Za-z0-9_:.@/-]+\\) *=" line) collect (match-string 1 line)))
         (add (cl-remove-if (lambda (l) (string-match "\\`\\(\"[^\"]+\"\\|[A-Za-z0-9_:.@/-]+\\) *=" l)
                              (member (match-string 1 l) have))
                            new)))
    (when add
      (with-temp-file file
        (if (string-match "^\\[tools\\] *$" existing)
            (insert (replace-match (concat "[tools]\n" (string-join add "\n")) t t existing))
          (insert existing (if (string-empty-p existing) "" "\n") "[tools]\n" (string-join add "\n") "\n"))))
    (length add)))

;;;###autoload
(defun ide-user-install-example (name)
  "Copy the shipped example NAME into user/ (Emacs Lisp and/or mise tools)."
  (interactive (list (completing-read "Example: " (ide-user--example-names) nil t)))
  (make-directory ide-user-dir t)
  (let (copied tools)
    (dolist (f (directory-files ide-examples-dir t (format "\\`%s\\..*\\.example\\'" (regexp-quote name))))
      (let ((base (file-name-nondirectory f)))
        (cond
         ((string-suffix-p ".el.example" base)
          (let ((dest (expand-file-name (string-remove-suffix ".example" base) ide-user-dir)))
            (if (file-exists-p dest) (message "Keeping your existing %s" dest)
              (copy-file f dest) (push dest copied))))
         ((string-suffix-p ".mise.example" base)
          (setq tools (+ (or tools 0) (ide-user--merge-mise-tools (with-temp-buffer (insert-file-contents f) (buffer-string)))))))))
    (when copied (find-file (car copied)))
    (message "Installed example %s: %d file(s), %d tool(s) added" name (length copied) (or tools 0))
    (when (and tools (> tools 0) (fboundp 'ide-mise-setup)
               (y-or-n-p "Install the new tools now with mise? "))
      (ide-mise-setup))
    copied))

;;;; Keys ---------------------------------------------------------------------------------

(with-eval-after-load '10-keybindings
  (define-key ide-keys-mode-map (kbd "C-c u a") #'ide-user-add-package)
  (define-key ide-keys-mode-map (kbd "C-c u i") #'ide-user-install-example)
  (define-key ide-keys-mode-map (kbd "C-c u n") #'ide-user-new-file)
  (define-key ide-keys-mode-map (kbd "C-c u e") #'ide-user-open)
  (define-key ide-keys-mode-map (kbd "C-c u r") #'ide-user-reload))

(provide '16-user)
;;; 16-user.el ends here
