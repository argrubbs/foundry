;;; init.el --- Foundry: a sandboxed Emacs IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; Bootstraps elpaca inside the sandbox, then loads modules in order.

;;; Code:

;; Batch runs skip early-init.el; load it so tests and `make tools' see the same setup.
(unless (bound-and-true-p ide--early-init-loaded)
  (load (expand-file-name "early-init" user-emacs-directory) nil t))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Elpaca dates the Emacs core from a table of releases and warns, assigning the
;; last release's date, for a development build such as 31.1.50.  A development
;; build is newer than that release, so give Elpaca the build date instead: from
;; `emacs-build-time', or, when the distribution strips it (reproducible builds),
;; from the date of the emacs binary, which for a packaged build is the build date.
(when (and (> (length (version-to-list emacs-version)) 2)
           (not (boundp 'elpaca-core-date)))
  (when-let* ((time (or emacs-build-time
                        (file-attribute-modification-time
                         (file-attributes (expand-file-name invocation-name invocation-directory))))))
    (defvar elpaca-core-date (list (string-to-number (format-time-string "%Y%m%d" time)))
      "List (N) with N the YYYYMMDD date of this Emacs build; see init.el.")))

;;;; Elpaca bootstrap
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process
                                 `("git" nil ,buffer t "clone"
                                   ,@(when-let* ((depth (plist-get order :depth)))
                                       (list (format "--depth=%d" depth) "--no-single-branch"))
                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Batch runs (`-l init.el') load us after `after-init-hook' has fired.
(when after-init-time (elpaca-process-queues))

;; Lockfile lives in the sandbox.
(setq elpaca-lock-file (expand-file-name "elpaca.lock" user-emacs-directory))

(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))
(elpaca-wait)

(setq use-package-always-defer t
      load-prefer-newer t)

;; Built-in packages must not be fetched by elpaca.
(defmacro ide-builtin (name &rest body)
  "Configure built-in package NAME with `use-package' BODY, never fetching it."
  (declare (indent 1))
  `(use-package ,name :ensure nil ,@body))

;;;; Module loader
(defvar ide-modules-directory (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path ide-modules-directory)

(defun ide--module-files ()
  "Return module files in load order."
  (let (files)
    (dolist (f (directory-files ide-modules-directory t "\\`[0-9][0-9]-.*"))
      (if (file-directory-p f)
          (setq files (append files (directory-files f t "\\.el\\'")))
        (when (string-suffix-p ".el" f) (setq files (append files (list f))))))
    files))

(dolist (file (ide--module-files))
  (condition-case err
      (progn
        (let ((t0 (float-time)))
          (load (file-name-sans-extension file) nil 'nomessage)
          (when (getenv "IDE_PROFILE")
            (message "[profile] load %-22s %.0f ms" (file-name-nondirectory file)
                     (* 1000 (- (float-time) t0)))))
        nil)
    (error (display-warning 'ide (format "Module %s failed: %s"
                                         (file-name-nondirectory file) (error-message-string err))
                            :error))))

;; The user's own packages and config (user/*.el) load last, each isolated so a
;; mistake there can never break the IDE.  Start with IDE_NO_USER=1 to skip them.
(when (fboundp 'ide-user-load-all) (ide-user-load-all))

;; Interactive sessions let elpaca install/activate packages asynchronously
;; after init (fast first frame); batch runs block so tests are deterministic.
(when noninteractive (elpaca-wait))

(when (getenv "IDE_PROFILE")
  (add-hook 'emacs-startup-hook
            (lambda () (message "[profile] startup %s (gcs %d)" (emacs-init-time) gcs-done))))

(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

(provide 'init)

;; init.el must stay uncompiled: the `elpaca' macro expands into calls that
;; need elpaca.el loaded, which only the (uncompiled) bootstrap guarantees.
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
