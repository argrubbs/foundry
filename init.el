;;; init.el --- McGeEmacs: a sandboxed Emacs IDE -*- lexical-binding: t; -*-

;;; Commentary:
;; Sets up package.el inside the sandbox, then loads modules in order.

;;; Code:

;; Batch runs skip early-init.el; load it so tests and `make tools' see the same setup.
(unless (bound-and-true-p ide--early-init-loaded)
  (load (expand-file-name "early-init" user-emacs-directory) nil t))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;;; Packages: package.el with GNU ELPA, NonGNU ELPA and MELPA, kept inside the sandbox.
(require 'package)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory)
      package-gnupghome-dir (expand-file-name "gnupg/" package-user-dir)
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/"))
      ;; MELPA (latest commits) wins, as this config tracks current package code;
      ;; GNU and NonGNU ELPA supply what MELPA does not carry.
      package-archive-priorities '(("melpa" . 3) ("gnu" . 2) ("nongnu" . 1))
      ;; One precomputed file of every package's autoloads makes startup fast.
      package-quickstart t
      package-quickstart-file (expand-file-name "var/package-quickstart.el" user-emacs-directory))
(package-activate-all)

(require 'use-package)
;; Missing packages install when their `use-package' form runs (archives are
;; refreshed on the first miss).  Built-ins say `:ensure nil' (see `ide-builtin').
(setq use-package-always-ensure t)

(setq use-package-always-defer t
      load-prefer-newer t)

;; Built-in packages must never be fetched from an archive.
(defmacro ide-builtin (name &rest body)
  "Configure built-in package NAME with `use-package' BODY, never fetching it."
  (declare (indent 1))
  `(use-package ,name :ensure nil ,@body))

;;;; Module loader
(defvar ide-modules-directory (expand-file-name "modules/" user-emacs-directory))
(add-to-list 'load-path ide-modules-directory)
(add-to-list 'load-path (expand-file-name "11-languages" ide-modules-directory))

(defun ide--module-files ()
  "Return module files in load order.
Language modules (11-languages/lang-*.el) are left out: 01-lang-modes.el loads
each one the first time a file of that language is opened."
  (let (files)
    (dolist (f (directory-files ide-modules-directory t "\\`[0-9][0-9]-.*"))
      (if (file-directory-p f)
          (setq files (append files
                              (seq-remove (lambda (file) (string-prefix-p "lang-" (file-name-nondirectory file)))
                                          (directory-files f t "\\.el\\'"))))
        (when (string-suffix-p ".el" f) (setq files (append files (list f))))))
    files))

(defun ide--module-feature (file)
  "Return the feature symbol provided by module FILE."
  (let ((base (file-name-base file)))
    (cond
     ((string= base "00-lang-lib") 'lang-lib)
     (t (intern base)))))

(dolist (file (ide--module-files))
  (condition-case err
      (let ((feat (ide--module-feature file)))
        (unless (featurep feat)
          (let ((t0 (float-time)))
            (load (file-name-sans-extension file) nil 'nomessage)
            (when (getenv "IDE_PROFILE")
              (message "[profile] load %-22s %.0f ms" (file-name-nondirectory file)
                       (* 1000 (- (float-time) t0))))))
        nil)
    (error (display-warning 'ide (format "Module %s failed: %s"
                                         (file-name-nondirectory file) (error-message-string err))
                            :error))))

;; The user's own packages and config (user/*.el) load last, each isolated so a
;; mistake there can never break the IDE.  Start with IDE_NO_USER=1 to skip them.
(when (fboundp 'ide-user-load-all) (ide-user-load-all))

(when (getenv "IDE_PROFILE")
  (add-hook 'emacs-startup-hook
            (lambda () (message "[profile] startup %s (gcs %d)" (emacs-init-time) gcs-done))))

(defun ide--load-custom-file ()
  "Load `custom-file' if it exists.
It loads after every module, so the user's saved Customize choices win over
the `use-package' bodies (which package.el runs right away)."
  (when (file-exists-p custom-file) (load custom-file nil 'nomessage)))

(ide--load-custom-file)

(provide 'init)

;; init.el stays uncompiled: it is small, and it sets up the package system.
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; init.el ends here
