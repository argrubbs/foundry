;;; 08-vcs.el --- Magit, credentials, PR refspecs -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit foundation.  The Azure DevOps forge lives in 12-devops/devops-azdo.el.
;; Credential setup is injected into Emacs-spawned git processes only; global
;; git config is never modified.

;;; Code:

(use-package magit
  :commands (magit-status magit-dispatch magit-diff-range magit-log-current)
  :init
  (setq magit-diff-refine-hunk 'all
        magit-save-repository-buffers 'dontask
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        magit-bury-buffer-function #'magit-restore-window-configuration)
  :config
  (setq magit-no-confirm '(stage-all-changes unstage-all-changes)))

(use-package git-timemachine :commands git-timemachine)

(defun ide--azure-url-p (url)
  "Non-nil if remote URL points at Azure DevOps."
  (and url (string-match-p
            "\\(dev\\.azure\\.com\\|ssh\\.dev\\.azure\\.com\\|\\.visualstudio\\.com\\)" url)))

;;;; Credentials: prefer Git Credential Manager, never prompt interactively.
(defun ide--git-credential-env ()
  "Return env entries that make git use GCM non-interactively."
  (append
   (list "GIT_TERMINAL_PROMPT=0" "GCM_INTERACTIVE=never")
   (when (and (executable-find "git-credential-manager")
              (null (ignore-errors
                      (process-lines "git" "config" "--get" "credential.helper"))))
     (list "GIT_CONFIG_COUNT=1" "GIT_CONFIG_KEY_0=credential.helper"
           "GIT_CONFIG_VALUE_0=manager"))))

(with-eval-after-load 'magit-process
  (advice-add 'magit-start-process :around
              (lambda (fn &rest args)
                (let ((process-environment
                       (append (ide--git-credential-env) process-environment)))
                  (apply fn args)))))

;;;; Pull-request refspec sync for Azure DevOps remotes.
(defvar ide--refspec-checked nil "Repos already checked for the PR refspec.")

(defun ide-vcs-ensure-azdo-pr-refspec ()
  "Make Azure DevOps remotes of the current repo fetch PR merge refs."
  (interactive)
  (when-let* ((root (ignore-errors (car (process-lines "git" "rev-parse" "--show-toplevel")))))
    (unless (member root ide--refspec-checked)
      (push root ide--refspec-checked)
      (let ((default-directory root))
        (dolist (remote (ignore-errors (process-lines "git" "remote")))
          (let ((url (ignore-errors
                       (car (process-lines "git" "config" "--get" (format "remote.%s.url" remote)))))
                (spec (format "+refs/pull/*/merge:refs/remotes/%s/pr/*" remote)))
            (when (and (ide--azure-url-p url)
                       (not (member spec (ignore-errors
                                           (process-lines "git" "config" "--get-all"
                                                          (format "remote.%s.fetch" remote))))))
              (if (zerop (call-process "git" nil nil nil "config" "--add"
                                       (format "remote.%s.fetch" remote) spec))
                  (message "Added PR refspec to remote %s" remote)
                (display-warning 'ide (format "Could not add PR refspec to remote %s" remote)
                                 :warning)))))))))

(add-hook 'magit-status-mode-hook #'ide-vcs-ensure-azdo-pr-refspec)

(provide '08-vcs)
;;; 08-vcs.el ends here
