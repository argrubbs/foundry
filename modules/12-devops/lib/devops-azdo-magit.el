;;; devops-azdo-magit.el --- Magit sections for Azure DevOps -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded lazily after Magit (see devops-azdo.el) because
;; `magit-insert-section' is a macro that must be known when this file is
;; read or compiled.

;;; Code:

(require 'cl-lib)
(require 'magit)
(require 'magit-section)
(require 'transient)
(require 'devops-azdo)

(defvar magit-azdo-pr-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'azdo-visit-pr-section) m))
(defvar magit-azdo-wi-section-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'azdo-visit-wi-section) m))

(defun azdo-visit-pr-section ()
  "Open the PR at point for review."
  (interactive)
  (azdo-pr-review (alist-get 'pullRequestId (oref (magit-current-section) value))))

(defun azdo-visit-wi-section ()
  "Open the work item at point."
  (interactive)
  (azdo-work-item-open (alist-get 'id (oref (magit-current-section) value))))

(defun azdo-magit-insert-pull-requests ()
  "Magit status section listing open pull requests."
  (when (azdo--ctx)
    (let ((prs (azdo--cached
                'prs
                (lambda (cb) (azdo--request-async
                              "GET" (azdo--url (azdo--git-base) "/pullrequests"
                                               '(("searchCriteria.status" . "active") ("$top" . 25)))
                              (lambda (d) (funcall cb (if (eq d azdo--failed) d (azdo--value d))))))
                (current-buffer))))
      (when prs
        (magit-insert-section (azdo-prs)
          (magit-insert-heading (format "Azure DevOps Pull Requests (%d)" (length prs)))
          (dolist (pr prs)
            (magit-insert-section (azdo-pr pr)
              (insert (azdo--pr-line pr) "\n")))
          (insert "\n"))))))

(defun azdo-magit-insert-work-items ()
  "Magit status section listing Azure Boards items assigned to me."
  (when (azdo--ctx)
    (let ((items (azdo--cached 'work-items #'azdo--fetch-work-items-async
                               (current-buffer))))
      (when items
        (magit-insert-section (azdo-wis)
          (magit-insert-heading (format "Azure Boards: Assigned to Me (%d)" (length items)))
          (dolist (wi items)
            (magit-insert-section (azdo-wi wi)
              (insert (azdo--wi-line wi) "\n")))
          (insert "\n"))))))

(defun azdo--build-face (b)
  (let ((res (alist-get 'result b)) (st (alist-get 'status b)))
    (cond ((equal res "succeeded") 'success)
          ((member res '("failed" "canceled")) 'error)
          ((member st '("inProgress" "notStarted")) 'warning)
          (t 'shadow))))

(defun azdo-magit-insert-builds ()
  "Magit status section with pipeline builds for the current branch / HEAD."
  (when-let* ((ctx (azdo--ctx))
              (branch (azdo--current-branch)))
    (let* ((head (car (azdo--git-lines "rev-parse" "HEAD")))
           (builds (azdo--cached
                    (list 'builds branch)
                    (lambda (cb)
                      (azdo--request-async
                       "GET" (azdo--url (azdo--base ctx) "/build/builds"
                                        `(("repositoryId" . ,(plist-get ctx :repo))
                                          ("repositoryType" . "TfsGit")
                                          ("branchName" . ,(concat "refs/heads/" branch))
                                          ("queryOrder" . "queueTimeDescending") ("$top" . 8)))
                       (lambda (d) (funcall cb (if (eq d azdo--failed) d (azdo--value d))))))
                    (current-buffer))))
      (when builds
        (magit-insert-section (azdo-builds)
          (magit-insert-heading "Azure Pipelines (this branch)")
          (dolist (b builds)
            (magit-insert-section (azdo-build b)
              (insert (propertize (format "%-11s" (or (alist-get 'result b) (alist-get 'status b)))
                                  'face (azdo--build-face b))
                      (format " %s #%s %s%s\n"
                              (alist-get 'name (alist-get 'definition b))
                              (alist-get 'buildNumber b)
                              (if (equal (alist-get 'sourceVersion b) head) "[HEAD] " "")
                              (or (alist-get 'sourceBranch b) "")))))
          (insert "\n"))))))

(progn
  (magit-add-section-hook 'magit-status-sections-hook #'azdo-magit-insert-pull-requests
                          'magit-insert-unpushed-to-upstream-or-recent t)
  (magit-add-section-hook 'magit-status-sections-hook #'azdo-magit-insert-work-items
                          #'azdo-magit-insert-pull-requests t)
  (magit-add-section-hook 'magit-status-sections-hook #'azdo-magit-insert-builds
                          #'azdo-magit-insert-work-items t)
  (define-key magit-status-mode-map (kbd "O") #'azdo-dispatch)
;; `P' is Magit's push menu.  Defining "P c" as a key sequence would turn `P'
  ;; into a prefix map that shadows the push menu, so add the PR commands to
  ;; the menu itself: P c / P l still work and plain P still pushes.
  (transient-append-suffix 'magit-push "e"
    '("c" "Azure DevOps: create pull request" azdo-pr-create))
  (transient-append-suffix 'magit-push "c"
    '("l" "Azure DevOps: list / checkout pull requests" azdo-pr-list))
  (transient-append-suffix 'magit-dispatch "!"
    '("O" "Azure DevOps" azdo-dispatch)))


(provide 'devops-azdo-magit)
;;; devops-azdo-magit.el ends here
