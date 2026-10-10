;;; org-roam.el --- Zettelkasten with sandboxed database -*- lexical-binding: t; -*-

;;; Commentary:
;; org-roam v2; db at var/org-roam.db, notes under notes/.

;;; Code:

(use-package org-roam
  :commands (org-roam-node-find org-roam-node-insert org-roam-buffer-toggle
             org-roam-capture org-roam-tag-add org-roam-db-sync)
  :init
  (setq org-roam-directory (expand-file-name "roam/" ide-notes-dir)
        org-roam-db-location (expand-file-name "org-roam.db" ide-var-dir)
        org-roam-v2-ack t
        org-roam-completion-everywhere t)
  (make-directory (expand-file-name "roam/" ide-notes-dir) t)
  :config
  (org-roam-db-autosync-mode 1)
  ;; Backlinks buffer docks into the right side drawer.
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right) (slot . 2) (window-width . 0.25)
                 (window-parameters . ((no-delete-other-windows . t))))))

(use-package org-roam-ui :commands org-roam-ui-mode)

(provide 'ide-org-roam)
;;; org-roam.el ends here
