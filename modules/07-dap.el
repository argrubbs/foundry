;;; 07-dap.el --- Visual debugger (dap-mode) -*- lexical-binding: t; -*-

;;; Commentary:
;; dap-mode with UI panes routed by `display-buffer-alist' (see 02-layout).
;; dap-mode is lsp-mode based; it is installed for the UI/breakpoint
;; machinery and its dap-python / dap-go adapters, used standalone with Eglot.

;;; Code:

(declare-function dap-ui-breakpoints "dap-ui")
(declare-function dap-ui-locals "dap-ui")
(declare-function dap-ui-sessions "dap-ui")

(use-package dap-mode
  :commands (dap-debug dap-debug-last dap-breakpoint-toggle dap-hydra dap-ui-mode)
  :init
  (setq dap-breakpoints-file (expand-file-name "dap-breakpoints" ide-var-dir)
        dap-utils-extension-path (expand-file-name "dap-extensions/" ide-var-dir)
        dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip)
        dap-ui-buffer-configurations
        `((,"*dap-ui-locals*" . ((side . right) (slot . 1) (window-width . 40)))
          (,"*dap-ui-expressions*" . ((side . right) (slot . 2) (window-width . 40)))
          (,"*dap-ui-sessions*" . ((side . bottom) (slot . 1) (window-height . 0.3)))
          (,"*dap-ui-breakpoints*" . ((side . bottom) (slot . 2) (window-height . 0.3)))
          (,"*dap-ui-repl*" . ((side . bottom) (slot . 3) (window-height . 0.3)))))
  :config
  (dap-auto-configure-mode 1)
  (dap-ui-controls-mode 1))

(defun ide-toggle-debug-window ()
  "Toggle the debug tool window."
  (interactive)
  (require 'dap-ui)
  (if (get-buffer-window "*dap-ui-locals*")
      (progn (dap-ui-locals) (dolist (b '("*dap-ui-locals*" "*dap-ui-sessions*" "*dap-ui-breakpoints*"))
                               (when-let* ((w (get-buffer-window b))) (delete-window w))))
    (dap-ui-sessions)
    (dap-ui-locals)
    (dap-ui-breakpoints)))

;; Clicking the left fringe/margin toggles breakpoints via dap-mode's own
;; `dap-mode-mouse-set-clear-breakpoint' binding (enabled by dap-auto-configure).

(provide '07-dap)
;;; 07-dap.el ends here
