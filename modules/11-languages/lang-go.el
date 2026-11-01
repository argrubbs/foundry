;;; lang-go.el --- Go (IDE parity) -*- lexical-binding: t; -*-

;;; Commentary:
;; gopls, delve via dap-dlv-go (F5 debugs the package or its tests), gofmt/goimports on
;; save, go-test in the drawer.

;;; Code:

(require '00-lang-lib)

(defun ide--go-organize-imports ()
  "Organize imports through gopls."
  (ide-lang-code-action "source.organizeImports"))

;; Eglot ships the gopls entry for the Go modes.
(ide-lang-setup '(go-mode go-ts-mode go-mod-ts-mode)
  :tab-width 4
  :indent-tabs nil
  :format-on-save nil
  :config (lambda ()
            (add-hook 'before-save-hook #'ide--go-organize-imports -10 t)))

(defun ide-go--run-command ()
  "S-<f10>: run the package, or the tests for a _test.go file."
  (when (derived-mode-p 'go-ts-mode)
    (if (string-suffix-p "_test.go" (or buffer-file-name "")) "go test ./..." "go run .")))

(defvar dap-dlv-go-delve-path)
(declare-function dap-debug "dap-mode")

(defun ide-go--debug-launch (file)
  "`dap-debug' launch plist for FILE: its package's tests for a _test.go file, else
the package (a main package) itself.  Delve serves the Debug Adapter Protocol itself
\(dlv dap), which dap-dlv-go starts."
  (let ((dir (file-name-directory file))
        (test (string-suffix-p "_test.go" file)))
    (list :type "go" :request "launch"
          :name (format "Go: %s %s" (if test "test" "debug")
                        (file-name-nondirectory (directory-file-name dir)))
          :mode (if test "test" "debug")
          :program dir :cwd dir
          :buildFlags "")))

(defun ide-go-debug ()
  "Debug the current Go package (or its tests, from a _test.go file) with delve."
  (interactive)
  (ide-lang-require-file)
  (require 'dap-mode)
  (require 'dap-dlv-go)
  ;; Looked up now, not when dap-dlv-go loaded: mise may put dlv on PATH later.
  (setq dap-dlv-go-delve-path (ide-lang-exe-or-error "dlv"))
  (dap-debug (ide-go--debug-launch buffer-file-name)))

;; Its launch templates (M-x dap-debug) too.
(with-eval-after-load 'dap-mode (require 'dap-dlv-go))

(ide-lang-register '(go-mode go-ts-mode)
  :run #'ide-go--run-command
  :debug (lambda () (when (derived-mode-p 'go-ts-mode 'go-mode) (ide-go-debug) t)))

(provide 'lang-go)
;;; lang-go.el ends here
