;;; lang-powershell-test.el --- Tests for modules/11-languages/lang-powershell.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-powershell-test.el -f ert-run-tests-batch-and-exit
;; Pure logic plus a few quick pwsh calls; the PSES, Pester and DAP round trips were checked by hand.

;;; Code:

(require 'ert)
(require 'lang-powershell nil t)

(ert-deftest powershell-menu-and-hooks-registered ()
  (should (eq (cdr (assoc '(powershell-mode) ide-lang-menus)) #'ide-powershell-dispatch))
  (should (memq #'ide-powershell--run-command ide-run-command-functions))
  (should (assoc "\\.ps[dm]?1\\'" auto-mode-alist)))

(ert-deftest powershell-environment-is-contained ()
  (let ((env (ide-powershell--env))
        (home (expand-file-name "tools/pwsh/" ide-var-dir)))
    (dolist (var '("XDG_DATA_HOME" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "DOTNET_CLI_HOME" "TMPDIR"))
      (should (cl-some (lambda (e) (string-prefix-p (concat var "=" home) e)) env)))
    (should (member "POWERSHELL_TELEMETRY_OPTOUT=1" env))
    (should (member "DOTNET_CLI_TELEMETRY_OPTOUT=1" env))))

(ert-deftest powershell-quoting ()
  (should (equal (ide-powershell--quote "it's") "'it''s'")))

(ert-deftest powershell-pester-names-at-point ()
  (with-temp-buffer
    (insert "Describe 'Thing' {\n  Context \"when ready\" {\n    It 'works' {\n      1 | Should -Be 1\n    }\n"
            "    It 'it''s fine' {\n      2\n    }\n  }\n}\n")
    (goto-char (point-min)) (search-forward "Should")
    (should (equal (ide-powershell--names-at-point) '("Thing" "when ready" "works")))
    (search-forward "      2")
    (should (equal (ide-powershell--names-at-point) '("Thing" "when ready" "it's fine")))
    (goto-char (point-min)) (forward-line 1)
    (should (equal (ide-powershell--names-at-point) '("Thing" "when ready")))
    (goto-char (point-max))
    (should-not (ide-powershell--names-at-point))))

(ert-deftest powershell-full-name-filter-escapes-wildcards ()
  (should (equal (ide-powershell--full-name-filter '("A" "b[1]" "c"))
                 "A.b`[1`].c*")))

(ert-deftest powershell-pester-file-detection ()
  (should (ide-powershell--pester-file-p "/x/a.Tests.ps1"))
  (should-not (ide-powershell--pester-file-p "/x/a.ps1")))

(ert-deftest powershell-pses-command-shape ()
  (cl-letf (((symbol-function 'ide-powershell--pses-start-script) (lambda () "/x/Start-EditorServices.ps1"))
            ((symbol-function (quote file-exists-p)) (lambda (_) t))
            ((symbol-function 'executable-find) (lambda (n) (concat "/bin/" n))))
    (let ((argv (ide-powershell--pses-command "-DebugServiceOnly")))
      (should (equal (car argv) "env"))
      (should (member "-Stdio" argv))
      (should (member "-DebugServiceOnly" argv))
      (should (member "-BundledModulesPath" argv)))))

(ert-deftest powershell-debug-provider-fills-defaults ()
  (cl-letf (((symbol-function 'ide-powershell--debug-command) (lambda () '("pwsh")))
            ((symbol-function 'ide-lang-root) (lambda (&optional _) "/p/")))
    (let ((conf (ide-powershell--populate-debug-args '(:script "/p/a.ps1"))))
      (should (equal (plist-get conf :type) "PowerShell"))
      (should (equal (plist-get conf :dap-server-path) '("pwsh")))
      (should (equal (plist-get conf :cwd) "/p/")))))

(ert-deftest powershell-eval-runs-contained-pwsh ()
  (skip-unless (executable-find "pwsh"))
  (should (equal (ide-powershell--eval "'a' + 'b'") "ab"))
  (should (string-prefix-p (expand-file-name "tools/pwsh/" ide-var-dir)
                           (ide-powershell--eval "$env:XDG_DATA_HOME"))))

(provide 'lang-powershell-test)
;;; lang-powershell-test.el ends here
