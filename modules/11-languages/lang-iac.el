;;; lang-iac.el --- Terraform / Packer HCL2 -*- lexical-binding: t; -*-

;;; Commentary:
;; terraform-mode + terraform-ls, hcl-mode for Packer, `iac-dispatch'.

;;; Code:

(eval-when-compile (require 'eglot))

(require 'transient)
(require 'compile)

(use-package hcl-mode
  :mode ("\\.hcl\\'" "\\.pkr\\.hcl\\'" "\\.pkrvars\\.hcl\\'"))

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tfvars\\'")
  :hook (terraform-mode . terraform-format-on-save-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(terraform-mode . ("terraform-ls" "serve"))))

(defun ide--iac-run (name &rest command)
  "Run COMMAND in the bottom drawer in buffer *iac-NAME*."
  (let ((compilation-buffer-name-function (lambda (_) (format "*iac-%s*" name)))
        (default-directory (or (when-let* ((p (project-current))) (project-root p))
                               default-directory)))
    (compile (mapconcat #'identity command " "))))

;;;###autoload (autoload 'iac-dispatch "lang-iac" nil t)
(transient-define-prefix iac-dispatch ()
  "Run Terraform / Packer commands in the bottom drawer."
  [["Terraform"
    ("i" "init"     (lambda () (interactive) (ide--iac-run "terraform" "terraform" "init")))
    ("v" "validate" (lambda () (interactive) (ide--iac-run "terraform" "terraform" "validate")))
    ("f" "fmt"      (lambda () (interactive) (ide--iac-run "terraform" "terraform" "fmt" "-recursive")))
    ("p" "plan"     (lambda () (interactive) (ide--iac-run "terraform" "terraform" "plan")))
    ("a" "apply"    (lambda () (interactive)
                      (when (yes-or-no-p "terraform apply? ")
                        (ide--iac-run "terraform" "terraform" "apply"))))]
   ["Packer"
    ("P i" "init"     (lambda () (interactive) (ide--iac-run "packer" "packer" "init" ".")))
    ("P v" "validate" (lambda () (interactive) (ide--iac-run "packer" "packer" "validate" ".")))
    ("P b" "build"    (lambda () (interactive) (ide--iac-run "packer" "packer" "build" ".")))]])

(provide 'lang-iac)
;;; lang-iac.el ends here
