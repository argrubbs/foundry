;;; align-test.el --- Tests for ide-align and auto-alignment -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for auto-alignment, smart delimiter splitting, YAML/Ansible
;; indentation, electric list item alignment, and Terraform snippet point preservation.

;;; Code:

(require 'ert)
(require 'ide-align)
(require 'yaml-ts-mode nil t)
(require 'terraform-mode nil t)
(require 'go-ts-mode nil t)
(require 'rust-ts-mode nil t)
(require 'typescript-ts-mode nil t)
(require 'json-ts-mode nil t)
(require 'python)
(require 'yasnippet)

(ert-deftest ide-align-test-smart-return-delimiters ()
  "Test delimiter splitting on RET across various modes."
  ;; Terraform
  (with-temp-buffer
    (terraform-mode)
    (insert "resource \"aws_s3_bucket\" \"b\" {}")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "resource \"aws_s3_bucket\" \"b\" {\n  \n}"))
    (should (= (current-column) 2)))

  ;; TypeScript
  (with-temp-buffer
    (typescript-ts-mode)
    (insert "function test() {}")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "function test() {\n  \n}"))
    (should (= (current-column) 2)))

  ;; JSON
  (with-temp-buffer
    (json-ts-mode)
    (insert "{}")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "{\n  \n}"))
    (should (= (current-column) 2)))

  ;; Go (tabs)
  (with-temp-buffer
    (go-ts-mode)
    (insert "func main() {}")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "func main() {\n\t\n}"))
    (should (= (current-column) 4)))

  ;; Rust
  (with-temp-buffer
    (rust-ts-mode)
    (insert "fn main() {}")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "fn main() {\n    \n}"))
    (should (= (current-column) 4))))

(ert-deftest ide-align-test-smart-return-brackets-parens ()
  "Test delimiter splitting for brackets and parens."
  (with-temp-buffer
    (json-ts-mode)
    (insert "[]")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "[\n  \n]"))
    (should (= (current-column) 2)))

  (with-temp-buffer
    (python-mode)
    (insert "call()")
    (backward-char)
    (ide-align-smart-return)
    (should (equal (buffer-string) "call(\n    \n)"))
    (should (= (current-column) 4))))

(ert-deftest ide-align-test-yaml-return-after-colon ()
  "Test hitting Enter after a YAML mapping key indents +2."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "    - name: Task 1\n      ansible.builtin.debug:")
    (ide-align-smart-return)
    (should (= (current-column) 8))
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "        "))))

(ert-deftest ide-align-test-yaml-electric-dash-alignment ()
  "Test typing `-' after a task aligns with the task's `- name:`."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "- name: Play 1\n  hosts: all\n  tasks:\n    - name: Task 1\n      ansible.builtin.debug:\n        msg: hello\n        ")
    ;; Point is at column 8 on empty line
    (ide-align-yaml-electric-dash)
    ;; It should align with "- name: Task 1" at column 4
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "    - "))
    (should (= (current-column) 6))
    ;; Type name: Task 2
    (insert "name: Task 2")
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "    - name: Task 2"))))

(ert-deftest ide-align-test-yaml-electric-dash-cycle ()
  "Test typing `-' repeatedly cycles from task to play to document delimiter."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "- name: Play 1\n  hosts: all\n  tasks:\n    - name: Task 1\n      ansible.builtin.debug:\n        msg: hello\n        ")
    ;; First '-': task level (col 4)
    (setq last-command nil)
    (ide-align-yaml-electric-dash)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "    - "))
    ;; Second '-': play level (col 0)
    (setq last-command 'ide-align-yaml-electric-dash)
    (ide-align-yaml-electric-dash)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "- "))
    ;; Third '-': document delimiter
    (setq last-command 'ide-align-yaml-electric-dash)
    (ide-align-yaml-electric-dash)
    (should (equal (buffer-substring (line-beginning-position) (line-end-position)) "---"))))

(ert-deftest ide-align-test-yaml-electric-backspace ()
  "Test backspace in YAML dedents by 2 spaces."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "      ")
    (ide-align-yaml-electric-backspace 1)
    (should (= (current-column) 4))
    (ide-align-yaml-electric-backspace 1)
    (should (= (current-column) 2))
    (ide-align-yaml-electric-backspace 1)
    (should (= (current-column) 0))))

(ert-deftest ide-align-test-yaml-tab-cycling ()
  "Test Tab in YAML cycles through sensible indentation levels."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "  tasks:\n")
    ;; First Tab: maximum indentation (col 4)
    (setq last-command nil)
    (ide-align-yaml-indent-line)
    (should (= (current-indentation) 4))
    ;; Repeated Tab: cycle back by 2 (col 2)
    (setq last-command 'ide-align-yaml-indent-line)
    (ide-align-yaml-indent-line)
    (should (= (current-indentation) 2))
    ;; Repeated Tab: cycle back to 0
    (setq last-command 'ide-align-yaml-indent-line)
    (ide-align-yaml-indent-line)
    (should (= (current-indentation) 0))
    ;; Repeated Tab: wrap to max (col 4)
    (setq last-command 'ide-align-yaml-indent-line)
    (ide-align-yaml-indent-line)
    (should (= (current-indentation) 4))))

(ert-deftest ide-align-test-ansible-module-completion ()
  "Test completing an Ansible module automatically appends `:` and indents."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (setq-local buffer-file-name "/tmp/playbook.yml")
    (insert "    - name: Test\n      ansible.builtin.debug")
    (ide-align--handle-ansible-module-completion)
    (should (equal (buffer-string) "    - name: Test\n      ansible.builtin.debug:\n        "))
    (should (= (current-column) 8))))

(ert-deftest ide-align-test-terraform-snippet-point ()
  "Test expanding Terraform resource snippet places point inside block at column 2."
  (with-temp-buffer
    (terraform-mode)
    (yas-minor-mode 1)
    (insert "res")
    (yas-expand)
    (insert "aws_s3_bucket")
    (yas-next-field)
    (insert "b")
    (yas-next-field)
    (should (equal (string-trim (buffer-string)) "resource \"aws_s3_bucket\" \"b\" {\n  \n}"))
    (should (= (current-column) 2))
    (should (= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-terraform-snippet-no-dollar-zero ()
  "Test that a snippet missing $0 still exits inside the block body."
  (with-temp-buffer
    (terraform-mode)
    (ide-align-mode 1)
    (yas-minor-mode 1)
    (yas-expand-snippet "provider \"${1:name}\" {\n  region = \"${2:region}\"\n}")
    (yas-next-field)
    (yas-next-field)
    ;; Point should be inside the block body, not outside '}'
    (should (not (looking-at-p "\\'")))
    (should (<= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-terraform-block-completion ()
  "Test completing a Terraform block with {} expands to 3 lines with point at column 2."
  (with-temp-buffer
    (terraform-mode)
    (ide-align-mode 1)
    (insert "resource \"aws_s3_bucket\" \"b\" {}")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "resource \"aws_s3_bucket\" \"b\" {\n  \n}"))
    (should (= (current-column) 2))
    (should (= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-terraform-provider-block-completion ()
  "Test completing a Terraform provider block placed on new line."
  (with-temp-buffer
    (terraform-mode)
    (ide-align-mode 1)
    (insert "provider \"aws\" {\n}")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "provider \"aws\" {\n  \n}"))
    (should (= (current-column) 2))
    (should (= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-terraform-tags-completion ()
  "Test completing nested tags = {} expands inside the parent block."
  (with-temp-buffer
    (terraform-mode)
    (ide-align-mode 1)
    (insert "resource \"aws_s3_bucket\" \"b\" {\n  tags = {}\n}")
    (forward-line -1)
    (end-of-line)
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "resource \"aws_s3_bucket\" \"b\" {\n  tags = {\n    \n  }\n}"))
    (should (= (current-column) 4))
    (should (= (line-number-at-pos) 3))))

(ert-deftest ide-align-test-yaml-schema-block-key-completion ()
  "Test completing a YAML schema block key like `tasks:` moves to child indent."
  (with-temp-buffer
    (yaml-ts-mode)
    (ide-align-yaml-mode 1)
    (insert "- name: Play\n  tasks:")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "- name: Play\n  tasks:\n    "))
    (should (= (current-column) 4))))

(ert-deftest ide-align-test-go-block-completion ()
  "Test completing func main() {} in Go expands with tab and point at column 4."
  (with-temp-buffer
    (go-ts-mode)
    (ide-align-mode 1)
    (insert "func main() {}")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "func main() {\n\t\n}"))
    (should (= (current-column) 4))
    (should (= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-rust-block-completion ()
  "Test completing fn main() {} in Rust expands with 4 spaces."
  (with-temp-buffer
    (rust-ts-mode)
    (ide-align-mode 1)
    (insert "fn main() {}")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "fn main() {\n    \n}"))
    (should (= (current-column) 4))
    (should (= (line-number-at-pos) 2))))

(ert-deftest ide-align-test-python-colon-completion ()
  "Test completing def foo(): in Python moves to 4 spaces indent."
  (with-temp-buffer
    (python-mode)
    (ide-align-mode 1)
    (insert "def compute():")
    (ide-align--post-completion-alignment)
    (should (equal (buffer-string) "def compute():\n    "))
    (should (= (current-column) 4))))

(ert-deftest ide-align-test-eglot-capf-advice ()
  "Test that ide-align--eglot-capf-advice wraps :exit-function."
  (with-temp-buffer
    (terraform-mode)
    (let* ((orig-called nil)
           (mock-capf (lambda ()
                        (list 1 1 '("resource \"aws_s3_bucket\" \"b\" {}")
                              :exit-function (lambda (_str _status) (setq orig-called t)))))
           (wrapped-capf (apply-partially #'ide-align--eglot-capf-advice mock-capf))
           (res (funcall wrapped-capf))
           (exit-fn (plist-get (nthcdr 3 res) :exit-function)))
      (insert "resource \"aws_s3_bucket\" \"b\" {}")
      (funcall exit-fn "resource \"aws_s3_bucket\" \"b\" {}" 'finished)
      (should orig-called)
      (should (equal (buffer-string) "resource \"aws_s3_bucket\" \"b\" {\n  \n}"))
      (should (= (current-column) 2)))))

(provide 'align-test)
;;; align-test.el ends here
