;;; ansible-integration-test.el --- ansible-intelligence alongside Eglot -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/ansible-integration-test.el
;; The package's own suite is tests/ansible-intelligence-tests.el.  These tests
;; cover the Foundry wiring: when it turns on, and that it cooperates with Eglot.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'xref)
(require 'eglot)
(require 'yaml-ts-mode)
(require 'lang-ansible)

(defconst ansible-int--playbook
  "- hosts: all\n  vars:\n    app_port: 8080\n  tasks:\n    - ansible.builtin.debug:\n        msg: \"{{ app_port }}\"\n")

(defmacro ansible-int--with-playbook (&rest body)
  "Run BODY in a yaml-ts-mode buffer visiting a temporary playbook."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (make-temp-file "ansible-int" t)))
          (file (expand-file-name "site.yml" dir)))
     (unwind-protect
         (progn
           (with-temp-file file (insert ansible-int--playbook))
           (let ((buf (find-file-noselect file)))
             (unwind-protect
                 (with-current-buffer buf
                   (yaml-ts-mode)
                   ,@body)
               (with-current-buffer buf (set-buffer-modified-p nil))
               (kill-buffer buf))))
       (delete-directory dir t))))

(defun ansible-int--goto (text)
  "Move point inside the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (backward-char 1))

(ert-deftest ansible-int-enabled-for-playbooks-only ()
  (ansible-int--with-playbook (should ansible-intelligence-mode))
  (with-temp-buffer
    (insert "key: value\nother: 1\n")
    (yaml-ts-mode)
    (should-not ansible-intelligence-mode)))

(ert-deftest ansible-int-detects-plays-without-a-telling-filename ()
  (with-temp-buffer
    (insert "- hosts: web\n  tasks: []\n")
    (should (ide--ansible-file-p))))

(ert-deftest ansible-int-xref-backend-order-leaves-eglot-in-charge ()
  "Our backend is tried first but only answers on Jinja identifiers."
  (ansible-int--with-playbook
    (add-hook 'xref-backend-functions #'eglot-xref-backend nil t)
    (should (eq (car xref-backend-functions) 'ansible-intelligence-native-xref-backend))
    (should (memq 'eglot-xref-backend xref-backend-functions))
    (ansible-int--goto "{{ app_port")
    (should (eq (xref-find-backend) 'ansible-intelligence-native))
    ;; A module name is not a variable: the server (here: Eglot's backend) gets it.
    (ansible-int--goto "ansible.builtin.debug")
    (should-not (eq (xref-find-backend) 'ansible-intelligence-native))))

(ert-deftest ansible-int-finds-variable-definition ()
  (ansible-int--with-playbook
    (ansible-int--goto "{{ app_port")
    (let* ((backend (xref-find-backend))
           (id (xref-backend-identifier-at-point backend))
           (defs (xref-backend-definitions backend id)))
      (should (equal id "app_port"))
      (should (= (length defs) 1))
      (should (string-match-p "app_port" (xref-item-summary (car defs)))))))

(ert-deftest ansible-int-completion-comes-after-eglot ()
  (ansible-int--with-playbook
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point nil t)
    (let ((fns completion-at-point-functions))
      (should (< (cl-position 'eglot-completion-at-point fns)
                 (cl-position 'ansible-intelligence-completion-at-point fns))))))

(ert-deftest ansible-int-disabling-removes-its-hooks ()
  (ansible-int--with-playbook
    (ansible-intelligence-mode -1)
    (should-not (memq 'ansible-intelligence-native-xref-backend xref-backend-functions))
    (should-not (memq 'ansible-intelligence-completion-at-point completion-at-point-functions))))

(ert-run-tests-batch-and-exit)

;;; ansible-integration-test.el ends here
