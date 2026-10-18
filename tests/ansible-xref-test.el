;;; ansible-xref-test.el --- Tests for lisp/ansible-xref.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Each test runs in a throwaway Ansible project (no git, so the whole tree is searched).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ansible-xref)

(defconst ansible-xref-test--files
  '(("site.yml" . "- hosts: all
  vars:
    app_port: 8080
  roles:
    - web
  tasks:
    - name: Check app_port
      ansible.builtin.include_tasks: tasks/extra.yml
    - name: Read it
      ansible.builtin.command: cat /etc/hostname
      register: hostname_out
    - name: Use it
      ansible.builtin.debug:
        msg: \"{{ hostname_out.stdout }} on {{ app_port }}\"
      when: app_port is defined
    - ansible.builtin.set_fact:
        computed: \"{{ app_port + 1 }}\"
")
    ("tasks/extra.yml" . "- name: Extra
  ansible.builtin.debug:
    msg: \"{{ entry }} {{ web_user }}\"
  loop: [a]
  loop_control:
    loop_var: entry
  when:
    - web_user is defined
")
    ("roles/web/defaults/main.yml" . "---
web_user: www
")
    ("roles/web/tasks/main.yml" . "- name: Install
  ansible.builtin.debug:
    msg: \"{{ web_user }}\"
"))
  "Alist of file name and text making up the test project.")

(defmacro ansible-xref-test--with-project (&rest body)
  "Run BODY with `default-directory' at the root of a temporary Ansible project."
  (declare (indent 0))
  `(let* ((default-directory (file-name-as-directory (make-temp-file "ansible-xref" t))))
     (unwind-protect
         (progn
           (pcase-dolist (`(,name . ,text) ansible-xref-test--files)
             (let ((file (expand-file-name name)))
               (make-directory (file-name-directory file) t)
               (with-temp-file file (insert text))))
           ,@body)
       (delete-directory default-directory t))))

(defun ansible-xref-test--lines (xrefs)
  "Return the sorted \"file:line\" strings of XREFS, relative to the project."
  (sort (mapcar (lambda (xref)
                  (let ((location (xref-item-location xref)))
                    (format "%s:%d" (file-relative-name (xref-location-group location))
                            (xref-location-line location))))
                xrefs)
        #'string<))

(defun ansible-xref-test--identifier-in (file text)
  "Visit FILE, go into the first TEXT, and return the identifier there."
  (with-current-buffer (find-file-noselect (expand-file-name file))
    (goto-char (point-min))
    (search-forward text)
    (backward-char 1)
    (prog1 (ansible-xref--identifier)
      (kill-buffer))))

(ert-deftest ansible-xref-finds-keys-of-vars-and-set-fact-blocks ()
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--lines (ansible-xref--definitions "app_port"))
                   '("site.yml:3")))
    (should (equal (ansible-xref-test--lines (ansible-xref--definitions "computed"))
                   '("site.yml:17")))))

(ert-deftest ansible-xref-finds-register-and-loop-var ()
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--lines (ansible-xref--definitions "hostname_out"))
                   '("site.yml:11")))
    (should (equal (ansible-xref-test--lines (ansible-xref--definitions "entry"))
                   '("tasks/extra.yml:6")))))

(ert-deftest ansible-xref-finds-top-level-keys-of-role-defaults ()
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--lines (ansible-xref--definitions "web_user"))
                   '("roles/web/defaults/main.yml:2")))))

(ert-deftest ansible-xref-ignores-keys-that-are-not-variables ()
  (ansible-xref-test--with-project
    (dolist (name '("name" "hosts" "msg" "loop_var" "stdout"))
      (should-not (ansible-xref--definitions name)))))

(ert-deftest ansible-xref-references-are-uses-in-expressions ()
  "Braces and conditions count; a task name or the definition itself does not."
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--lines (ansible-xref--references "app_port"))
                   '("site.yml:14" "site.yml:15" "site.yml:17")))
    (should (equal (ansible-xref-test--lines (ansible-xref--references "web_user"))
                   '("roles/web/tasks/main.yml:3" "tasks/extra.yml:3" "tasks/extra.yml:8")))))

(ert-deftest ansible-xref-references-skip-fields-of-other-names ()
  "`hostname_out.stdout' uses hostname_out, not stdout, and `x.entry' is not `entry'."
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--lines (ansible-xref--references "hostname_out"))
                   '("site.yml:14")))
    (should-not (ansible-xref--references "stdout"))))

(ert-deftest ansible-xref-identifier-is-the-base-of-a-dotted-name ()
  (ansible-xref-test--with-project
    (should (equal (ansible-xref-test--identifier-in "site.yml" "out.stdout") "hostname_out"))
    (should (equal (ansible-xref-test--identifier-in "site.yml" "stdout") "hostname_out"))))

(ert-deftest ansible-xref-resolves-include-and-role-links ()
  (ansible-xref-test--with-project
    (let ((include (ansible-xref-test--identifier-in "site.yml" "include_tasks"))
          (role (ansible-xref-test--identifier-in "site.yml" "- web")))
      (should (equal (get-text-property 0 'ansible-xref-file include)
                     (expand-file-name "tasks/extra.yml")))
      (should (equal (get-text-property 0 'ansible-xref-file role)
                     (expand-file-name "roles/web/tasks/main.yml"))))))

(ert-deftest ansible-xref-backend-answers-only-what-it-can-resolve ()
  (ansible-xref-test--with-project
    (with-current-buffer (find-file-noselect (expand-file-name "site.yml"))
      (unwind-protect
          (progn
            (goto-char (point-min))
            (search-forward "{{ app_port")
            (should (eq (ansible-xref-backend) 'ansible-xref))
            (goto-char (point-min))
            (search-forward "ansible.builtin.command")
            (should-not (ansible-xref-backend))
            (goto-char (point-min))
            (search-forward "hosts")
            (should-not (ansible-xref-backend)))
        (kill-buffer)))))

(ert-deftest ansible-xref-definitions-see-unsaved-edits ()
  (ansible-xref-test--with-project
    (with-current-buffer (find-file-noselect (expand-file-name "site.yml"))
      (unwind-protect
          (progn
            (goto-char (point-max))
            (insert "      register: fresh_result\n")
            (should (equal (ansible-xref-test--lines (ansible-xref--definitions "fresh_result"))
                           '("site.yml:18"))))
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(provide 'ansible-xref-test)
;;; ansible-xref-test.el ends here
