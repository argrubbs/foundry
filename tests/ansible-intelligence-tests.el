;;; ansible-intelligence-tests.el --- Tests for Ansible Intelligence -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'ansible-intelligence)

(defmacro ansible-intelligence-tests-with-buffer (contents &rest body)
  "Evaluate BODY in a YAML buffer containing CONTENTS."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (insert ,contents)
     (setq-local major-mode 'yaml-ts-mode)
     (ansible-intelligence-mode 1)
     ,@body))

(ert-deftest ansible-intelligence-register-is-source-ordered ()
  (ansible-intelligence-tests-with-buffer
      "- hosts: all\n  tasks:\n    - debug:\n        var: \"{{ outcome }}\"\n    - command: true\n      register: outcome\n    - debug:\n        var: \"{{ outcome }}\"\n"
    (goto-char (point-min))
    (search-forward "{{ outcome }}")
    (should-not (ansible-intelligence--symbol-at-point))
    (goto-char (point-max))
    (search-backward "outcome")
    (forward-char (length "outcome"))
    (should (eq (ansible-intelligence-symbol-kind (ansible-intelligence--symbol-at-point))
                'register))))

(ert-deftest ansible-intelligence-loop-result-has-results-field ()
  (ansible-intelligence-tests-with-buffer
      "- command: echo {{ item }}\n  loop: [one]\n  register: output\n- debug:\n    var: \"{{ output. }}\"\n"
    (goto-char (point-max))
    (search-backward "output.")
    (goto-char (+ (point) (length "output.")))
    (let ((completion (ansible-intelligence-completion-at-point)))
      (should (= (car completion) (point)))
      (should (member "results" (nth 2 completion))))))

(ert-deftest ansible-intelligence-finds-register-from-result-field ()
  (ansible-intelligence-tests-with-buffer
      "- command: echo hello\n  register: command_result\n- debug:\n    msg: \"{{ command_result.stdout }}\"\n"
    (goto-char (point-max))
    (search-backward "stdout")
    (forward-char (length "stdout"))
    (should (eq (ansible-intelligence-symbol-kind (ansible-intelligence--symbol-at-point))
                'register))))

(ert-deftest ansible-intelligence-definition-refreshes-stale-snapshot ()
  (ansible-intelligence-tests-with-buffer
      "- command: echo hello\n  register: command_result\n- debug:\n    msg: \"{{ command_result }}\"\n"
    (setq-local ansible-intelligence--document
                (ansible-intelligence-document-create :symbols nil :references nil))
    (goto-char (point-max))
    (search-backward "command_result")
    (forward-char (length "command_result"))
    (ansible-intelligence-find-definition)
    (should (= (line-number-at-pos) 2))
    (should (looking-at "command_result"))))

(ert-deftest ansible-intelligence-finds-base-from-middle-or-field ()
  (ansible-intelligence-tests-with-buffer
      "- command: echo hello\n  register: command_result\n- debug:\n    msg: \"{{ command_result.stdout }}\"\n"
    (goto-char (point-min))
    (search-forward "command_res")
    (should (string= (ansible-intelligence-symbol-name
                      (ansible-intelligence--symbol-at-point))
                     "command_result"))
    (search-forward "stdout")
    (backward-char 3)
    (should (string= (ansible-intelligence-symbol-name
                      (ansible-intelligence--symbol-at-point))
                     "command_result"))))

(defun ansible-intelligence-tests--write-file (root relative contents)
  "Write CONTENTS below ROOT at RELATIVE and return its path."
  (let ((file (expand-file-name relative root)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert contents))
    file))

(ert-deftest ansible-intelligence-parses-static-project-links ()
  (let ((buffer-file-name (expand-file-name "example/playbook.yml"
                                            default-directory)))
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "example/playbook.yml"
                                               default-directory))
      (insert "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - import_tasks: tasks/setup.yml\n    - include_role:\n        name: web\n")
      (let ((links (ansible-intelligence-document-links
                    (ansible-intelligence-parse-buffer))))
        (should (equal (mapcar #'ansible-intelligence-link-kind links)
                       '(vars-files import-tasks include-role)))
        (should (equal (mapcar #'ansible-intelligence-link-target links)
                       '("vars/common.yml" "tasks/setup.yml" "web")))))))

(ert-deftest ansible-intelligence-project-index-connects-vars-and-imports ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - import_tasks: tasks/setup.yml\n    - debug:\n        msg: \"{{ shared_value }} {{ imported_result }}\"\n"))
         (vars
          (ansible-intelligence-tests--write-file
           root "vars/common.yml" "shared_value: present\n"))
         (tasks
          (ansible-intelligence-tests--write-file
           root "tasks/setup.yml"
           "- command: echo ready\n  register: imported_result\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :generation 0 :status 'ready)))
    (unwind-protect
        (progn
          (dolist (file (list playbook vars tasks))
            (ansible-intelligence-index-put-document
             index (ansible-intelligence--parse-file file)))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (setq ansible-intelligence--document
                  (ansible-intelligence-parse-buffer))
            (goto-char (point-min))
            (search-forward "import_tasks")
            (beginning-of-line)
            (should-not
             (member "imported_result"
                     (mapcar #'ansible-intelligence-symbol-name
                             (ansible-intelligence--visible-symbols (point)))))
            (goto-char (point-max))
            (let ((names (mapcar #'ansible-intelligence-symbol-name
                                 (ansible-intelligence--visible-symbols (point)))))
              (should (member "shared_value" names))
              (should (member "imported_result" names)))
            (should (= (length (ansible-intelligence-index-definitions
                                index "shared_value"))
                       1))
            (should (= (length (ansible-intelligence-index-references
                                index "imported_result"))
                       1))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-project-index-cross-file-capf-includes-imported-fields ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  tasks:\n    - debug:\n        msg: \"{{ imported_result }}\"\n    - name: Load setup tasks\n      ansible.builtin.import_tasks: tasks/setup.yml\n    - debug:\n        msg: \"{{ imported_result. }}\"\n"))
         (tasks
          (ansible-intelligence-tests--write-file
           root "tasks/setup.yml"
           "- command: echo ready\n  register: imported_result\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready)))
    (unwind-protect
        (progn
          (dolist (file (list playbook tasks))
            (ansible-intelligence-index-put-document
             index (ansible-intelligence--parse-file file)))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence-mode t
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (setq ansible-intelligence--document
                  (ansible-intelligence-parse-buffer))
            (goto-char (point-min))
            (search-forward "imported_result }}")
            (should-not
             (member "imported_result"
                     (mapcar #'ansible-intelligence-symbol-name
                             (ansible-intelligence--visible-symbols (point)))))
            (goto-char (point-max))
            (search-backward "imported_result.")
            (goto-char (+ (point) (length "imported_result.")))
            (let ((completion (ansible-intelligence-completion-at-point)))
              (should completion)
              (should (member "stdout" (nth 2 completion))))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-project-index-cross-file-capf-includes-nested-vars ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - debug:\n        msg: \"{{ common_nested. }}\"\n"))
         (vars
          (ansible-intelligence-tests--write-file
           root "vars/common.yml"
           "common_nested:\n  enabled: true\n  deeper:\n    value: present\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready)))
    (unwind-protect
        (progn
          (dolist (file (list playbook vars))
            (ansible-intelligence-index-put-document
             index (ansible-intelligence--parse-file file)))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence-mode t
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (setq ansible-intelligence--document
                  (ansible-intelligence-parse-buffer))
            (goto-char (point-max))
            (search-backward "common_nested.")
            (goto-char (+ (point) (length "common_nested.")))
            (let ((completion (ansible-intelligence-completion-at-point)))
              (should completion)
              (should (member "enabled" (nth 2 completion)))
              (should (member "deeper" (nth 2 completion)))
              (should (string-match-p
                       "inferred"
                       (get-text-property 0 'annotation
                                          (seq-find (lambda (candidate)
                                                      (string= candidate "enabled"))
                                                    (nth 2 completion))))))
            (delete-region (line-beginning-position) (line-end-position))
            (insert "        msg: \"{{ common_nested.deeper. }}\"")
            (search-backward "common_nested.deeper.")
            (goto-char (+ (point) (length "common_nested.deeper.")))
            (let ((completion (ansible-intelligence-completion-at-point)))
              (should completion)
              (should (member "value" (nth 2 completion))))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-unsaved-vars-edit-updates-cross-file-capf ()
  (let* ((temporary-file-directory
          (expand-file-name "ansible-intelligence-test-tmp/" default-directory))
  (root
   (progn
     (make-directory temporary-file-directory t)
     (make-temp-file "project-" t)))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - debug:\n        msg: \"{{ common_nested. }}\"\n"))
         (vars
          (ansible-intelligence-tests--write-file
           root "vars/common.yml"
           "common_nested:\n  enabled: true\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready))
         (vars-buffer (generate-new-buffer " *ansible-intelligence-vars*"))
         (playbook-buffer (generate-new-buffer " *ansible-intelligence-playbook*")))
    (unwind-protect
        (progn
          (with-current-buffer vars-buffer
            (setq buffer-file-name vars
                  major-mode 'yaml-ts-mode
                  ansible-intelligence-mode t
                  ansible-intelligence--project-index index)
            (insert-file-contents vars)
            (ansible-intelligence--analyze))
          (with-current-buffer playbook-buffer
            (setq buffer-file-name playbook
                  major-mode 'yaml-ts-mode
                  ansible-intelligence-mode t
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (ansible-intelligence--analyze))
          (with-current-buffer vars-buffer
            (goto-char (point-max))
            (insert "  cat_based: true\n  cat_type: tabby\n")
            (should (buffer-modified-p))
            (ansible-intelligence--analyze)
            (let* ((document
                    (ansible-intelligence-index-current-document index vars))
                   (nested
                    (seq-find
                     (lambda (symbol)
                       (string= (ansible-intelligence-symbol-name symbol)
                                "common_nested"))
                     (ansible-intelligence-document-symbols document))))
              (should (member "cat_based"
                              (mapcar #'car
                                      (ansible-intelligence-symbol-fields nested))))
              (should (member "cat_type"
                              (mapcar #'car
                                      (ansible-intelligence-symbol-fields nested))))))
          (with-current-buffer playbook-buffer
            (goto-char (point-max))
            (search-backward "common_nested.")
            (goto-char (+ (point) (length "common_nested.")))
            (let ((completion (ansible-intelligence-completion-at-point)))
              (should completion)
              (should (member "cat_based" (nth 2 completion)))
              (should (member "cat_type" (nth 2 completion))))))
      (when (buffer-live-p vars-buffer)
        (kill-buffer vars-buffer))
      (when (buffer-live-p playbook-buffer)
        (kill-buffer playbook-buffer))
      (delete-directory root t)
      (when (file-directory-p temporary-file-directory)
        (delete-directory temporary-file-directory t)))))

(ert-deftest ansible-intelligence-enables-vars-file-buffers ()
  (with-temp-buffer
    (setq buffer-file-name "/project/vars/common.yml"
          major-mode 'yaml-ts-mode)
    (let (enabled)
      (cl-letf (((symbol-function 'ansible-intelligence-mode)
                 (lambda (&optional arg) (setq enabled arg))))
        (ansible-intelligence-enable-maybe))
      (should enabled))))

(ert-deftest ansible-intelligence-definition-resolves-after-static-import ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  tasks:\n    - ansible.builtin.import_tasks: tasks/setup.yml\n    - debug:\n        msg: \"{{ imported_result.stdout }}\"\n"))
         (tasks
          (ansible-intelligence-tests--write-file
           root "tasks/setup.yml"
           "- command: echo ready\n  register: imported_result\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready)))
    (unwind-protect
        (progn
          (dolist (file (list playbook tasks))
            (ansible-intelligence-index-put-document
             index (ansible-intelligence--parse-file file)))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (setq ansible-intelligence--document
                  (ansible-intelligence-parse-buffer))
            (goto-char (point-max))
            (search-backward "imported_result.stdout")
            (forward-char (length "imported_result.stdout"))
            (let (location)
              (cl-letf (((symbol-function 'xref--show-location)
                         (lambda (xref-location &optional _display)
                           (setq location xref-location))))
                (ansible-intelligence-find-definition))
              (should location)
              (should (equal (file-truename (xref-location-group location))
                             (file-truename tasks)))
              (should (= (xref-location-line location) 2)))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-index-parser-keeps-temp-buffer-inert ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (file (ansible-intelligence-tests--write-file
                root "tasks/main.yml" "- debug:\n    msg: hello\n"))
         (visited-file-killed nil)
         (hook (lambda ()
                 (when buffer-file-name
                   (setq visited-file-killed buffer-file-name)))))
    (unwind-protect
        (progn
          (add-hook 'kill-buffer-hook hook)
          (ansible-intelligence--parse-file file)
          (should-not visited-file-killed))
      (remove-hook 'kill-buffer-hook hook)
      (delete-directory root t))))

(ert-deftest ansible-intelligence-play-vars-are-safe-and-not-polluted ()
  (ansible-intelligence-tests-with-buffer
      "- hosts: all\n  vars:\n    play_value: yes\n  tasks:\n    - debug:\n        msg: \"{{ play_value }}\"\n"
    (goto-char (point-max))
    (let ((symbols (ansible-intelligence-document-symbols
                    ansible-intelligence--document)))
      (should (eq (ansible-intelligence-symbol-kind
                   (seq-find (lambda (symbol)
                               (string= (ansible-intelligence-symbol-name symbol)
                                        "play_value"))
                             symbols))
                  'play-var))
      (should-not
       (seq-find (lambda (symbol)
                   (string= (ansible-intelligence-symbol-name symbol) "msg"))
                 symbols))
      (should (ansible-intelligence-completion-at-point)))))

(ert-deftest ansible-intelligence-custom-loop-var-in-module-first-task ()
  (ansible-intelligence-tests-with-buffer
      "- command: echo {{ package }}\n  loop: [one]\n  loop_control:\n    loop_var: package\n"
    (let ((names
           (mapcar #'ansible-intelligence-symbol-name
                   (ansible-intelligence-document-symbols
                    ansible-intelligence--document))))
      (should (member "package" names))
      (should-not (member "item" names))
      (should-not (member "loop_var" names)))))

(ert-deftest ansible-intelligence-index-keeps-local-source-order ()
  (let* ((file (expand-file-name "ansible-intelligence-order.yml"
                                default-directory))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-directory file)
           :documents (make-hash-table :test #'equal)
           :status 'ready)))
    (with-temp-buffer
      (setq buffer-file-name file
            ansible-intelligence--project-index index)
      (insert "- debug:\n    var: \"{{ later }}\"\n- command: true\n  register: later\n")
      (setq ansible-intelligence--document (ansible-intelligence-parse-buffer))
      (ansible-intelligence-index-put-document index ansible-intelligence--document)
      (goto-char (point-min))
      (search-forward "later")
      (should-not
       (ansible-intelligence-index-visible-definitions
        index ansible-intelligence--document "later" (point))))))

(ert-deftest ansible-intelligence-capf-is-buffer-local ()
  (let ((global-capfs (default-value 'completion-at-point-functions)))
    (ansible-intelligence-tests-with-buffer "- debug:\n    msg: hello\n"
      (should (memq #'ansible-intelligence-completion-at-point
                    completion-at-point-functions))
      (should (equal global-capfs
                     (default-value 'completion-at-point-functions))))))

(ert-deftest ansible-intelligence-jinja-references-exclude-fields-and-filters ()
  (ansible-intelligence-tests-with-buffer
      "- debug:\n    msg: \"{{ result.stdout | default(other_value) }}\"\n"
    (should (equal
             (mapcar #'ansible-intelligence-reference-name
                     (ansible-intelligence-document-references
                      ansible-intelligence--document))
             '("result" "other_value")))))

(ert-deftest ansible-intelligence-set-fact-does-not-capture-task-keys ()
  (ansible-intelligence-tests-with-buffer
      "- set_fact:\n    configured: true\n  when: enabled\n  tags: [test]\n  loop: [one]\n"
    (let ((names
           (mapcar #'ansible-intelligence-symbol-name
                   (ansible-intelligence-document-symbols
                    ansible-intelligence--document))))
      (should (member "configured" names))
      (should (member "item" names))
      (should-not (member "when" names))
      (should-not (member "tags" names))
      (should-not (member "loop" names)))))

(ert-deftest ansible-intelligence-nested-vars-keep-following-siblings ()
  (ansible-intelligence-tests-with-buffer
      "- hosts: all\n  vars:\n    first: 1\n    nested:\n      child: value\n      list:\n        - key: value\n    second: 2\n  tasks: []\n"
    (let ((names
           (mapcar #'ansible-intelligence-symbol-name
                   (ansible-intelligence-document-symbols
                    ansible-intelligence--document))))
      (should (equal names '("first" "nested" "second"))))))

(ert-deftest ansible-intelligence-project-el-discovers-yaml-files ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (yaml (ansible-intelligence-tests--write-file
                root "vars/main.yml" "from_project: yes\n"))
         (ignored (ansible-intelligence-tests--write-file
                   root "notes.txt" "not yaml\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :project 'test-project :documents (make-hash-table :test #'equal)
           :generation 1 :status 'discovering)))
    (unwind-protect
        (cl-letf (((symbol-function 'project-files)
                   (lambda (_project) (list yaml ignored))))
          (ansible-intelligence--discover-project-files index 1)
          (should (eq (ansible-intelligence-project-index-status index) 'ready))
          (should (= (hash-table-count
                      (ansible-intelligence-project-index-documents index))
                     1))
          (should (ansible-intelligence-index-definitions index "from_project")))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-resolves-static-link-at-point ()
  (let* ((root (make-temp-file "ansible-intelligence-" t))
         (playbook (ansible-intelligence-tests--write-file
                    root "playbook.yml" "- import_tasks: tasks/setup.yml\n"))
         (_tasks (ansible-intelligence-tests--write-file
                  root "tasks/setup.yml" "- debug:\n    msg: ready\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal) :status 'ready)))
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name playbook
                ansible-intelligence--project-index index)
          (insert-file-contents playbook)
          (setq ansible-intelligence--document
                (ansible-intelligence-parse-buffer))
          (goto-char (point-max))
          (search-backward "setup")
          (should (= (length
                      (ansible-intelligence--link-xrefs
                       (ansible-intelligence--link-at-point)))
                     1)))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-index-preserves-symlinked-live-document ()
  (let* ((root (make-temp-file "ansible-intelligence-real-" t))
         (link (concat root "-link"))
         (real-file (ansible-intelligence-tests--write-file
                     root "tasks/main.yml"
                     "- command: echo disk\n  register: disk_result\n"))
         (linked-file (expand-file-name "tasks/main.yml" link))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :files (list (file-truename real-file))
           :pending (list (file-truename real-file))
           :generation 1 :status 'indexing)))
    (make-symbolic-link root link)
    (unwind-protect
        (with-temp-buffer
          (setq buffer-file-name linked-file
                buffer-file-truename (file-truename linked-file))
          (insert "- command: echo live\n  register: live_result\n")
          (setq-local ansible-intelligence--document
                      (ansible-intelligence-parse-buffer))
          (ansible-intelligence-index-put-document
           index ansible-intelligence--document)
          (ansible-intelligence--index-next-batch index 1)
          (should (ansible-intelligence-index-definitions index "live_result"))
          (should-not
           (ansible-intelligence-index-definitions index "disk_result")))
      (delete-file link)
      (delete-directory root t))))

(ert-deftest ansible-intelligence-jinja-extracts-all-region-kinds-and-offsets ()
  (ansible-intelligence-tests-with-buffer
      "msg: \"before {{ value }} {% set answer = other %} {# note #} after\"\n"
    (let* ((document ansible-intelligence--document)
           (snapshot (ansible-intelligence-document-jinja document))
           (regions (ansible-intelligence-jinja-snapshot-regions snapshot))
           (expected '("{{ value }}" "{% set answer = other %}" "{# note #}")))
      (should (equal (mapcar #'ansible-intelligence-jinja-region-kind regions)
                     '(expression statement comment)))
      (should (equal (mapcar #'ansible-intelligence-jinja-region-text regions)
                     expected))
      (dolist (region regions)
        (let ((range (ansible-intelligence-jinja-region-range region)))
          (should
           (equal (buffer-substring-no-properties
                   (ansible-intelligence-range-start range)
                   (ansible-intelligence-range-end range))
                  (ansible-intelligence-jinja-region-text region))))))))

(ert-deftest ansible-intelligence-jinja-references-have-precise-paths ()
  (ansible-intelligence-tests-with-buffer
      "msg: \"{{ result.stdout | default(other_value) }} {{ result['rc'] }} {{ value is defined }}\"\n"
    (let ((references (ansible-intelligence-document-references
                       ansible-intelligence--document)))
      (should (equal (mapcar #'ansible-intelligence-reference-name references)
                     '("result" "other_value" "result" "value")))
      (let ((first (nth 0 references))
            (bracket (nth 2 references)))
        (should (equal (ansible-intelligence-reference-path first)
                       '("stdout")))
        (should (equal (ansible-intelligence-reference-path bracket)
                       '("rc")))
        (should
         (equal
          (buffer-substring-no-properties
           (ansible-intelligence-range-start
            (car (ansible-intelligence-reference-path-ranges first)))
           (ansible-intelligence-range-end
            (car (ansible-intelligence-reference-path-ranges first))))
          "stdout")))
      (should-not
       (seq-find (lambda (reference)
                   (member (ansible-intelligence-reference-name reference)
                           '("default" "stdout" "rc" "defined")))
                 references)))))

(ert-deftest ansible-intelligence-jinja-call-arguments-are-references ()
  (ansible-intelligence-tests-with-buffer
      "msg: '{{ lookup(first, key=second) }} {{ result[\"stdout\"] }}'\n"
    (let ((names (mapcar #'ansible-intelligence-reference-name
                         (ansible-intelligence-document-references
                          ansible-intelligence--document))))
      (should (equal names '("first" "second" "result")))
      (should-not (member "lookup" names))
      (should-not (member "key" names))
      (should-not (member "stdout" names)))))

(ert-deftest ansible-intelligence-jinja-escaped-yaml-quote-is-a-literal-field ()
  (ansible-intelligence-tests-with-buffer
      "msg: \"{{ result[\\\"stdout\\\"] }}\"\n"
    (let* ((references (ansible-intelligence-document-references
                        ansible-intelligence--document))
           (reference (car references)))
      (should (equal (mapcar #'ansible-intelligence-reference-name references)
                     '("result")))
      (should (equal (ansible-intelligence-reference-path reference)
                     '("stdout"))))))

(ert-deftest ansible-intelligence-jinja-set-and-for-scope-is-lexical ()
  (ansible-intelligence-tests-with-buffer
      "msg: \"{% set answer = source %} {% for item in items %}{{ item.name }}{% endfor %} {{ item }}\"\n"
    (let* ((document ansible-intelligence--document)
           (snapshot (ansible-intelligence-document-jinja document))
           (definitions (ansible-intelligence-jinja-snapshot-definitions snapshot))
           (answer (seq-find (lambda (symbol)
                               (string= (ansible-intelligence-symbol-name symbol)
                                        "answer"))
                             definitions))
           (item (seq-find (lambda (symbol)
                             (string= (ansible-intelligence-symbol-name symbol)
                                      "item"))
                           definitions)))
      (should answer)
      (should item)
      (should (eq (ansible-intelligence-symbol-kind answer) 'jinja-set))
      (should (eq (ansible-intelligence-symbol-kind item) 'jinja-loop-var))
      (goto-char (point-min))
      (search-forward "{{ item.name }}")
      (should
       (seq-find (lambda (symbol)
                   (string= (ansible-intelligence-symbol-name symbol) "item"))
                 (ansible-intelligence--local-visible-symbols (point))))
      (search-forward "{{ item }}")
      (should-not
       (seq-find (lambda (symbol)
                   (string= (ansible-intelligence-symbol-name symbol) "item"))
                 (ansible-intelligence--local-visible-symbols (point)))))))

(ert-deftest ansible-intelligence-jinja-malformed-region-is-isolated ()
  (ansible-intelligence-tests-with-buffer
      "first: \"{{ broken\"\nsecond: \"{{ good.value }}\"\n"
    (let* ((snapshot (ansible-intelligence-document-jinja
                      ansible-intelligence--document))
           (regions (ansible-intelligence-jinja-snapshot-regions snapshot))
           (references (ansible-intelligence-document-references
                        ansible-intelligence--document)))
      (should (= (length regions) 2))
      (should-not (ansible-intelligence-jinja-region-complete (car regions)))
      (should (ansible-intelligence-jinja-region-complete (cadr regions)))
      (should (equal (mapcar #'ansible-intelligence-reference-name references)
                     '("good")))
      (should (ansible-intelligence-jinja-snapshot-incomplete snapshot)))))

(ert-deftest ansible-intelligence-jinja-cache-reuses-only-unchanged-regions ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode
          ansible-intelligence-mode t
          ansible-intelligence--jinja-cache (make-hash-table :test #'equal))
    (insert "msg: \"{{ first }} {{ second }}\"\n")
    (ansible-intelligence--analyze)
    (let* ((first-document ansible-intelligence--document)
           (first-snapshot (ansible-intelligence-document-jinja first-document))
           (first-regions (ansible-intelligence-jinja-snapshot-regions
                           first-snapshot)))
      (ansible-intelligence--analyze)
      (let* ((second-snapshot
              (ansible-intelligence-document-jinja ansible-intelligence--document))
             (second-regions
              (ansible-intelligence-jinja-snapshot-regions second-snapshot)))
        (should (= (ansible-intelligence-jinja-snapshot-cache-hits
                    second-snapshot)
                   2))
        (should (eq (ansible-intelligence-jinja-region-analysis
                     (car first-regions))
                    (ansible-intelligence-jinja-region-analysis
                     (car second-regions))))
        (goto-char (point-min))
        (search-forward "first")
        (replace-match "changed")
        (ansible-intelligence--analyze)
        (let* ((third-snapshot
                (ansible-intelligence-document-jinja ansible-intelligence--document))
               (third-regions
                (ansible-intelligence-jinja-snapshot-regions third-snapshot)))
          (should (= (ansible-intelligence-jinja-snapshot-cache-hits
                      third-snapshot)
                     1))
          (should (= (ansible-intelligence-jinja-snapshot-cache-misses
                      third-snapshot)
                     1))
          (should-not (eq (ansible-intelligence-jinja-region-analysis
                           (car second-regions))
                          (ansible-intelligence-jinja-region-analysis
                           (car third-regions))))
          (should (eq (ansible-intelligence-jinja-region-analysis
                       (cadr second-regions))
                      (ansible-intelligence-jinja-region-analysis
                       (cadr third-regions)))))))))

(ert-deftest ansible-intelligence-jinja-limits-mark-partial-without-blocking ()
  (let ((ansible-intelligence-jinja-max-regions 1))
    (ansible-intelligence-tests-with-buffer
        "msg: \"{{ first }} {{ second }}\"\n"
      (let ((snapshot (ansible-intelligence-document-jinja
                       ansible-intelligence--document)))
        (should (= (ansible-intelligence-jinja-snapshot-region-count snapshot)
                   1))
        (should (ansible-intelligence-jinja-snapshot-incomplete snapshot))
        (should (ansible-intelligence-jinja-snapshot-errors snapshot)))))
  (let ((ansible-intelligence-jinja-max-scalar-bytes 4))
    (ansible-intelligence-tests-with-buffer
        "msg: \"{{ first }}\"\n"
      (should
       (ansible-intelligence-jinja-snapshot-incomplete
        (ansible-intelligence-document-jinja ansible-intelligence--document))))))

(ert-deftest ansible-intelligence-jinja-dotted-and-bracket-completion-is-snapshot-only ()
  (with-temp-buffer
    (setq buffer-file-name "/project/vars/main.yml"
          major-mode 'yaml-ts-mode
          ansible-intelligence-mode t
          ansible-intelligence--jinja-cache (make-hash-table :test #'equal))
    (insert "nested:\n  child:\n    value: yes\nmsg: \"{{ nested.child. }}\"\n")
    (ansible-intelligence--analyze)
    (goto-char (point-max))
    (search-backward "}}")
    (let ((completion (ansible-intelligence-completion-at-point)))
      (should completion)
      (should (member "value" (mapcar #'substring-no-properties
                                      (nth 2 completion)))))
    (delete-region (line-beginning-position) (line-end-position))
    (insert "msg: \"{{ nested['child']. }}\"")
    (ansible-intelligence--analyze)
    (goto-char (point-max))
    (search-backward "}}")
    (let ((completion (ansible-intelligence-completion-at-point)))
      (should completion)
      (should (member "value" (mapcar #'substring-no-properties
                                      (nth 2 completion)))))))

(ert-deftest ansible-intelligence-completion-never-parses-jinja ()
  (ansible-intelligence-tests-with-buffer
      "msg: \"{{ value. }}\"\n"
    (let (called)
      (cl-letf (((symbol-function 'ansible-intelligence-jinja--parse-analysis)
                 (lambda (&rest _)
                   (setq called t)
                   (error "CAPF parsed Jinja"))))
        (goto-char (point-max))
        (search-backward "}}")
        (ansible-intelligence-completion-at-point))
      (should-not called))))

(ert-deftest ansible-intelligence-jinja-fallback-is-graceful ()
  (cl-letf (((symbol-function 'ansible-intelligence-jinja--language)
             (lambda () nil)))
    (ansible-intelligence-tests-with-buffer
        "msg: \"{{ fallback_value }}\"\n"
      (let* ((document ansible-intelligence--document)
             (snapshot (ansible-intelligence-document-jinja document)))
        (should (eq (ansible-intelligence-jinja-snapshot-parser snapshot)
                    'fallback))
        (should (equal
                 (mapcar #'ansible-intelligence-reference-name
                         (ansible-intelligence-document-references document))
                 '("fallback_value")))
        (should (ansible-intelligence-jinja-snapshot-errors snapshot))))))

(ert-deftest ansible-intelligence-native-xref-selects-local-and-declines-module ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: command_result\n- debug:\n    msg: \"{{ command_result.stdout }}\"\n"
    (goto-char (point-max))
    (search-backward "command_result.stdout")
    (forward-char (length "command_result.stdout"))
    (let ((backend (ansible-intelligence-native-xref-backend)))
      (should (eq backend 'ansible-intelligence-native))
      (let ((definitions (xref-backend-definitions backend "command_result")))
        (should (= (length definitions) 1))
        (let ((location (xref-item-location (car definitions))))
          (with-current-buffer (marker-buffer
                               (xref-location-marker location))
            (goto-char (marker-position (xref-location-marker location)))
            (should (= (line-number-at-pos) 2)))))
      (should (xref-backend-references backend "command_result")))
    (goto-char (point-min))
    (search-forward "debug")
    (should-not (ansible-intelligence-native-xref-backend))
    (should-not (ansible-intelligence-native-capf-owned-p))))

(ert-deftest ansible-intelligence-native-xref-precedes-unconditional-eglot ()
  (ansible-intelligence-tests-with-buffer
     "- command: true\n  register: command_result\n- debug:\n    msg: \"{{ command_result }}\"\n"
    (let ((eglot-backend (lambda () 'eglot)))
     (unwind-protect
         (progn
           (add-hook 'xref-backend-functions eglot-backend nil t)
           (goto-char (point-max))
           (search-backward "command_result")
           (forward-char (length "command_result"))
           (should (eq (xref-find-backend)
                       'ansible-intelligence-native))
           (goto-char (point-min))
           (search-forward "debug")
           (should (eq (xref-find-backend) 'eglot)))
       (remove-hook 'xref-backend-functions eglot-backend t)))))

(ert-deftest ansible-intelligence-native-xref-preserves-static-link-ownership ()
  (let ((root (make-temp-file "ansible-intelligence-native-" t)))
    (unwind-protect
        (let ((playbook (ansible-intelligence-tests--write-file
                         root "playbook.yml"
                         "- import_tasks: tasks/setup.yml\n"))
              (tasks (ansible-intelligence-tests--write-file
                      root "tasks/setup.yml" "- debug:\n    msg: ready\n")))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  major-mode 'yaml-ts-mode
                  ansible-intelligence-mode t)
            (insert-file-contents playbook)
            (setq ansible-intelligence--project-index
                  (ansible-intelligence-project-index-create
                   :root (file-name-as-directory root)
                   :documents (make-hash-table :test #'equal)
                   :status 'ready))
            (ansible-intelligence--analyze)
            (goto-char (point-max))
            (search-backward "tasks/setup")
            (should (eq (ansible-intelligence-native-xref-backend)
                        'ansible-intelligence-native))
            (let ((definitions
                   (xref-backend-definitions
                    'ansible-intelligence-native "tasks/setup.yml")))
              (should (= (length definitions) 1))
              (should (equal (file-truename tasks)
                             (file-truename
                              (xref-location-group
                               (xref-item-location (car definitions)))))))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-native-xref-declines-stale-live-offsets ()
  (let* ((root (make-temp-file "ansible-intelligence-native-live-" t))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- import_tasks: tasks/main.yml\n  tasks:\n    - debug:\n        msg: \"{{ result }}\"\n"))
         (tasks
          (ansible-intelligence-tests--write-file
           root "tasks/main.yml"
           "- command: true\n  register: result\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready))
         (tasks-document
          (with-temp-buffer
            (setq buffer-file-name tasks)
            (insert-file-contents tasks)
            (ansible-intelligence-parse-buffer)))
         (playbook-document
          (with-temp-buffer
            (setq buffer-file-name playbook)
            (insert-file-contents playbook)
            (ansible-intelligence-parse-buffer)))
         (live-buffer nil))
    (unwind-protect
        (progn
          (ansible-intelligence-index-put-document index tasks-document)
          (ansible-intelligence-index-put-document index playbook-document)
          (setq live-buffer (generate-new-buffer " *ansible-intelligence-live*"))
          (with-current-buffer live-buffer
            (setq buffer-file-name tasks
                  ansible-intelligence-mode t
                  ansible-intelligence--document tasks-document
                  ansible-intelligence--project-index index)
            (insert-file-contents tasks)
            (goto-char (point-min))
            (insert "# unsaved insertion\n"))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence-mode t
                  ansible-intelligence--document playbook-document
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (goto-char (point-max))
            (search-backward "result")
            (forward-char (length "result"))
            (cl-letf (((symbol-function 'ansible-intelligence-index-start)
                       (lambda (_index) nil)))
              (let* ((backend 'ansible-intelligence-native)
                     (definitions
                      (xref-backend-definitions backend "result")))
                (should-not definitions)))))
      (when (buffer-live-p live-buffer)
        (kill-buffer live-buffer))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-native-eldoc-is-provenance-rich ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: command_result\n- debug:\n    msg: \"{{ command_result.stdout }}\"\n"
    (goto-char (point-max))
    (search-backward "command_result.stdout")
    (forward-char (length "command_result.stdout"))
    (let ((text (ansible-intelligence-native-eldoc)))
      (should (string-match-p "register" text))
      (should (string-match-p "source" text))
      (should (string-match-p "confidence" text))
      (should (string-match-p "definition" text))
      (should (string-match-p "stdout" text)))
    (goto-char (point-min))
    (search-forward "debug")
    (should-not (ansible-intelligence-native-eldoc))))

(ert-deftest ansible-intelligence-capf-exposes-provenance-documentation ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: command_result\n- debug:\n    msg: \"{{ command_ }}\"\n"
    (goto-char (point-max))
    (search-backward "command_")
    (forward-char (length "command_"))
    (let* ((capf (ansible-intelligence-completion-at-point))
           (candidates (nth 2 capf))
           (candidate
            (seq-find
             (lambda (item) (string= item "command_result"))
             candidates))
           (doc-function (plist-get (nthcdr 3 capf) :company-doc-buffer))
           (doc-buffer (and candidate (funcall doc-function candidate))))
      (should candidate)
      (should (functionp doc-function))
      (should (bufferp doc-buffer))
      (with-current-buffer doc-buffer
        (should (string-match-p "command_result: register"
                                (buffer-string)))
        (should (string-match-p "source" (buffer-string)))
        (should (string-match-p "confidence" (buffer-string)))
        (should (string-match-p "definition" (buffer-string)))))))

(ert-deftest ansible-intelligence-native-imenu-categories-and-ranges ()
  (ansible-intelligence-tests-with-buffer
      "- name: Play name\n  hosts: all\n  tasks:\n    - name: Task name\n      command: true\n      register: task_result\n    - set_fact:\n        configured: true\n  roles:\n    - web\n"
    (let ((index (ansible-intelligence-native-create-imenu-index)))
      (dolist (category '("Plays" "Tasks" "Registers" "Variables" "Roles"))
        (should (assoc category index)))
      (let ((task (assoc "Task name" (cdr (assoc "Tasks" index)))))
        (should task)
        (should (markerp (cdr task)))
        (should (= (marker-position (cdr task))
                   (1+ (string-match "Task name" (buffer-string))))))
    (setq-local ansible-intelligence-native--imenu-previous
                (lambda () '(("YAML heading" . 1))))
    (let ((index (ansible-intelligence-native-create-imenu-index)))
      (should (assoc "YAML heading" index))))))

(ert-deftest ansible-intelligence-native-flymake-reports-conservative-severities ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: known\n- debug:\n    msg: \"{{ unknown }}\"\n"
    (setq ansible-intelligence--project-index
          (ansible-intelligence-project-index-create
           :root default-directory :documents (make-hash-table :test #'equal)
           :status 'ready))
    (ansible-intelligence--analyze)
    (let ((diagnostics (ansible-intelligence-native--diagnostics)))
      (should diagnostics)
      (let ((diagnostic
             (seq-find
              (lambda (item)
                (eq (plist-get (flymake-diagnostic-data item) :reason)
                    'unresolved-reference))
              diagnostics)))
        (should diagnostic)
        (should (eq (flymake-diagnostic-type diagnostic) 'error)))))
  (ansible-intelligence-tests-with-buffer
      "- include_tasks: \"{{ include_file }}\"\n"
    (setq ansible-intelligence--project-index
          (ansible-intelligence-project-index-create
           :root default-directory :documents (make-hash-table :test #'equal)
           :status 'ready))
    (let ((diagnostics (ansible-intelligence-native--diagnostics)))
      (let ((diagnostic
             (seq-find
              (lambda (item)
                (eq (plist-get (flymake-diagnostic-data item) :reason)
                    'dynamic-include))
              diagnostics)))
        (should diagnostic)
        (should (eq (flymake-diagnostic-type diagnostic) 'warning))))))

(ert-deftest ansible-intelligence-native-flymake-generation-fences-report ()
  (ansible-intelligence-tests-with-buffer
      "- debug:\n    msg: \"{{ value }}\"\n"
    (let (scheduled reports)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (_delay _repeat function &rest args)
                   (setq scheduled (lambda () (apply function args)))
                   'fake-timer)))
        (ansible-intelligence-native-flymake-backend
         (lambda (diagnostics) (push diagnostics reports)))
        (insert " ")
        (funcall scheduled))
      ;; The snapshot tick changed before the scheduled callback ran.
      (should (= (length reports) 1))
      (should-not (car reports)))))

(ert-deftest ansible-intelligence-native-flymake-defers-stale-snapshot ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: known\n- debug:\n    msg: \"{{ unknown }}\"\n"
    (let (scheduled reports scheduled-analysis restarted)
      (insert " ")
      (let ((old-document ansible-intelligence--document))
        (should-not
         (ansible-intelligence-native--diagnostics
          (ansible-intelligence-document-tick old-document)))
        (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (_delay _repeat function &rest args)
                     (setq scheduled (lambda () (apply function args)))
                     'fake-timer))
                  ((symbol-function 'ansible-intelligence--schedule-analysis)
                   (lambda (&rest _)
                     (setq scheduled-analysis t)))
                  ((symbol-function 'flymake-start)
                   (lambda (&rest _)
                     (setq restarted t))))
          (ansible-intelligence-native-flymake-backend
           (lambda (diagnostics) (push diagnostics reports)))
          (funcall scheduled)
          (should (equal reports '(nil)))
          (should scheduled-analysis)
          (should-not restarted)
          (ansible-intelligence--analyze)
          (should restarted)
          (should-not ansible-intelligence-native--flymake-pending))))))

(ert-deftest ansible-intelligence-native-flymake-activation-coexists-and-tears-down ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (insert "- debug:\n    msg: hello\n")
    (let ((eglot-flymake (lambda (&rest _args) nil)))
      (add-hook 'flymake-diagnostic-functions eglot-flymake nil t)
      (unwind-protect
          (progn
            (ansible-intelligence-native-enable)
            (should (bound-and-true-p flymake-mode))
            (should ansible-intelligence-native--flymake-owned)
            (should (memq eglot-flymake flymake-diagnostic-functions))
            (should (memq #'ansible-intelligence-native-flymake-backend
                          flymake-diagnostic-functions))
            (ansible-intelligence-native-disable)
            (should (bound-and-true-p flymake-mode))
            (should-not ansible-intelligence-native--flymake-owned)
            (should (memq eglot-flymake flymake-diagnostic-functions))
            (should-not
             (memq #'ansible-intelligence-native-flymake-backend
                   flymake-diagnostic-functions)))
        (remove-hook 'flymake-diagnostic-functions eglot-flymake t)
        (when (bound-and-true-p flymake-mode)
          (flymake-mode -1))))))

(ert-deftest ansible-intelligence-native-flymake-keeps-preexisting-mode ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (insert "- debug:\n    msg: hello\n")
    (flymake-mode 1)
    (unwind-protect
        (progn
          (ansible-intelligence-native-enable)
          (should-not ansible-intelligence-native--flymake-owned)
          (ansible-intelligence-native-disable)
          (should (bound-and-true-p flymake-mode)))
      (when (bound-and-true-p flymake-mode)
        (flymake-mode -1)))))

(ert-deftest ansible-intelligence-native-adapters-do-not-parse-or-discover ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  register: result\n- debug:\n    msg: \"{{ result }}\"\n"
    (let (parsed discovered)
      (cl-letf (((symbol-function 'ansible-intelligence-parse-buffer)
                 (lambda (&rest _) (setq parsed t) (error "adapter parsed")))
                ((symbol-function 'project-files)
                 (lambda (&rest _) (setq discovered t) (error "adapter scanned"))))
        (goto-char (point-max))
        (search-backward "result")
        (ansible-intelligence-completion-at-point)
        (ansible-intelligence-native-context-at-point)
        (ansible-intelligence-native-eldoc)
        (let ((old ansible-intelligence-native--imenu-previous))
          (setq-local ansible-intelligence-native--imenu-previous nil)
          (ansible-intelligence-native-create-imenu-index)
          (setq-local ansible-intelligence-native--imenu-previous old))
        (ansible-intelligence-native--diagnostics))
      (should-not parsed)
      (should-not discovered))))

(ert-deftest ansible-intelligence-yaml-query-and-fallback-preserve-links ()
  (with-temp-buffer
    (setq buffer-file-name "/project/playbook.yml"
          major-mode 'yaml-ts-mode)
    (insert "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - ansible.builtin.import_tasks: tasks/setup.yml\n  roles:\n    - web\n")
    (treesit-parser-create 'yaml)
    (let* ((tree (ansible-intelligence--tree))
           (query (ansible-intelligence-yaml-query-extract tree))
           (query-links
            (mapcar #'ansible-intelligence-link-target (plist-get query :links)))
           fallback-links)
      (cl-letf (((symbol-function 'ansible-intelligence-yaml--query-records)
                 (lambda (&rest _) nil)))
        (setq fallback-links
              (mapcar #'ansible-intelligence-link-target
                      (plist-get (ansible-intelligence-yaml-query-extract tree)
                                 :links))))
      (should (plist-get query :available))
      (should (equal query-links fallback-links)))))

(ert-deftest ansible-intelligence-yaml-roles-ignore-nested-vars-in-query-and-fallback ()
  (ansible-intelligence-tests-with-buffer
      "- hosts: all\n  roles:\n    - role: web\n      vars:\n        foo: bar\n    - name: db\n      vars:\n        nested:\n          name: not_a_role\n"
    (treesit-parser-create 'yaml)
    (ansible-intelligence--analyze)
    (let* ((document ansible-intelligence--document)
           (links (ansible-intelligence-document-links document))
           (outline (ansible-intelligence-document-outline document))
           (role-links
            (mapcar #'ansible-intelligence-link-target links))
           (role-outline
            (mapcar #'ansible-intelligence-outline-entry-name outline))
           (imenu (ansible-intelligence-native-create-imenu-index))
           (imenu-roles (cdr (assoc "Roles" imenu)))
           fallback)
      (should (equal role-links '("web" "db")))
      (should (equal role-outline '("web" "db")))
      (should (equal (mapcar #'car imenu-roles) '("web" "db")))
      (cl-letf (((symbol-function 'ansible-intelligence-yaml--query-records)
                 (lambda (&rest _) nil)))
        (setq fallback
              (ansible-intelligence-yaml-query-extract
               (ansible-intelligence--tree))))
      (should (equal
               (mapcar #'ansible-intelligence-link-target
                       (plist-get fallback :links))
               '("web" "db")))
      (should (equal
               (mapcar #'ansible-intelligence-outline-entry-name
                       (plist-get fallback :outline))
               '("web" "db"))))))

(ert-deftest ansible-intelligence-bare-conditions-map-scalar-and-list-offsets ()
  (ansible-intelligence-tests-with-buffer
      "- name: Conditions\n  when: service_enabled and count > 0\n  failed_when:\n    - result.rc != 0\n    - result.stdout == bad\n"
    (let* ((document ansible-intelligence--document)
           (references (ansible-intelligence-document-references document))
           (names (mapcar #'ansible-intelligence-reference-name references)))
      (should (equal names '("service_enabled" "count" "result" "result" "bad")))
      (dolist (reference references)
        (let* ((range (ansible-intelligence-reference-range reference))
               (text (buffer-substring-no-properties
                      (ansible-intelligence-range-start range)
                      (ansible-intelligence-range-end range))))
          (should (string-match-p
                   (regexp-quote
                    (ansible-intelligence-reference-name reference))
                   text)))))))

(ert-deftest ansible-intelligence-loop-literals-are-not-bare-references ()
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  loop: [one, two]\n"
    (should-not
     (ansible-intelligence-document-references ansible-intelligence--document))
    (let ((index
           (ansible-intelligence-project-index-create
            :root default-directory
            :documents (make-hash-table :test #'equal)
            :status 'ready)))
      (setq ansible-intelligence--project-index index)
      (should-not (ansible-intelligence-native--diagnostics))))
  (ansible-intelligence-tests-with-buffer
      "- command: true\n  loop: items\n"
    (should (equal
             (mapcar #'ansible-intelligence-reference-name
                     (ansible-intelligence-document-references
                      ansible-intelligence--document))
             '("items")))))

(ert-deftest ansible-intelligence-quoted-bare-conditions-preserve-offsets ()
  (ansible-intelligence-tests-with-buffer
      "- name: Conditions\n  when: \"service_enabled and count > 0\"\n  failed_when:\n    - 'result.rc != 0'\n"
    (should (equal
             (mapcar #'ansible-intelligence-reference-name
                     (ansible-intelligence-document-references
                      ansible-intelligence--document))
             '("service_enabled" "count" "result")))
    (treesit-parser-create 'yaml)
    (ansible-intelligence--analyze)
    (let ((contents (buffer-string))
          (references
           (ansible-intelligence-document-references
            ansible-intelligence--document)))
      (should (equal (mapcar #'ansible-intelligence-reference-name references)
                     '("service_enabled" "count" "result")))
      (dolist (reference references)
        (let* ((name (ansible-intelligence-reference-name reference))
               (range (ansible-intelligence-reference-range reference))
               (expected (string-match (regexp-quote name) contents)))
          (should (= (1+ expected)
                     (ansible-intelligence-range-start range)))
          (should (= (+ 1 expected (length name))
                     (ansible-intelligence-range-end range)))
          (should (equal
                   (buffer-substring-no-properties
                    (ansible-intelligence-range-start range)
                    (ansible-intelligence-range-end range))
                   name)))))))

(ert-deftest ansible-intelligence-bash-ranges-are-optional-and-structural ()
  (ansible-intelligence-tests-with-buffer
      "- name: Shell\n  ansible.builtin.shell: |\n    echo $HOME\n    echo hi\n"
    (let ((ranges
           (ansible-intelligence-document-shell-ranges
            ansible-intelligence--document)))
      (should (= (length ranges) 1))
      (should (eq (ansible-intelligence-shell-range-kind (car ranges))
                  'block-scalar))
      (if (ansible-intelligence-bash-grammar-available-p)
          (should (eq (ansible-intelligence-shell-range-parser (car ranges))
                      'bash))
        (should-not (ansible-intelligence-shell-range-parser (car ranges)))))))

(ert-deftest ansible-intelligence-index-retains-completed-entry-for-stale-live-edit ()
  (let* ((root (make-temp-file "ansible-intelligence-stale-index-" t))
           (file (ansible-intelligence-tests--write-file
                  root "tasks/main.yml"
                  "- command: echo disk\n  register: disk_result\n"))
           (index
            (ansible-intelligence-project-index-create
             :root (file-name-as-directory root)
             :documents (make-hash-table :test #'equal)
             :files (list (file-truename file))
             :pending (list (file-truename file))
             :generation 1 :status 'indexing))
           (completed (ansible-intelligence--parse-file file))
           (live-buffer (generate-new-buffer " *ansible-intelligence-stale*")))
    (unwind-protect
          (progn
            (ansible-intelligence-index-put-document index completed)
            (with-current-buffer live-buffer
              (setq buffer-file-name file
                    ansible-intelligence-mode t)
              (insert-file-contents file)
              (set-buffer-modified-p nil)
              (setq-local ansible-intelligence--document
                          (ansible-intelligence-parse-buffer))
              (goto-char (point-max))
              (insert "- debug:\n    msg: unsaved\n"))
            (ansible-intelligence--index-next-batch index 1)
            (should (eq (ansible-intelligence-index-current-document index file)
                        completed))
            (should-not
             (ansible-intelligence-index-definitions index "unsaved")))
      (when (timerp (ansible-intelligence-project-index-timer index))
          (cancel-timer (ansible-intelligence-project-index-timer index)))
      (when (buffer-live-p live-buffer)
          (kill-buffer live-buffer))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-native-xref-declines-external-disk-edit ()
  (let* ((root (make-temp-file "ansible-intelligence-external-edit-" t))
           (playbook
            (ansible-intelligence-tests--write-file
             root "playbook.yml"
             "- import_tasks: tasks/main.yml\n  tasks:\n    - debug:\n        msg: \"{{ result }}\"\n"))
           (tasks
            (ansible-intelligence-tests--write-file
             root "tasks/main.yml"
             "- command: true\n  register: result\n"))
           (index
            (ansible-intelligence-project-index-create
             :root (file-name-as-directory root)
             :documents (make-hash-table :test #'equal)
             :status 'ready))
           reindex-called)
    (unwind-protect
          (progn
            (dolist (file (list playbook tasks))
              (ansible-intelligence-index-put-document
               index (ansible-intelligence--parse-file file)))
            (with-temp-buffer
              (setq buffer-file-name playbook
                    ansible-intelligence-mode t
                    ansible-intelligence--project-index index)
              (insert-file-contents playbook)
              (set-buffer-modified-p nil)
              (setq-local ansible-intelligence--document
                          (ansible-intelligence-parse-buffer))
              (with-temp-file tasks
                (insert "- command: true\n  register: changed_result\n"))
              (goto-char (point-max))
              (search-backward "result")
              (forward-char (length "result"))
              (cl-letf (((symbol-function 'ansible-intelligence-index-start)
                         (lambda (_index) (setq reindex-called t))))
                (should-not
                 (xref-backend-definitions
                  'ansible-intelligence-native "result")))
              (should reindex-called)))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-unmodified-stale-buffer-is-live-only ()
  (let* ((temporary-file-directory
          (expand-file-name "ansible-intelligence-test-tmp/" default-directory))
         (root (progn
                 (make-directory temporary-file-directory t)
                 (make-temp-file "project-" t)))
         (vars
          (ansible-intelligence-tests--write-file
           root "vars/common.yml" "oldvar: present\n"))
         (playbook
          (ansible-intelligence-tests--write-file
           root "playbook.yml"
           "- hosts: all\n  vars_files:\n    - vars/common.yml\n  tasks:\n    - debug:\n        msg: \"{{ oldvar }}\"\n"))
         (index
          (ansible-intelligence-project-index-create
           :root (file-name-as-directory root)
           :documents (make-hash-table :test #'equal)
           :status 'ready))
         (vars-buffer (find-file-noselect vars))
         vars-document)
    (unwind-protect
        (progn
          (with-current-buffer vars-buffer
            (setq-local ansible-intelligence--project-index index
                        ansible-intelligence-mode t)
            (set-buffer-modified-p nil)
            (setq vars-document (ansible-intelligence-parse-buffer 'vars-file))
            (setq-local ansible-intelligence--document vars-document)
            (should-not (buffer-modified-p))
            (ansible-intelligence-index-put-document index vars-document))
          (with-temp-file vars
            (insert "newvar: present\n"))
          (with-current-buffer vars-buffer
            (setq vars-document (ansible-intelligence-parse-buffer 'vars-file))
            (setq-local ansible-intelligence--document vars-document)
            (should-not (buffer-modified-p))
            (should (ansible-intelligence-source-identity-live-p
                     (ansible-intelligence-document-source-identity
                      vars-document)))
            (ansible-intelligence-index-put-document index vars-document))
          (kill-buffer vars-buffer)
          (should-not
           (ansible-intelligence-index-document-current-p vars-document))
          (ansible-intelligence-index-put-document
           index (ansible-intelligence--parse-file playbook))
          (with-temp-buffer
            (setq buffer-file-name playbook
                  ansible-intelligence-mode t
                  ansible-intelligence--project-index index)
            (insert-file-contents playbook)
            (setq-local ansible-intelligence--document
                        (ansible-intelligence-parse-buffer))
            (goto-char (point-max))
            (search-backward "oldvar")
            (forward-char (length "oldvar"))
            (should-not
             (xref-backend-definitions
              'ansible-intelligence-native "oldvar"))))
      (when (buffer-live-p vars-buffer)
        (kill-buffer vars-buffer))
      (delete-directory root t)
      (when (file-directory-p temporary-file-directory)
        (delete-directory temporary-file-directory t)))))

(ert-deftest ansible-intelligence-mapping-relationships-ignore-metadata ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                      (treesit-language-available-p 'yaml)))
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (insert
     "- include_tasks:\n    file: tasks/main.yml\n    apply:\n      tags: ignored.yml\n- import_tasks: {file: tasks/flow.yml, apply: {tags: ignored-flow.yml}}\n- include_vars:\n    file: vars/common.yml\n    name: ignored-name\n- import_tasks: tasks/direct.yml\n")
    (treesit-parser-create 'yaml)
    (let ((query
             (mapcar #'ansible-intelligence-link-target
                     (ansible-intelligence-document-links
                      (ansible-intelligence-parse-buffer))))
            fallback)
      (cl-letf (((symbol-function 'ansible-intelligence-yaml--query-records)
                   (lambda (&rest _) nil)))
          (setq fallback
                (mapcar #'ansible-intelligence-link-target
                        (ansible-intelligence-document-links
                         (ansible-intelligence-parse-buffer)))))
      (should (equal query
                       '("tasks/main.yml" "tasks/flow.yml"
                         "vars/common.yml" "tasks/direct.yml")))
      (should (equal fallback query)))))

(ert-deftest ansible-intelligence-open-and-closed-flow-yaml-are-equivalent ()
  (skip-unless (and (fboundp 'treesit-language-available-p)
                      (treesit-language-available-p 'yaml)))
  (let* ((root (make-temp-file "ansible-intelligence-flow-" t))
           (file
            (ansible-intelligence-tests--write-file
             root "playbook.yml"
             "- name: Flow play\n  hosts: all\n  roles: [{role: web, vars: {name: ignored}}]\n  tasks:\n    - include_tasks: {file: tasks/flow.yml, apply: {tags: [flow]}}\n")))
    (unwind-protect
        (let* ((closed (ansible-intelligence--parse-file file))
                 (index
                  (ansible-intelligence-project-index-create
                   :root (file-name-as-directory root)
                   :documents (make-hash-table :test #'equal)
                   :status 'ready)))
            (ansible-intelligence-index-put-document index closed)
            (should (eq (ansible-intelligence-index-current-document index file)
                        closed))
            (with-temp-buffer
              (setq buffer-file-name file
                    major-mode 'yaml-ts-mode)
              (insert-file-contents file)
              (set-buffer-modified-p nil)
              (treesit-parser-create 'yaml)
              (let ((open (ansible-intelligence-parse-buffer)))
                (should
                 (equal
                  (mapcar #'ansible-intelligence-link-target
                          (ansible-intelligence-document-links open))
                  (mapcar #'ansible-intelligence-link-target
                          (ansible-intelligence-document-links closed))))
                (should
                 (equal
                  (mapcar #'ansible-intelligence-outline-entry-name
                          (ansible-intelligence-document-outline open))
                  (mapcar #'ansible-intelligence-outline-entry-name
                          (ansible-intelligence-document-outline closed)))))))
      (delete-directory root t))))

(ert-deftest ansible-intelligence-native-eldoc-teardown-is-conservative ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (setq-local eldoc-documentation-functions nil)
    (eldoc-mode -1)
    (unwind-protect
          (progn
            (ansible-intelligence-native-enable)
            (should (bound-and-true-p eldoc-mode))
            (ansible-intelligence-native-disable)
            (should-not (bound-and-true-p eldoc-mode)))
      (ansible-intelligence-native-disable)
      (when (bound-and-true-p eldoc-mode)
          (eldoc-mode -1))))
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (setq-local eldoc-documentation-functions nil)
    (eldoc-mode -1)
    (let ((consumer (lambda (&rest _args) "later")))
      (unwind-protect
            (progn
              (ansible-intelligence-native-enable)
              (add-hook 'eldoc-documentation-functions consumer nil t)
              (ansible-intelligence-native-disable)
              (should (bound-and-true-p eldoc-mode)))
          (remove-hook 'eldoc-documentation-functions consumer t)
          (ansible-intelligence-native-disable)
          (when (bound-and-true-p eldoc-mode)
            (eldoc-mode -1))))))

(ert-deftest ansible-intelligence-native-flymake-teardown-clears-and-is-idempotent ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (setq-local flymake-diagnostic-functions nil)
    (let (reports)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                   (lambda (&rest _args) 'fake-timer)))
          (ansible-intelligence-native-flymake-backend
           (lambda (diagnostics) (push diagnostics reports))))
      (setq reports '(existing))
      (ansible-intelligence-native-disable)
      (should (member nil reports))
      (ansible-intelligence-native-disable)
      (should-not
       (memq #'ansible-intelligence-native-flymake-backend
               flymake-diagnostic-functions)))))

(ert-deftest ansible-intelligence-native-flymake-ai-only-teardown-disables-mode ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (setq-local flymake-diagnostic-functions nil)
    (flymake-mode -1)
    (unwind-protect
        (progn
          (ansible-intelligence-native-enable)
          (should ansible-intelligence-native--flymake-owned)
          (ansible-intelligence-native-disable)
          (should-not (bound-and-true-p flymake-mode))
          (ansible-intelligence-native-disable)
          (should-not (bound-and-true-p flymake-mode)))
      (ansible-intelligence-native-disable)
      (when (bound-and-true-p flymake-mode)
        (flymake-mode -1)))))

(ert-deftest ansible-intelligence-native-flymake-keeps-eglot-added-after-ai ()
  (with-temp-buffer
    (setq major-mode 'yaml-ts-mode)
    (setq-local flymake-diagnostic-functions nil)
    (flymake-mode -1)
    (let ((eglot-flymake (lambda (&rest _args) nil)))
      (unwind-protect
            (progn
              (ansible-intelligence-native-enable)
              (should ansible-intelligence-native--flymake-owned)
              (add-hook 'flymake-diagnostic-functions eglot-flymake nil t)
              (ansible-intelligence-native-disable)
              (should (bound-and-true-p flymake-mode)))
          (remove-hook 'flymake-diagnostic-functions eglot-flymake t)
          (ansible-intelligence-native-disable)
          (when (bound-and-true-p flymake-mode)
            (flymake-mode -1))))))

(provide 'ansible-intelligence-tests)

;;; ansible-intelligence-tests.el ends here
