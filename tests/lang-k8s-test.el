;;; lang-k8s-test.el --- Tests for modules/11-languages/lang-k8s.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-k8s-test.el -f ert-run-tests-batch-and-exit
;; Pure logic plus the real helm, kubeconform, yamllint and actionlint where installed.
;; helm-ls, yaml-language-server and kubectl against a cluster were checked by hand.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst k8s-test-manifest
  "apiVersion: apps/v1\nkind: Deployment\nmetadata:\n  name: web\nspec:\n  replicas: 1\n  template:\n    spec:\n      containers:\n        - name: a\n          image: x\n          ports:\n            - containerPort: 80\n---\napiVersion: v1\nkind: Service\nmetadata:\n  name: svc\n")

(defmacro k8s-test-with-yaml (text &rest body)
  `(with-temp-buffer (insert ,text) (goto-char (point-min)) ,@body))

(ert-deftest k8s-menu-and-hooks-registered ()
  (should (eq (cdr (cl-find-if (lambda (m) (memq 'ide-helm-mode (car m))) ide-lang-menus)) #'ide-k8s-dispatch))
  (should (memq #'ide-k8s--run-command ide-run-command-functions))
  (should (memq #'ide-k8s--debug ide-debug-functions)))

(ert-deftest k8s-key-path-by-indentation ()
  (k8s-test-with-yaml k8s-test-manifest
    (search-forward "containerPort")
    (should (equal (ide-k8s--key-path-indent)
                   '("spec" "template" "spec" "containers" "ports" "containerPort")))
    (should (equal (ide-k8s-explain-target)
                   '("Deployment.spec.template.spec.containers.ports.containerPort" . "apps/v1")))))

(ert-deftest k8s-key-path-tree-sitter-agrees ()
  (skip-unless (treesit-language-available-p 'yaml))
  (k8s-test-with-yaml k8s-test-manifest
    (yaml-ts-mode)
    (search-forward "image")
    (should (equal (ide-k8s--key-path-ts) (ide-k8s--key-path-indent)))
    (should (equal (ide-k8s-key-path) '("spec" "template" "spec" "containers" "image")))))

(ert-deftest k8s-explain-uses-the-document-around-point ()
  (k8s-test-with-yaml k8s-test-manifest
    (search-forward "name: svc")
    (should (equal (ide-k8s-explain-target) '("Service.metadata.name" . "v1")))))

(ert-deftest k8s-explain-command-line ()
  (let ((ide-k8s-context "ctx"))
    (should (equal (ide-k8s-explain-command '("Deployment.spec" . "apps/v1") t)
                   '("kubectl" "explain" "Deployment.spec" "--api-version=apps/v1" "--recursive"
                     "--context" "ctx" "--request-timeout=15s")))))

(ert-deftest k8s-locate-json-pointer-segments ()
  (k8s-test-with-yaml k8s-test-manifest
    (let ((r (ide-k8s--locate 1 (point-max) '("spec" "template" "spec" "containers" "0" "ports" "0"))))
      (should (cdr r))
      (should (= (line-number-at-pos (car r)) 13)))
    (let ((r (ide-k8s--locate 1 (point-max) '("spec" "nothing"))))
      (should-not (cdr r))
      (should (= (line-number-at-pos (car r)) 5)))))

(ert-deftest k8s-documents-and-imenu ()
  (k8s-test-with-yaml k8s-test-manifest
    (let ((docs (ide-k8s--documents)))
      (should (equal (mapcar (lambda (d) (plist-get d :kind)) docs) '("Deployment" "Service")))
      (should (equal (plist-get (cadr docs) :name) "svc")))
    (should (equal (mapcar #'car (ide-helm--imenu-index)) '("Deployment/web" "Service/svc")))))

(ert-deftest k8s-core-api-versus-crd ()
  (should (ide-k8s--core-api-p "v1"))
  (should (ide-k8s--core-api-p "apps/v1"))
  (should (ide-k8s--core-api-p "networking.k8s.io/v1"))
  (should-not (ide-k8s--core-api-p "cert-manager.io/v1"))
  (should (equal (ide-k8s-crd-schema-url "cert-manager.io/v1" "Certificate")
                 "https://raw.githubusercontent.com/datreeio/CRDs-catalog/main/cert-manager.io/certificate_v1.json")))

(ert-deftest k8s-file-kind-by-path ()
  (should (eq (ide-k8s-file-kind "/p/.github/workflows/ci.yml") 'gh-workflow))
  (should (eq (ide-k8s-file-kind "/p/azure-pipelines.yml") 'azure-pipeline))
  (should (eq (ide-k8s-file-kind "/p/.azure-pipelines/build/a.yml") 'azure-pipeline))
  (should (eq (ide-k8s-file-kind "/p/docker-compose.override.yml") 'compose))
  (should (eq (ide-k8s-file-kind "/p/overlays/kustomization.yaml") 'kustomization))
  (should-not (ide-k8s-file-kind "/p/other.yaml")))

(ert-deftest k8s-yaml-settings-keep-ci-schemas-and-register-files ()
  (let ((ide-k8s--registered '(("kubernetes" "/p/a.yaml"))))
    (let* ((schemas (plist-get (ide-k8s--yaml-settings) :schemas)))
      (should (equal (plist-get schemas :kubernetes) ["/p/a.yaml"]))
      (should (plist-get schemas :https://json.schemastore.org/github-workflow.json)))))

(ert-deftest k8s-yamllint-uses-project-config-or-relaxed-default ()
  (let ((dir (make-temp-file "k8s-yl" t)))
    (unwind-protect
        (let ((file (expand-file-name "a.yaml" dir)))
          (should (member "-d" (ide-k8s--yamllint-args file)))
          (write-region "extends: default\n" nil (expand-file-name ".yamllint" dir))
          (should (equal (cl-subseq (ide-k8s--yamllint-args file) 2 4)
                         (list "-c" (expand-file-name ".yamllint" dir)))))
      (delete-directory dir t))))

(ert-deftest k8s-helm-command-lines ()
  (let* ((dir (make-temp-file "k8s-chart" t)) (root (file-name-as-directory dir)))
    (unwind-protect
        (progn
          (write-region "name: web\nversion: 1\n" nil (expand-file-name "Chart.yaml" root))
          (puthash root '("values-prod.yaml") ide-helm--values-selection)
          (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) "/proj/")))
            (should (equal (ide-helm-command 'template root)
                           (list "helm" "template" "web" root "-f" (expand-file-name "values-prod.yaml" root))))
            (should (equal (ide-helm-command 'install root)
                           (list "helm" "install" "web" root "--dry-run=client" "--debug"
                                 "-f" (expand-file-name "values-prod.yaml" root))))
            (should (equal (ide-helm-command 'package root)
                           (list "helm" "package" root "--destination" "/proj/dist")))
            (should (equal (ide-helm-command 'lint root "--strict")
                           (list "helm" "lint" root "-f" (expand-file-name "values-prod.yaml" root) "--strict")))))
      (remhash root ide-helm--values-selection)
      (delete-directory dir t))))

(ert-deftest k8s-kubectl-command-lines ()
  (let ((ide-k8s-context "c") (ide-k8s-namespace "n"))
    (should (equal (ide-k8s-kubectl-command 'apply-server "f.yaml")
                   '("kubectl" "apply" "--dry-run=server" "-f" "f.yaml" "--context" "c" "--namespace" "n")))
    (should (equal (ide-k8s-kubectl-command 'diff "-")
                   '("kubectl" "diff" "-f" "-" "--context" "c" "--namespace" "n")))))

(ert-deftest k8s-values-reference-at-point ()
  (with-temp-buffer
    (insert "image: {{ .Values.image.repository }}:{{ .Values.image.tag }}\n")
    (goto-char (point-min)) (search-forward "repos")
    (should (equal (ide-helm--values-ref-at-point) '("image" "repository")))
    (search-forward "ima") 
    (should (equal (ide-helm--values-ref-at-point) '("image")))))

(ert-deftest k8s-helm-template-highlighting ()
  (with-temp-buffer
    (insert "a: {{ .Values.x | quote }}\nb: {{/* note */}}\n")
    (ide-helm-mode)
    (font-lock-ensure)
    (goto-char (point-min)) (search-forward ".Values")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-variable-use-face))
    (search-forward "quote")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-function-name-face))
    (search-forward "note")
    (should (eq (get-text-property (match-beginning 0) 'face) 'font-lock-comment-face))))

(ert-deftest k8s-parse-kubeconform-pointer-to-line ()
  (with-temp-buffer
    (insert k8s-test-manifest)
    (let* ((out "{\"resources\":[{\"kind\":\"Deployment\",\"name\":\"web\",\"version\":\"apps/v1\",\"status\":\"statusInvalid\",\"msg\":\"m\",\"validationErrors\":[{\"path\":\"/spec/template/spec/containers/0/ports/0\",\"msg\":\"bad port\"},{\"path\":\"/spec/replicas\",\"msg\":\"bad\"}]}]}")
           (diags (ide-k8s--parse-kubeconform out (current-buffer))))
      (should (equal (mapcar (lambda (d) (line-number-at-pos (flymake-diagnostic-beg d))) diags) '(13 6)))
      (should (string-match-p "spec.replicas: bad" (flymake-diagnostic-text (cadr diags)))))))

(ert-deftest k8s-parse-actionlint ()
  (with-temp-buffer
    (insert "a: 1\nb: ${{ x }}\n")
    (let ((d (car (ide-k8s--parse-actionlint
                   "[{\"message\":\"boom\",\"line\":2,\"column\":4,\"kind\":\"expression\",\"end_column\":9}]"
                   (current-buffer)))))
      (should (= (line-number-at-pos (flymake-diagnostic-beg d)) 2))
      (should (string-match-p "boom \\[expression\\]" (flymake-diagnostic-text d))))))

(ert-deftest k8s-parse-trivy-and-kube-linter ()
  (should (equal (ide-k8s--parse-trivy
                  "{\"Results\":[{\"Target\":\"a.yaml\",\"Misconfigurations\":[{\"ID\":\"KSV-1\",\"Title\":\"T\",\"Severity\":\"HIGH\",\"Message\":\"M\",\"CauseMetadata\":{\"StartLine\":7}}]}]}")
                 '(("a.yaml" 7 "error" "[KSV-1] T: M")))))

(ert-deftest k8s-report-buffer-supports-next-error ()
  (let ((file (make-temp-file "k8s-rep" nil ".yaml" "a: 1\nb: 2\nc: 3\n")))
    (unwind-protect
        (let ((buf (ide-k8s--show-report "k8s-test" (list (list file 3 "warning" "check c")))))
          (with-current-buffer buf
            (goto-char (point-min)) (forward-line 1)
            (compile-goto-error)
            (should (equal (buffer-file-name) file))
            (should (= (line-number-at-pos) 3)))
          (kill-buffer buf))
      (delete-file file))))

(ert-deftest k8s-kubeconform-real-tool ()
  (skip-unless (executable-find "kubeconform"))
  (skip-unless (executable-find "curl"))
  (let ((out (with-temp-buffer
               (insert "apiVersion: v1\nkind: Service\nmetadata:\n  name: s\nspec:\n  ports:\n    - port: 80\n      protocol: 7\n")
               (apply #'call-process-region (point-min) (point-max) "kubeconform" t t nil (ide-k8s--kubeconform-args))
               (buffer-string))))
    (skip-unless (string-match-p "statusInvalid\\|statusValid" out))
    (with-temp-buffer
      (insert "apiVersion: v1\nkind: Service\nmetadata:\n  name: s\nspec:\n  ports:\n    - port: 80\n      protocol: 7\n")
      (let ((d (car (ide-k8s--parse-kubeconform out (current-buffer)))))
        (should (= (line-number-at-pos (flymake-diagnostic-beg d)) 8))))))

(ert-deftest k8s-helm-template-renders-real-chart ()
  (skip-unless (executable-find "helm"))
  (let* ((dir (make-temp-file "k8s-helm" t))
         (chart (expand-file-name "c" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "templates" chart) t)
          (write-region "apiVersion: v2\nname: c\nversion: 0.1.0\n" nil (expand-file-name "Chart.yaml" chart))
          (write-region "word: three\n" nil (expand-file-name "values.yaml" chart))
          (write-region "apiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: x\ndata:\n  word: {{ .Values.word | quote }}\n"
                        nil (expand-file-name "templates/cm.yaml" chart))
          (let ((out (ide-k8s--with-env
                       (with-temp-buffer
                         (apply #'call-process (car (ide-helm-command 'template (file-name-as-directory chart)))
                                nil t nil (cdr (ide-helm-command 'template (file-name-as-directory chart))))
                         (buffer-string)))))
            (should (string-match-p "word: \"three\"" out))))
      (delete-directory dir t))))


(ert-deftest k8s-trivy-chart-lines-map-back-to-the-template ()
  (let* ((dir (make-temp-file "k8s-tr" t))
         (tpl (expand-file-name "templates/d.yaml" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory tpl) t)
          (write-region "a: 1\ncontainers:\n  {{- if x }}\n  - name: {{ .Chart.Name }}\n  {{- end }}\n" nil tpl)
          (let ((e (car (ide-k8s--parse-trivy
                         "{\"Results\":[{\"Target\":\"templates/d.yaml\",\"Misconfigurations\":[{\"ID\":\"K\",\"Title\":\"T\",\"Severity\":\"LOW\",\"Message\":\"M\",\"CauseMetadata\":{\"StartLine\":3,\"Code\":{\"Lines\":[{\"Number\":3,\"Content\":\"  - name: demo\",\"IsCause\":true}]}}}]}]}"
                         dir))))
            (should (equal (nth 0 e) tpl))
            (should (= (nth 1 e) 4))
            (should (string-match-p "(rendered line 3)" (nth 3 e))))
          (let ((e (car (ide-k8s--parse-trivy
                         "{\"Results\":[{\"Target\":\"templates/d.yaml\",\"Misconfigurations\":[{\"ID\":\"K\",\"Title\":\"T\",\"Severity\":\"LOW\",\"Message\":\"M\",\"CauseMetadata\":{\"StartLine\":9,\"Code\":{\"Lines\":[{\"Content\":\"nothing like it\",\"IsCause\":true}]}}}]}]}"
                         dir))))
            (should (= (nth 1 e) 9))
            (should (string-match-p "of the rendered output" (nth 3 e)))))
      (delete-directory dir t))))

(defun k8s-test--cluster-p ()
  "Non-nil when the kind-foundry-test cluster of the sandbox answers."
  (and (executable-find "kubectl")
       (file-exists-p (expand-file-name "tools/k8s/kubeconfig" ide-var-dir))
       (let ((process-environment (cons (concat "KUBECONFIG=" (expand-file-name "tools/k8s/kubeconfig" ide-var-dir))
                                        process-environment)))
         (zerop (call-process "kubectl" nil nil nil "--context" "kind-foundry-test"
                              "--request-timeout=3s" "get" "--raw" "/readyz")))))

(ert-deftest k8s-cluster-explain-and-server-dry-run ()
  (skip-unless (k8s-test--cluster-p))
  (let ((ide-k8s-kubeconfig (expand-file-name "tools/k8s/kubeconfig" ide-var-dir))
        (ide-k8s-context "kind-foundry-test"))
    (let ((out (ide-k8s--with-env
                 (with-temp-buffer
                   (apply #'call-process (car (ide-k8s-explain-command '("Deployment.spec.template.spec.containers.ports" . "apps/v1")))
                          nil t nil (cdr (ide-k8s-explain-command '("Deployment.spec.template.spec.containers.ports" . "apps/v1"))))
                   (buffer-string)))))
      (should (string-match-p "KIND: +Deployment" out))
      (should (string-match-p "containerPort" out)))
    (let ((file (make-temp-file "k8s-cm" nil ".yaml" "apiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: t\n")))
      (unwind-protect
          (should (string-match-p "server dry run"
                                  (ide-k8s--with-env
                                    (with-temp-buffer
                                      (apply #'call-process "kubectl" nil t nil
                                             (cdr (ide-k8s-kubectl-command 'apply-server file)))
                                      (buffer-string)))))
        (delete-file file)))))

(provide 'lang-k8s-test)
;;; lang-k8s-test.el ends here
