;;; lang-k8s.el --- Helm, Kubernetes manifests, CI pipelines: YAML tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; The DevOps side of YAML, without leaving Emacs:
;;
;;   helm charts   `ide-helm-mode' (YAML with Go-template highlighting, no false YAML
;;                 errors on {{ }}) for templates/, helm-ls through Eglot, jump from
;;                 .Values.a.b to its key in values.yaml and back, helm lint / template /
;;                 dependency update / package / install --dry-run --debug
;;   manifests     yaml-language-server with the Kubernetes schema for core resources,
;;                 CRD schema hints, kubeconform (Flymake), `kubectl explain' for the field
;;                 at point, kubectl apply --dry-run / diff, kustomize, kube-linter, trivy
;;   pipelines     SchemaStore schemas for GitHub Actions, Azure Pipelines and docker-compose,
;;                 actionlint (Flymake) for workflows
;;   everything    yamllint (Flymake; honours the project's .yamllint)
;;
;; C-c m opens the menu.  Ansible stays with lang-ansible.el: a manifest, a chart file and
;; an Ansible file are told apart here and there by `ide--ansible-file-p' and the chart
;; layout, so the two never claim the same buffer.  The cluster-facing commands (apply,
;; diff, explain, server dry-runs) need a reachable cluster; the command lines are built
;; and tested without one.  Helm, kubectl and trivy write their caches under var/tools/k8s/.

;;; Code:

(eval-when-compile (require 'eglot))
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'transient)
(require 'compile)
(require 'imenu)

(defvar ide-run-command-functions)
(defvar ide-debug-functions)
(defvar ide-cache-dir)
(defvar ide-eglot-format-on-save)
(defvar yaml-indent-offset)
(defvar eglot-server-programs)
(defvar kubel-context)
(declare-function ide-lang-run "00-lang-lib")
(declare-function ide-lang-exe-or-error "00-lang-lib")
(declare-function ide-lang-root "00-lang-lib")
(declare-function ide-lang-browse "00-lang-lib")
(declare-function ide-lang-tools-dir "00-lang-lib")
(declare-function ide-lang-diag "00-lang-lib")
(declare-function ide-lang-parse-regexp "00-lang-lib")
(declare-function ide-lang-register-menu "00-lang-lib")
(declare-function ide-lang-workspace-config "00-lang-lib")
(declare-function ide-lang-define-linter "00-lang-lib")
(declare-function ide--ansible-file-p "lang-ansible")
(declare-function eglot-ensure "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-reconnect "eglot")
(declare-function eglot-signal-didChangeConfiguration "eglot")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-child-by-field-name "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function kubel "kubel")
(declare-function yaml-mode "yaml-mode")

(defgroup ide-k8s nil "Helm, Kubernetes and CI YAML support." :group 'ide-lang)

(defcustom ide-k8s-context nil
  "kubectl context for the commands of this module; nil means the current one."
  :type '(choice (const nil) string))

(defcustom ide-k8s-namespace nil
  "kubectl namespace for the commands of this module; nil means the context default."
  :type '(choice (const nil) string))

(defcustom ide-k8s-kubernetes-version nil
  "Kubernetes version kubeconform validates against (\"1.30.0\"), or nil for the latest."
  :type '(choice (const nil) string))

(defcustom ide-k8s-crd-catalog
  "https://raw.githubusercontent.com/datreeio/CRDs-catalog/main/{{.Group}}/{{.ResourceKind}}_{{.ResourceAPIVersion}}.json"
  "Schema location template for custom resources (kubeconform and the schema comment)."
  :type '(choice (const nil) string))

(defcustom ide-k8s-kubeconfig nil
  "Kubeconfig file for kubectl, helm and kubel started from here, or nil.
nil leaves KUBECONFIG as Emacs inherited it (and kubectl's own default after that)."
  :type '(choice (const nil) file))

(defcustom ide-helm-extra-args nil
  "Extra arguments added to helm lint, template and install."
  :type '(repeat string))

;;;; Sandboxed caches ---------------------------------------------------------------

(defun ide-k8s--env ()
  "Environment entries that keep helm, kubectl and trivy out of the home directory."
  (let ((dir (ide-lang-tools-dir "k8s" "state")))
    (append
     (when ide-k8s-kubeconfig (list (concat "KUBECONFIG=" (expand-file-name ide-k8s-kubeconfig))))
     (list (concat "HELM_CACHE_HOME=" dir "/helm/cache")
          (concat "HELM_CONFIG_HOME=" dir "/helm/config")
          (concat "HELM_DATA_HOME=" dir "/helm/data")
          (concat "KUBECACHEDIR=" dir "/kube-cache")
          (concat "TRIVY_CACHE_DIR=" dir "/trivy-cache")
           (concat "TMPDIR=" (let ((tmp (concat dir "/tmp"))) (make-directory tmp t) tmp))))))

(defmacro ide-k8s--with-env (&rest body)
  "Run BODY with `ide-k8s--env' in front of `process-environment'."
  (declare (indent 0))
  `(let ((process-environment (append (ide-k8s--env) process-environment))) ,@body))

(defun ide-k8s--cache-dir (name)
  "A cache directory NAME under the sandbox cache dir (created)."
  (let ((d (expand-file-name (concat "k8s-" name "/") ide-cache-dir)))
    (make-directory d t)
    d))

;;;; Detecting what a file is -----------------------------------------------------------

(defun ide-helm-chart-root (&optional file)
  "Directory of the Helm chart FILE (default: this buffer) belongs to, or nil."
  (when-let* ((start (or file buffer-file-name default-directory))
              (dir (locate-dominating-file start "Chart.yaml")))
    (file-name-as-directory (expand-file-name dir))))

(defun ide-helm--template-file-p (&optional file)
  "Non-nil if FILE is under the templates/ directory of a chart."
  (when-let* ((file (or file buffer-file-name))
              (root (ide-helm-chart-root file)))
    (string-prefix-p "templates/" (file-relative-name file root))))

(defun ide-helm--chart-file-p (&optional file)
  "Non-nil if FILE is Chart.yaml or a values file of a chart (not a template)."
  (when-let* ((file (or file buffer-file-name))
              (root (ide-helm-chart-root file)))
    (let ((rel (file-relative-name file root)))
      (string-match-p "\\`\\(Chart\\.ya?ml\\|values[^/]*\\.ya?ml\\|ci/[^/]*\\.ya?ml\\)\\'" rel))))

(defun ide-k8s--manifest-in-text-p ()
  "Non-nil if the buffer has a top-level apiVersion and kind near its start."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((limit (min (point-max) (+ (point-min) 40000))))
        (and (re-search-forward "^apiVersion:[ \t]*[^ \t\n]" limit t)
             (progn (goto-char (point-min))
                    (re-search-forward "^kind:[ \t]*[^ \t\n]" limit t))
             t)))))

(defun ide-k8s-file-kind (&optional file)
  "What the YAML FILE (default: this buffer's) is, as a symbol.
One of `helm-template', `helm-chart', `gh-workflow', `azure-pipeline', `compose',
`kustomization', `k8s-manifest', or nil.  Ansible files are never manifests."
  (let ((file (or file buffer-file-name)))
    (cond
     ((and file (ide-helm--template-file-p file)) 'helm-template)
     ((and file (ide-helm--chart-file-p file)) 'helm-chart)
     ((and file (string-match-p "/\\.github/workflows/[^/]+\\.ya?ml\\'" file)) 'gh-workflow)
     ((and file (string-match-p "\\(/azure-pipelines[^/]*\\.ya?ml\\|/\\.azure-pipelines/.*\\.ya?ml\\)\\'" file))
      'azure-pipeline)
     ((and file (string-match-p "/\\(docker-\\)?compose[^/]*\\.ya?ml\\'" file)) 'compose)
     ((and file (string-match-p "/kustomization\\.ya?ml\\'" file)) 'kustomization)
     ((and (eq file buffer-file-name) (not (and (fboundp 'ide--ansible-file-p) (ide--ansible-file-p)))
           (ide-k8s--manifest-in-text-p))
      'k8s-manifest))))

(defun ide-k8s--core-api-p (api-version)
  "Non-nil if API-VERSION belongs to the built-in Kubernetes API (not a CRD)."
  (let ((group (if (string-match "\\`\\(.+\\)/[^/]+\\'" api-version) (match-string 1 api-version) "")))
    (or (string-empty-p group)
        (string-suffix-p ".k8s.io" group)
        (member group '("apps" "batch" "policy" "autoscaling" "extensions")))))

(defun ide-k8s-crd-schema-url (api-version kind)
  "Schema URL in the CRD catalog for a resource of API-VERSION and KIND, or nil."
  (when (and ide-k8s-crd-catalog (string-match "\\`\\(.+\\)/\\([^/]+\\)\\'" api-version))
    (let ((group (match-string 1 api-version)) (version (match-string 2 api-version)))
      (replace-regexp-in-string
       "{{\\.ResourceAPIVersion}}" version
       (replace-regexp-in-string
        "{{\\.ResourceKind}}" (downcase kind)
        (replace-regexp-in-string "{{\\.Group}}" group ide-k8s-crd-catalog t t) t t)
       t t))))

;;;; Reading YAML structure by indentation -----------------------------------------------

(defconst ide-k8s--key-regexp
  "\\(?:\"\\([^\"\n]+\\)\"\\|'\\([^'\n]+\\)'\\|\\([^ \t\n\"'#{[][^:\n]*?\\)\\)[ \t]*:\\(?:[ \t]\\|$\\)"
  "Matches a mapping key and its colon; the key is group 1, 2 or 3.")

(defun ide-k8s--line-info ()
  "For the line at point: (POS EFFECTIVE-INDENT DASH-COLUMN KEY), or nil to skip it.
EFFECTIVE-INDENT counts the \"- \" prefixes; DASH-COLUMN is the column of the first
dash.  Blank lines, comments, document markers and template-only lines give nil."
  (save-excursion
    (forward-line 0)
    (when (and (looking-at "\\( *\\)\\(\\(?:- +\\)*\\)\\([^ \t\n#]\\)")
               (not (looking-at " *\\(?:---\\|\\.\\.\\.\\)\\(?:[ \t]\\|$\\)"))
               (not (looking-at " *{{")))
      (let* ((indent (length (match-string 1)))
             (dashes (length (match-string 2)))
             (bol (line-beginning-position))
             (key (progn (goto-char (+ bol indent dashes))
                         (when (looking-at ide-k8s--key-regexp)
                           (or (match-string-no-properties 1) (match-string-no-properties 2) (match-string-no-properties 3))))))
        (list bol (+ indent dashes) (and (> dashes 0) indent) key)))))

(defun ide-k8s--scan (beg end)
  "Vector of `ide-k8s--line-info' for the content lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (forward-line 0)
    (let (acc)
      (while (< (point) end)
        (when-let* ((info (ide-k8s--line-info))) (push info acc))
        (forward-line 1))
      (vconcat (nreverse acc)))))

(defun ide-k8s--locate (beg end segments)
  "Find the key path SEGMENTS (strings; digits index a sequence) between BEG and END.
Return (POSITION . COMPLETE): the start of the line of the deepest key found, and
whether every segment was found."
  (let* ((v (ide-k8s--scan beg end)) (i 0) (j (length v)) (pos beg) (complete t))
    (catch 'stop
      (dolist (seg segments)
        (let* ((dash-lines (cl-loop for k from i below j when (nth 2 (aref v k)) collect k))
               (min-dash (and dash-lines (apply #'min (mapcar (lambda (k) (nth 2 (aref v k))) dash-lines))))
               (starts (and min-dash (cl-remove-if-not (lambda (k) (= (nth 2 (aref v k)) min-dash)) dash-lines)))
               (min-eff (and (< i j) (apply #'min (cl-loop for k from i below j collect (nth 1 (aref v k)))))))
          (cond
           ((and starts (string-match-p "\\`[0-9]+\\'" seg))
            (let ((s (nth (string-to-number seg) starts)))
              (unless s (setq complete nil) (throw 'stop nil))
              (setq pos (car (aref v s))
                    j (or (nth (1+ (string-to-number seg)) starts) j)
                    i s)))
           (t
            (let ((m (and min-eff
                          (cl-loop for k from i below j
                                   when (and (= (nth 1 (aref v k)) min-eff) (equal (nth 3 (aref v k)) seg))
                                   return k))))
              (unless m (setq complete nil) (throw 'stop nil))
              (setq pos (car (aref v m))
                    i (1+ m)
                    j (or (cl-loop for k from (1+ m) below j
                                   when (<= (nth 1 (aref v k)) min-eff) return k)
                          j))))))))
    (cons pos complete)))

(defun ide-k8s--key-path-indent ()
  "Key path of the line at point (outermost first), from the indentation."
  (save-excursion
    (forward-line 0)
    (let ((info (ide-k8s--line-info)) path eff)
      (while (and (not info) (zerop (forward-line -1)))
        (setq info (ide-k8s--line-info)))
      (when info
        (setq eff (nth 1 info))
        (when (nth 3 info) (push (nth 3 info) path))
        (goto-char (car info))
        (while (and (zerop (forward-line -1)) (not (looking-at "---")))
          (when-let* ((i (ide-k8s--line-info)))
            (when (and (< (nth 1 i) eff) (nth 3 i))
              (push (nth 3 i) path)
              (setq eff (nth 1 i))))))
      path)))

(defun ide-k8s--key-path-ts ()
  "Key path at point (outermost first) from the tree-sitter YAML tree."
  (let ((node (treesit-node-at (point))) path)
    (while node
      (when (equal (treesit-node-type node) "block_mapping_pair")
        (when-let* ((k (treesit-node-child-by-field-name node "key")))
          (push (string-trim (treesit-node-text k t) "[\"']" "[\"']") path)))
      (setq node (treesit-node-parent node)))
    path))

(defun ide-k8s-key-path ()
  "The key path at point, outermost first: tree-sitter when available, else indentation."
  (if (and (derived-mode-p 'yaml-ts-mode) (ignore-errors (treesit-parser-list)))
      (ide-k8s--key-path-ts)
    (ide-k8s--key-path-indent)))

;;;; Documents of a multi-document file -----------------------------------------------------

(defun ide-k8s--doc-bounds (&optional pos)
  "Start and end of the YAML document around POS (default point) as (BEG . END)."
  (save-excursion
    (goto-char (or pos (point)))
    (forward-line 0)
    (let ((beg (if (looking-at "---") (point)
                 (if (re-search-backward "^---\\(?:[ \t]\\|$\\)" nil t) (point) (point-min))))
          (end (progn (forward-line 1)
                      (if (re-search-forward "^---\\(?:[ \t]\\|$\\)" nil t)
                          (match-beginning 0) (point-max)))))
      (cons beg end))))

(defun ide-k8s--field (beg end segments)
  "The scalar value of the key path SEGMENTS in BEG..END, or nil."
  (save-excursion
    (let ((r (ide-k8s--locate beg end segments)))
      (when (cdr r)
        (goto-char (car r))
        (when (and (re-search-forward ide-k8s--key-regexp (line-end-position) t)
                   (looking-at "[ \t]*\\(?:\"\\([^\"]*\\)\"\\|'\\([^']*\\)'\\|\\([^ \t\n#][^\n#]*?\\)\\)[ \t]*\\(?:#.*\\)?$"))
          (or (match-string-no-properties 1) (match-string-no-properties 2) (match-string-no-properties 3)))))))

(defun ide-k8s--documents ()
  "The documents of the buffer: a list of plists (:beg :end :kind :name :api :line)."
  (save-excursion
    (save-restriction
      (widen)
      (let ((starts (list (point-min))) docs)
        (goto-char (point-min))
        (while (re-search-forward "^---\\(?:[ \t].*\\)?$" nil t)
          (push (1+ (match-end 0)) starts))
        (setq starts (nreverse starts))
        (cl-loop for (s next) on starts
                 for e = (or next (1+ (point-max)))
                 for end = (min (point-max) (if next (1- e) e))
                 when (< s end)
                 do (let* ((kind (ide-k8s--field s end '("kind")))
                           (loc (ide-k8s--locate s end '("kind"))))
                      (when kind
                        (push (list :beg s :end end :kind kind
                                    :name (ide-k8s--field s end '("metadata" "name"))
                                    :api (ide-k8s--field s end '("apiVersion"))
                                    :line (line-number-at-pos (car loc)))
                              docs))))
        (nreverse docs)))))

;;;; Helm template mode ---------------------------------------------------------------------

(defconst ide-helm--keywords
  '("if" "else" "end" "range" "with" "define" "template" "block" "include" "tpl" "break" "continue"
    "nil" "true" "false")
  "Go template keywords.")

(defconst ide-helm--functions
  '("toYaml" "toJson" "toPrettyJson" "fromYaml" "fromJson" "nindent" "indent" "quote" "squote" "default"
    "required" "printf" "print" "println" "lower" "upper" "title" "trim" "trimAll" "trimPrefix"
    "trimSuffix" "replace" "contains" "hasPrefix" "hasSuffix" "hasKey" "dict" "list" "set" "unset"
    "get" "index" "len" "not" "and" "or" "eq" "ne" "lt" "le" "gt" "ge" "empty" "coalesce" "ternary"
    "b64enc" "b64dec" "sha256sum" "lookup" "semverCompare" "join" "split" "splitList" "kindIs"
    "typeOf" "merge" "mergeOverwrite" "mustMergeOverwrite" "keys" "values" "pick" "omit" "first" "last"
    "uniq" "has" "until" "add" "sub" "mul" "div" "int" "int64" "toString" "regexMatch" "regexReplaceAll"
    "fail" "now" "date" "randAlphaNum" "genCA" "genSignedCert" "getHostByName" "lookup" "tuple"
    "trunc" "kebabcase" "snakecase" "camelcase" "cat" "wrap" "repeat" "substr" "urlParse" "fromYamlArray"
    "include" "mustRegexMatch")
  "Helm and Sprig functions worth highlighting.")

(defun ide-helm--in-action-p (pos)
  "Non-nil if POS lies inside a {{ ... }} action on its line."
  (save-match-data
    (save-excursion
      (goto-char pos)
      (let ((bol (line-beginning-position)))
        (and (search-backward "{{" bol t)
             (let ((open (point)))
               (not (save-excursion
                      (goto-char (+ open 2))
                      (re-search-forward "}}" pos t)))))))))

(defun ide-helm--matcher (regexp)
  "A font-lock matcher for REGEXP matches that lie inside a template action."
  (lambda (limit)
    (let (found)
      (while (and (not found) (re-search-forward regexp limit t))
        (when (ide-helm--in-action-p (match-beginning 0)) (setq found t)))
      found)))

(defun ide-helm--multiline-comment (limit)
  "Match a {{/* ... */}} comment, which may span lines, before LIMIT."
  (when (re-search-forward "{{[-~]?[ \t\n]*/\\*\\(?:.\\|\n\\)*?\\*/[ \t\n]*[-~]?}}" limit t)
    (put-text-property (match-beginning 0) (match-end 0) 'font-lock-multiline t)
    t))

(defvar ide-helm-font-lock-keywords
  `((ide-helm--multiline-comment (0 'font-lock-comment-face t))
    (,(ide-helm--matcher "\"\\(?:\\\\.\\|[^\"\\\n]\\)*\"\\|`[^`\n]*`") (0 'font-lock-string-face t))
    (,(ide-helm--matcher (concat "\\_<" (regexp-opt ide-helm--keywords) "\\_>")) (0 'font-lock-keyword-face t))
    (,(ide-helm--matcher (concat "\\_<" (regexp-opt ide-helm--functions) "\\_>")) (0 'font-lock-function-name-face t))
    (,(ide-helm--matcher "\\.\\(?:Values\\|Release\\|Chart\\|Capabilities\\|Template\\|Files\\)\\(?:\\.[A-Za-z0-9_-]+\\)*")
     (0 'font-lock-variable-use-face t))
    (,(ide-helm--matcher "\\$[A-Za-z0-9_]*") (0 'font-lock-variable-name-face t))
    ("[-~]?}}\\|{{[-~]?" (0 'font-lock-preprocessor-face t))
    ("{{[-~]?[ \t]*\\(/\\*\\)" (1 'font-lock-comment-delimiter-face t)))
  "Go template highlighting layered on top of the YAML mode's own.")

(defun ide-helm--imenu-index ()
  "Imenu index: the resources (Kind/name) and the named templates of the buffer."
  (let (items)
    (dolist (d (ide-k8s--documents))
      (push (cons (format "%s/%s" (plist-get d :kind) (or (plist-get d :name) "?"))
                  (save-excursion (goto-char (point-min)) (forward-line (1- (plist-get d :line))) (point)))
            items))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "{{-?[ \t]*\\(?:define\\|block\\)[ \t]+\"\\([^\"]+\\)\"" nil t)
        (push (cons (concat "define " (match-string-no-properties 1)) (match-beginning 0)) items)))
    (sort items (lambda (a b) (< (cdr a) (cdr b))))))

(defun ide-helm--setup ()
  "Settings shared by the Helm template modes."
  (setq-local tab-width 2 indent-tabs-mode nil
              comment-start "{{/* " comment-end " */}}"
              imenu-create-index-function #'ide-helm--imenu-index
              outline-regexp "{{-? *\\(?:define\\|block\\) "
              ide-eglot-format-on-save nil)
  (font-lock-add-keywords nil ide-helm-font-lock-keywords 'append)
  (setq-local font-lock-multiline t))

(defvar-keymap ide-helm-mode-map
  :doc "Keys of the Helm template modes."
  "C-c C-v" #'ide-helm-goto-values
  "C-c C-e" #'ide-k8s-explain)

(define-derived-mode ide-helm-mode yaml-mode "Helm"
  "Major mode for Helm chart templates: YAML with Go-template highlighting.
The server is helm-ls, which embeds yaml-language-server; formatting on save is off
because YAML formatters do not understand {{ }}."
  (setq-local yaml-indent-offset 2)
  (ide-helm--setup))

(define-derived-mode ide-helm-text-mode text-mode "Helm-Text"
  "Major mode for Helm helpers (_helpers.tpl) and NOTES.txt: text with Go templates."
  (ide-helm--setup))
(set-keymap-parent ide-helm-text-mode-map ide-helm-mode-map)

(defun ide-helm-auto-mode ()
  "Choose the mode of a file under a templates/ directory.
A chart's template gets `ide-helm-mode' (YAML) or `ide-helm-text-mode' (.tpl, .txt);
any other templates/ file keeps the plain YAML mode."
  (let ((file (or buffer-file-name "")))
    (cond ((and (ide-helm--template-file-p file) (string-match-p "\\.ya?ml\\'" file)) (ide-helm-mode))
          ((ide-helm--template-file-p file) (ide-helm-text-mode))
          ((string-match-p "\\.ya?ml\\'" file) (yaml-ts-mode))
          (t (text-mode)))))

(add-to-list 'auto-mode-alist '("/templates/.*\\.\\(ya?ml\\|tpl\\|txt\\)\\'" . ide-helm-auto-mode))
(add-to-list 'auto-mode-alist '("/_[^/]*\\.tpl\\'" . ide-helm-auto-mode))

(add-hook 'ide-helm-mode-hook #'eglot-ensure)
(add-hook 'ide-helm-text-mode-hook #'eglot-ensure)

;; devops-k8s.el sends /k8s/, /manifests/ and /kubernetes/ YAML to `k8s-mode' (a
;; yaml-mode with completion and snippets).  Chart templates there must still be
;; Helm templates, and the rest gets the same server and linters as yaml-ts-mode.
(defun ide-k8s--remap-k8s-mode ()
  "Start `ide-helm-mode' for chart templates, `k8s-mode' for any other manifest."
  (if (ide-helm--template-file-p)
      (ide-helm-auto-mode)
    (k8s-mode)))
(declare-function k8s-mode "k8s-mode")
(add-to-list 'major-mode-remap-alist '(k8s-mode . ide-k8s--remap-k8s-mode))

;; The yaml-mode package registers \.ya?ml\' ahead of every entry added before it
;; loads, and 05-treesitter remaps yaml-mode to yaml-ts-mode; remap it once more so
;; that chart templates reach `ide-helm-mode' whatever the order of the alist.
(defun ide-k8s--remap-yaml-mode ()
  "Start `ide-helm-mode' for chart templates, `yaml-ts-mode' for any other YAML."
  (if (ide-helm--template-file-p) (ide-helm-auto-mode) (yaml-ts-mode)))
(declare-function yaml-ts-mode "yaml-ts-mode")
(add-to-list 'major-mode-remap-alist '(yaml-mode . ide-k8s--remap-yaml-mode))
(add-hook 'k8s-mode-hook #'eglot-ensure)
(add-hook 'k8s-mode-hook (lambda () (setq-local tab-width 2)))

;;;; Servers -----------------------------------------------------------------------------------

(defun ide-helm-ls-program ()
  "Path of the helm-ls executable (mise names it helm_ls), or nil."
  (or (executable-find "helm_ls") (executable-find "helm-ls")))

(defun ide-helm--server-contact (&rest _)
  "The command line that starts helm-ls with helm's caches in the sandbox."
  (if-let* ((exe (ide-helm-ls-program)))
      (append '("env") (ide-k8s--env) (list exe "serve"))
    (user-error "helm-ls not found; run M-x ide-mise-setup")))

(defun ide-helm--project-find (dir)
  "A project rooted at the chart of DIR, for charts outside any VCS project.
helm-ls needs the chart directory as its workspace root, not templates/."
  (when-let* ((root (ide-helm-chart-root dir)))
    (cons 'transient root)))

;; After the VCS backend (depth 90): inside a git repository the repository stays the project.
(add-hook 'project-find-functions #'ide-helm--project-find 90)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '((k8s-mode :language-id "yaml") . ("yaml-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs
               `(((ide-helm-mode :language-id "helm") (ide-helm-text-mode :language-id "helm"))
                 . ,#'ide-helm--server-contact)))

;; helm-ls embeds yaml-language-server; its own settings live under "helm-ls".
(ide-lang-workspace-config
 :helm-ls
 '(:logLevel "info"
   :valuesFiles (:mainValuesFile "values.yaml"
                 :lintOverlayValuesFile "values.lint.yaml"
                 :additionalValuesFilesGlobPattern "values*.yaml")
   :yamlls (:enabled t
            :enabledForFilesGlob "*.{yaml,yml}"
            :diagnosticsLimit 50
            :showDiagnosticsDirectly nil
            :path "yaml-language-server"
            :initTimeoutSeconds 3
            :config (:completion t :hover t :validate t
                     :schemas (:kubernetes "templates/**")))))

;;;; yaml-language-server settings --------------------------------------------------------------

(defconst ide-k8s--schemas
  '(("https://json.schemastore.org/github-workflow.json"
     "**/.github/workflows/*.yml" "**/.github/workflows/*.yaml")
    ("https://json.schemastore.org/chart.json" "**/Chart.yaml")
    ("https://json.schemastore.org/github-action.json" "**/action.yml" "**/action.yaml")
    ("https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json"
     "**/azure-pipelines*.yml" "**/azure-pipelines*.yaml" "**/.azure-pipelines/**/*.yml" "**/.azure-pipelines/**/*.yaml")
    ("https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json"
     "**/docker-compose*.yml" "**/docker-compose*.yaml" "**/compose.yml" "**/compose.yaml")
    ("https://json.schemastore.org/kustomization.json" "**/kustomization.yml" "**/kustomization.yaml"))
  "Schemas for CI and deployment files: (URL GLOB...).
SchemaStore matching in yaml-language-server covers the same files; these entries
make it work offline of the catalog and without waiting for it.")

(defconst ide-k8s--kind-schema
  '((gh-workflow . "https://json.schemastore.org/github-workflow.json")
    (azure-pipeline . "https://raw.githubusercontent.com/microsoft/azure-pipelines-vscode/master/service-schema.json")
    (compose . "https://raw.githubusercontent.com/compose-spec/compose-spec/master/schema/compose-spec.json")
    (kustomization . "https://json.schemastore.org/kustomization.json")
    (k8s-manifest . "kubernetes"))
  "The schema key `ide-k8s-file-kind' maps to.  \"kubernetes\" is the server's built-in alias.")

(defvar ide-k8s--registered nil
  "Alist (SCHEMA . FILES): files the server was told to validate with that schema.
The `**/' globs of `ide-k8s--schemas' cannot cross a dot directory (a hidden
parent), so every file seen is also registered by its absolute path.")

(defun ide-k8s--yaml-settings ()
  "The `yaml' workspace settings: the static schemas plus the registered files."
  (let ((table (mapcar (lambda (s) (cons (car s) (copy-sequence (cdr s)))) ide-k8s--schemas)))
    (dolist (r ide-k8s--registered)
      (if-let* ((cell (assoc (car r) table)))
          (setcdr cell (append (cdr cell) (cdr r)))
        (push (cons (car r) (cdr r)) table)))
    (let (schemas)
      (dolist (s (nreverse table))
        (setq schemas (append schemas (list (if (equal (car s) "kubernetes") :kubernetes
                                              (intern (concat ":" (car s))))
                                            (vconcat (cdr s))))))
      `(:schemas ,schemas :validate t :hover t :completion t
        :schemaStore (:enable t :url "https://www.schemastore.org/api/json/catalog.json")
        :format (:enable t)))))

(defun ide-k8s--apply-yaml-settings ()
  "Store the merged `yaml' settings, keeping any keys other modules added."
  (with-eval-after-load 'eglot
    (let ((old (plist-get (default-value 'eglot-workspace-configuration) :yaml))
          (new (ide-k8s--yaml-settings)))
      (while old
        (unless (plist-member new (car old)) (setq new (append new (list (car old) (cadr old)))))
        (setq old (cddr old)))
      (ide-lang-workspace-config :yaml new))))

(ide-k8s--apply-yaml-settings)

(defun ide-k8s--manifest-core-p ()
  "Non-nil if every resource of this buffer belongs to the built-in Kubernetes API."
  (let ((docs (ide-k8s--documents)))
    (and docs (cl-every (lambda (d) (and (plist-get d :api) (ide-k8s--core-api-p (plist-get d :api)))) docs))))

(defun ide-k8s--register-file ()
  "Tell the server which schema this buffer's file uses.
Manifests get the Kubernetes schema only when all their resources are built in:
custom resources would be flagged as unknown kinds (see `ide-k8s-insert-schema-comment')."
  (when (and buffer-file-name (derived-mode-p 'yaml-ts-mode 'k8s-mode))
    (let* ((kind (ide-k8s-file-kind))
           (schema (cdr (assq kind ide-k8s--kind-schema))))
      (when (and schema (or (not (eq kind 'k8s-manifest)) (ide-k8s--manifest-core-p)))
        (let ((cell (assoc schema ide-k8s--registered)))
          (unless (member buffer-file-name (cdr cell))
            (if cell (push buffer-file-name (cdr cell))
              (push (list schema buffer-file-name) ide-k8s--registered))
            (ide-k8s--apply-yaml-settings)
            (when-let* ((server (ignore-errors (eglot-current-server))))
              (eglot-signal-didChangeConfiguration server))))))))

(add-hook 'eglot-managed-mode-hook #'ide-k8s--register-file)

;;;; Manifest minor mode -------------------------------------------------------------------------

(defun ide-k8s--manifest-imenu ()
  "Imenu index of a manifest: one Kind/name entry per document."
  (ide-helm--imenu-index))

(defvar-keymap ide-k8s-mode-map
  :doc "Keys of `ide-k8s-mode'."
  "C-c C-e" #'ide-k8s-explain)

(define-minor-mode ide-k8s-mode
  "Kubernetes manifest conveniences: Kind/name Imenu, `kubectl explain' on C-c C-e."
  :lighter " K8s"
  (if ide-k8s-mode
      (setq-local imenu-create-index-function #'ide-k8s--manifest-imenu)
    (kill-local-variable 'imenu-create-index-function)))

(defun ide-k8s--maybe-enable ()
  "Enable `ide-k8s-mode' for manifests, kustomizations and chart files."
  (when (memq (ide-k8s-file-kind) '(k8s-manifest kustomization helm-chart))
    (ide-k8s-mode 1)))

(add-hook 'yaml-ts-mode-hook #'ide-k8s--maybe-enable)
(add-hook 'k8s-mode-hook #'ide-k8s--maybe-enable)

;;;; Linters -------------------------------------------------------------------------------------

(defun ide-k8s--yamllint-config (file)
  "The project's yamllint configuration above FILE, or nil."
  (when-let* ((dir (and file (locate-dominating-file
                              file (lambda (d) (cl-some (lambda (n) (file-exists-p (expand-file-name n d)))
                                                        '(".yamllint" ".yamllint.yaml" ".yamllint.yml")))))))
    (cl-loop for n in '(".yamllint" ".yamllint.yaml" ".yamllint.yml")
             when (file-exists-p (expand-file-name n dir)) return (expand-file-name n dir))))

(defconst ide-k8s--yamllint-default
  (concat "{extends: default, rules: {line-length: disable, document-start: disable, "
          "truthy: {check-keys: false}, comments: {min-spaces-from-content: 1}, "
          "comments-indentation: disable, braces: {max-spaces-inside: 1}, "
          "brackets: {max-spaces-inside: 1}, "
          "indentation: {spaces: 2, indent-sequences: consistent}}}")
  "Relaxed yamllint rules used when the project has no configuration.")

(defun ide-k8s--yamllint-args (file)
  "yamllint arguments for the buffer visiting FILE (read on stdin)."
  (append '("-f" "parsable")
          (if-let* ((cfg (ide-k8s--yamllint-config file)))
              (list "-c" cfg)
            (list "-d" ide-k8s--yamllint-default))
          '("-")))

(ide-lang-define-linter
 'yamllint :safe t :modes '(yaml-ts-mode k8s-mode)
 :program (lambda () (executable-find "yamllint"))
 :args #'ide-k8s--yamllint-args
 :parse (ide-lang-parse-regexp
         "yamllint" "^[^:\n]*:\\([0-9]+\\):\\([0-9]+\\): \\[\\(error\\|warning\\)\\] \\(.*\\)$"
         :line 1 :col 2 :severity 3 :message 4))

(defun ide-k8s--parse-actionlint (output buffer)
  "Flymake diagnostics from actionlint's JSON OUTPUT for BUFFER."
  (let ((errors (json-parse-string (if (string-empty-p (string-trim output)) "[]" output)
                                   :object-type 'alist :array-type 'list :null-object nil)))
    (mapcar (lambda (e)
              (ide-lang-diag buffer "actionlint" (alist-get 'line e) (alist-get 'column e) :error
                             (format "%s [%s]" (alist-get 'message e) (alist-get 'kind e))
                             (and (alist-get 'end_column e) (1+ (alist-get 'end_column e)))))
            errors)))

(ide-lang-define-linter
 'actionlint :safe t :modes '(yaml-ts-mode k8s-mode)
 :program (lambda () (and (eq (ide-k8s-file-kind) 'gh-workflow) (executable-find "actionlint")))
 :args '("-no-color" "-format" "{{json .}}" "-")
 :parse #'ide-k8s--parse-actionlint)

(defun ide-k8s--pointer-segments (pointer)
  "Split the JSON pointer POINTER (\"/spec/ports/0\") into a list of segments."
  (split-string pointer "/" t))

(defun ide-k8s--parse-kubeconform (output buffer)
  "Flymake diagnostics from kubeconform's JSON OUTPUT for BUFFER.
kubeconform reports a JSON pointer, not a line; it is mapped back to the document
with the same kind and name."
  (let* ((json (json-parse-string output :object-type 'alist :array-type 'list :null-object nil))
         (docs (with-current-buffer buffer (ide-k8s--documents)))
         diags)
    (dolist (r (alist-get 'resources json))
      (when (member (alist-get 'status r) '("statusInvalid" "statusError"))
        (let* ((doc (or (cl-find-if (lambda (d) (and (equal (plist-get d :kind) (alist-get 'kind r))
                                                     (equal (plist-get d :name) (alist-get 'name r))
                                                     (equal (plist-get d :api) (alist-get 'version r))))
                                    docs)
                        (cl-find-if (lambda (d) (equal (plist-get d :kind) (alist-get 'kind r))) docs)))
               (errors (or (alist-get 'validationErrors r)
                           (list `((path . "") (msg . ,(alist-get 'msg r)))))))
          (dolist (e errors)
            (let* ((line (if doc
                             (with-current-buffer buffer
                               (save-excursion
                                 (save-restriction
                                   (widen)
                                   (line-number-at-pos
                                    (car (ide-k8s--locate (plist-get doc :beg) (plist-get doc :end)
                                                          (ide-k8s--pointer-segments (alist-get 'path e))))))))
                           1))
                   (path (alist-get 'path e)))
              (push (ide-lang-diag buffer "kubeconform" line 1 :error
                                   (if (string-empty-p path) (alist-get 'msg e)
                                     (format "%s: %s" (string-replace "/" "." (string-trim-left path "/"))
                                             (alist-get 'msg e))))
                    diags))))))
    (nreverse diags)))

(defun ide-k8s--kubeconform-common ()
  "kubeconform options shared by the Flymake backend and the render-then-validate command."
  (append '("-strict" "-ignore-missing-schemas" "-cache")
          (list (ide-k8s--cache-dir "kubeconform"))
          (when ide-k8s-kubernetes-version (list "-kubernetes-version" ide-k8s-kubernetes-version))
          (when ide-k8s-crd-catalog
            (list "-schema-location" "default" "-schema-location" ide-k8s-crd-catalog))))

(defun ide-k8s--kubeconform-args (&optional _file)
  "kubeconform arguments for a manifest on stdin, with JSON output."
  (append '("-output" "json") (ide-k8s--kubeconform-common) '("-")))

(ide-lang-define-linter
 'kubeconform :safe t :modes '(yaml-ts-mode k8s-mode)
 :program (lambda () (and (eq (ide-k8s-file-kind) 'k8s-manifest) (executable-find "kubeconform")))
 :args #'ide-k8s--kubeconform-args
 :parse #'ide-k8s--parse-kubeconform)

;;;; Process helpers -------------------------------------------------------------------------------

(defun ide-k8s--call (argv callback &optional dir)
  "Run ARGV asynchronously in DIR; call CALLBACK with exit code, stdout and stderr."
  (let* ((default-directory (or dir (ide-lang-root)))
         (out (generate-new-buffer " *k8s-out*"))
         (err (generate-new-buffer " *k8s-err*")))
    (ide-k8s--with-env
      (make-process
       :name (car argv) :buffer out :stderr err :command argv :noquery t
       :connection-type 'pipe
       :sentinel (lambda (p _)
                   (when (memq (process-status p) '(exit signal))
                     (let ((code (process-exit-status p))
                           (so (with-current-buffer out (buffer-string)))
                           (se (with-current-buffer err (buffer-string))))
                       (kill-buffer out)
                       (kill-buffer err)
                       (funcall callback code so se))))))))

(defun ide-k8s--show-text (name text &optional mode)
  "Show TEXT in a read-only buffer NAME, in MODE (default `special-mode')."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (when mode (funcall mode))
        (unless mode (special-mode))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (setq buffer-read-only t))
    (pop-to-buffer buf)
    buf))

(defun ide-k8s--show-report (name entries)
  "List ENTRIES, each (FILE LINE SEVERITY MESSAGE), in a compilation-style buffer NAME.
Visiting a line with RET or `next-error' jumps to the file."
  (let ((buf (get-buffer-create (format "*run: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (compilation-mode)
        (erase-buffer)
        (insert (format "-*- %s: %d finding%s -*-\n" name (length entries) (if (= 1 (length entries)) "" "s")))
        (dolist (e entries)
          (insert (format "%s:%d: %s: %s\n" (nth 0 e) (nth 1 e) (nth 2 e) (nth 3 e))))
        (goto-char (point-min))
        (setq default-directory (ide-lang-root))))
    (pop-to-buffer buf)
    buf))

(defun ide-k8s--file-or-error ()
  "The file of this buffer, saved, or a `user-error'."
  (unless buffer-file-name (user-error "This buffer does not visit a file"))
  (when (buffer-modified-p) (save-buffer))
  buffer-file-name)

;;;; Helm commands -----------------------------------------------------------------------------------

(defvar ide-helm--values-selection (make-hash-table :test #'equal)
  "Chart root -> list of values files chosen for template, lint and install.")

(defun ide-helm--chart-or-error ()
  "The root of the chart of this buffer, or a `user-error'."
  (or (ide-helm-chart-root) (user-error "Not inside a Helm chart (no Chart.yaml above this file)")))

(defun ide-helm--chart-name (root)
  "The chart name from ROOT/Chart.yaml (the directory name when unreadable)."
  (let ((f (expand-file-name "Chart.yaml" root)))
    (or (when (file-readable-p f)
          (with-temp-buffer
            (insert-file-contents f)
            (when (re-search-forward "^name:[ \t]*\\([^ \t\n#]+\\)" nil t)
              (string-trim (match-string 1) "[\"']" "[\"']"))))
        (file-name-nondirectory (directory-file-name root)))))

(defun ide-helm--values-candidates (root)
  "Values files of the chart at ROOT, relative to it."
  (cl-remove-duplicates
   (append (mapcar (lambda (f) (file-relative-name f root))
                   (directory-files root t "\\`values.*\\.ya?ml\\'"))
           (when (file-directory-p (expand-file-name "ci" root))
             (mapcar (lambda (f) (file-relative-name f root))
                     (directory-files (expand-file-name "ci" root) t "\\.ya?ml\\'"))))
   :test #'equal))

(defun ide-helm-select-values ()
  "Choose the values files (on top of values.yaml) that template, lint and install use."
  (interactive)
  (let* ((root (ide-helm--chart-or-error))
         (chosen (completing-read-multiple
                  "Values files (empty for none): "
                  (ide-helm--values-candidates root) nil nil
                  (string-join (gethash root ide-helm--values-selection) ","))))
    (puthash root chosen ide-helm--values-selection)
    (message "Helm values: %s" (if chosen (string-join chosen ", ") "chart defaults only"))))

(defun ide-helm--values-args (root)
  "The -f arguments for the chosen values files of the chart at ROOT."
  (cl-loop for f in (gethash root ide-helm--values-selection)
           append (list "-f" (expand-file-name f root))))

(defun ide-helm--cluster-flags ()
  "helm's --kube-context and --namespace for the context and namespace chosen here."
  (append (when ide-k8s-context (list "--kube-context" ide-k8s-context))
          (when ide-k8s-namespace (list "--namespace" ide-k8s-namespace))))

(defun ide-helm-command (verb root &rest extra)
  "The helm command line (a list) for VERB on the chart at ROOT, with the chosen values.
VERB is one of lint, template, install, package, dependency-update; EXTRA is
appended.  The release name is the chart name."
  (let ((release (ide-helm--chart-name root))
        (values (ide-helm--values-args root)))
    (append
     (pcase verb
       ('lint (append (list "helm" "lint" root) values))
       ('template (append (list "helm" "template" release root) values))
       ('install (append (list "helm" "install" release root "--dry-run=client" "--debug") values
                         (ide-helm--cluster-flags)))
       ('upgrade (append (list "helm" "upgrade" "--install" release root) values (ide-helm--cluster-flags)))
       ('uninstall (append (list "helm" "uninstall" release) (ide-helm--cluster-flags)))
       ('dependency-update (list "helm" "dependency" "update" root))
       ('package (list "helm" "package" root "--destination"
                       (expand-file-name "dist" (ide-lang-root root))))
       (_ (error "Unknown helm verb %s" verb)))
     (unless (memq verb '(dependency-update package uninstall)) ide-helm-extra-args)
     extra)))

(defun ide-helm--run (name verb &rest extra)
  "Run the helm VERB for the current chart in the drawer."
  (let ((root (ide-helm--chart-or-error)))
    (ide-k8s--with-env
      (ide-lang-run name (apply #'ide-helm-command verb root extra) root))))

(defun ide-helm-lint (&optional strict)
  "helm lint the current chart; with prefix argument STRICT, add --strict."
  (interactive "P")
  (apply #'ide-helm--run "helm lint" 'lint (when strict '("--strict"))))

(defun ide-helm-dependency-update ()
  "helm dependency update for the current chart."
  (interactive)
  (ide-helm--run "helm dependency" 'dependency-update))

(defun ide-helm-package ()
  "helm package the current chart into dist/ under the project root."
  (interactive)
  (ide-helm--run "helm package" 'package))

(defun ide-helm-install-dry-run ()
  "helm install --dry-run --debug the current chart (nothing reaches a cluster)."
  (interactive)
  (ide-helm--run "helm install" 'install))

(defun ide-helm-upgrade-install ()
  "helm upgrade --install the current chart into the chosen cluster (asks first)."
  (interactive)
  (when (yes-or-no-p (format "helm upgrade --install %s on context %s? "
                             (ide-helm--chart-name (ide-helm--chart-or-error))
                             (or ide-k8s-context "current")))
    (ide-helm--run "helm upgrade" 'upgrade)))

(defun ide-helm-uninstall ()
  "helm uninstall the release of the current chart (asks first)."
  (interactive)
  (when (yes-or-no-p (format "helm uninstall %s on context %s? "
                             (ide-helm--chart-name (ide-helm--chart-or-error))
                             (or ide-k8s-context "current")))
    (ide-helm--run "helm uninstall" 'uninstall)))

(defun ide-helm-template (&optional only-this)
  "Render the current chart into a YAML buffer; with ONLY-THIS render just this template."
  (interactive "P")
  (let* ((root (ide-helm--chart-or-error))
         (rel (and only-this buffer-file-name (file-relative-name buffer-file-name root)))
         (argv (apply #'ide-helm-command 'template root (when rel (list "--show-only" rel))))
         (name (format "*helm template: %s%s*" (ide-helm--chart-name root) (if rel (concat " " rel) ""))))
    (when (and buffer-file-name (buffer-modified-p)) (save-buffer))
    (ide-k8s--call
     argv
     (lambda (code out err)
       (if (zerop code)
           (ide-k8s--show-text name out #'yaml-ts-mode)
         (ide-k8s--show-text "*helm template error*" err)))
     root)))

(defun ide-helm-template-and-validate ()
  "Render the chart with helm template and validate the result with kubeconform."
  (interactive)
  (let ((root (ide-helm--chart-or-error)))
    (ide-k8s--with-env
      (ide-lang-run "helm template | kubeconform"
                    (concat (mapconcat #'shell-quote-argument (ide-helm-command 'template root) " ")
                            " | "
                            (mapconcat #'shell-quote-argument
                                       (append '("kubeconform") (ide-k8s--kubeconform-common) '("-summary" "-"))
                                       " "))
                    root))))

;;;; Jumping between templates and values ----------------------------------------------------------------

(defun ide-helm--values-ref-at-point ()
  "The key path of the `.Values.a.b' reference at point, as a list of strings, or nil.
When point is on a component, the path stops there (`.Values.a' on `a')."
  (save-excursion
    (let ((pt (point)) result)
      (goto-char (line-beginning-position))
      (while (and (not result) (re-search-forward "\\.Values\\(\\(?:\\.[A-Za-z0-9_-]+\\)+\\)" (line-end-position) t))
        (when (and (>= pt (match-beginning 0)) (<= pt (match-end 0)))
          (let* ((start (match-beginning 1))
                 (full (split-string (match-string-no-properties 1) "\\." t))
                 (n 0) (pos start))
            ;; count components whose start is at or before point
            (dolist (c full)
              (when (<= (1+ pos) pt) (setq n (1+ n)))
              (setq pos (+ pos 1 (length c))))
            (setq result (cl-subseq full 0 (max 1 n))))))
      result)))

(defun ide-helm-goto-values ()
  "Jump from the `.Values.x.y' reference at point to its key in values.yaml.
Use \\[xref-go-back] to return."
  (interactive)
  (let* ((path (or (ide-helm--values-ref-at-point) (user-error "No .Values reference at point")))
         (root (ide-helm--chart-or-error))
         (file (expand-file-name "values.yaml" root)))
    (unless (file-exists-p file) (user-error "No values.yaml in %s" root))
    (xref-push-marker-stack)
    (find-file file)
    (let ((r (ide-k8s--locate (point-min) (point-max) path)))
      (goto-char (car r))
      (back-to-indentation)
      (unless (cdr r)
        (message "values.yaml has no key %s (nearest parent shown)" (string-join path "."))))))

(defun ide-helm-values-usages ()
  "List the templates that use the values key at point in values.yaml."
  (interactive)
  (let* ((root (ide-helm--chart-or-error))
         (path (or (ide-k8s--key-path-indent) (user-error "No key at point")))
         (needle (concat ".Values." (string-join path "."))))
    (ide-lang-run (concat "usages of " needle)
                  (list "grep" "-rnF" "--include=*.yaml" "--include=*.yml" "--include=*.tpl" "--include=*.txt"
                        "-e" needle "templates")
                  root)))

(defun ide-helm-open-values ()
  "Open values.yaml of the current chart."
  (interactive)
  (find-file (expand-file-name "values.yaml" (ide-helm--chart-or-error))))

(defun ide-helm-open-chart-yaml ()
  "Open Chart.yaml of the current chart."
  (interactive)
  (find-file (expand-file-name "Chart.yaml" (ide-helm--chart-or-error))))

;;;; kubectl --------------------------------------------------------------------------------------------------

(defun ide-k8s--kubectl-flags ()
  "The --context and --namespace arguments chosen for this module."
  (append (when ide-k8s-context (list "--context" ide-k8s-context))
          (when ide-k8s-namespace (list "--namespace" ide-k8s-namespace))))

(defun ide-k8s-kubectl-command (verb file &rest extra)
  "kubectl command line (a list) for VERB (apply-client, apply-server, diff) on FILE.
FILE is `-' when the manifests arrive on stdin."
  (append '("kubectl")
          (pcase verb
            ('apply-client (list "apply" "--dry-run=client" "-f" file))
            ('apply-server (list "apply" "--dry-run=server" "-f" file))
            ('diff (list "diff" "-f" file))
            (_ (error "Unknown kubectl verb %s" verb)))
          (ide-k8s--kubectl-flags)
          extra))

(defun ide-k8s--manifest-source (verb)
  "Shell command for kubectl VERB on the current file; a chart template is rendered first."
  (let ((chart (and (memq (ide-k8s-file-kind) '(helm-template helm-chart)) (ide-helm-chart-root))))
    (if chart
        (concat (mapconcat #'shell-quote-argument (ide-helm-command 'template chart) " ") " | "
                (mapconcat #'shell-quote-argument (ide-k8s-kubectl-command verb "-") " "))
      (mapconcat #'shell-quote-argument (ide-k8s-kubectl-command verb (ide-k8s--file-or-error)) " "))))

(defun ide-k8s--kubectl (name verb)
  (ide-k8s--with-env (ide-lang-run name (ide-k8s--manifest-source verb))))

(defun ide-k8s-apply-dry-run-client ()
  "kubectl apply --dry-run=client for this file (a chart is rendered first)."
  (interactive) (ide-k8s--kubectl "kubectl apply client" 'apply-client))

(defun ide-k8s-apply-dry-run-server ()
  "kubectl apply --dry-run=server for this file (needs a reachable cluster)."
  (interactive) (ide-k8s--kubectl "kubectl apply server" 'apply-server))

(defun ide-k8s-diff ()
  "kubectl diff for this file against the cluster (needs a reachable cluster)."
  (interactive) (ide-k8s--kubectl "kubectl diff" 'diff))

(defun ide-k8s-set-context (context)
  "Choose the kubectl CONTEXT used by this module."
  (interactive
   (list (completing-read "kubectl context: "
                          (ide-k8s--with-env (process-lines "kubectl" "config" "get-contexts" "-o" "name")) nil t)))
  (setq ide-k8s-context context)
  (message "kubectl context: %s" context))

(defun ide-k8s-set-namespace (namespace)
  "Set the kubectl NAMESPACE used by this module (empty for the context default)."
  (interactive "sNamespace (empty for default): ")
  (setq ide-k8s-namespace (unless (string-empty-p namespace) namespace))
  (message "kubectl namespace: %s" (or ide-k8s-namespace "default")))

;;;; kubectl explain ---------------------------------------------------------------------------------------------

(defun ide-k8s-explain-target ()
  "The `kubectl explain' target at point: (RESOURCE.PATH . API-VERSION), or nil.
RESOURCE is the kind of the document around point; PATH is the dotted field path
derived from the YAML structure."
  (let* ((b (ide-k8s--doc-bounds))
         (kind (ide-k8s--field (car b) (cdr b) '("kind")))
         (api (ide-k8s--field (car b) (cdr b) '("apiVersion"))))
    (when kind
      (cons (string-join (cons kind (ide-k8s-key-path)) ".") api))))

(defun ide-k8s-explain-command (target &optional recursive)
  "The kubectl explain command line for TARGET, a (RESOURCE.PATH . API-VERSION) cons."
  (append (list "kubectl" "explain" (car target))
          (when (cdr target) (list (concat "--api-version=" (cdr target))))
          (when recursive '("--recursive"))
          (when ide-k8s-context (list "--context" ide-k8s-context))
          '("--request-timeout=15s")))

(defun ide-k8s-explain (&optional recursive)
  "Show `kubectl explain' for the field at point in a help buffer.
With prefix argument RECURSIVE, print the whole subtree.  Needs the cluster's API."
  (interactive "P")
  (let ((target (or (ide-k8s-explain-target) (user-error "No Kubernetes resource (kind:) around point"))))
    (message "kubectl explain %s ..." (car target))
    (ide-k8s--call
     (ide-k8s-explain-command target recursive)
     (lambda (code out err)
       (if (zerop code)
           (ide-k8s--show-text (format "*kubectl explain: %s*" (car target)) out)
         (message "kubectl explain %s failed: %s" (car target)
                  (car (split-string (string-trim err) "\n"))))))))

;;;; kustomize, kube-linter, trivy, actionlint ----------------------------------------------------------------------

(defun ide-k8s-kustomize ()
  "Render the kustomization of this directory (kubectl kustomize) into a YAML buffer."
  (interactive)
  (let ((dir (or (locate-dominating-file default-directory "kustomization.yaml")
                 (locate-dominating-file default-directory "kustomization.yml")
                 (user-error "No kustomization.yaml above this file"))))
    (ide-k8s--call (list "kubectl" "kustomize" (expand-file-name dir))
                   (lambda (code out err)
                     (if (zerop code)
                         (ide-k8s--show-text (format "*kustomize: %s*" (abbreviate-file-name dir)) out #'yaml-ts-mode)
                       (ide-k8s--show-text "*kustomize error*" err))))))

(defun ide-k8s--object-line (file kind name)
  "Line in FILE of the resource KIND/NAME (the kind line), falling back to the first KIND."
  (or (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (let ((docs (ide-k8s--documents)))
            (plist-get (or (cl-find-if (lambda (d) (and (equal (plist-get d :kind) kind)
                                                        (equal (plist-get d :name) name)))
                                       docs)
                           (cl-find-if (lambda (d) (equal (plist-get d :kind) kind)) docs))
                       :line))))
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (when (re-search-forward (concat "^kind:[ \t]*" (regexp-quote kind)) nil t)
            (line-number-at-pos (match-beginning 0)))))
      1))

(defun ide-k8s--parse-kube-linter (output)
  "Report entries (FILE LINE SEVERITY MESSAGE) from kube-linter's JSON OUTPUT."
  (let ((json (json-parse-string output :object-type 'alist :array-type 'list :null-object nil)))
    (mapcar
     (lambda (r)
       (let* ((obj (alist-get 'K8sObject (alist-get 'Object r)))
              (file (alist-get 'FilePath (alist-get 'Metadata (alist-get 'Object r))))
              (kind (alist-get 'Kind (alist-get 'GroupVersionKind obj)))
              (name (alist-get 'Name obj)))
         (list file (ide-k8s--object-line file kind name) "warning"
               (format "[%s] %s (%s/%s)  Fix: %s" (alist-get 'Check r)
                       (alist-get 'Message (alist-get 'Diagnostic r)) kind name (alist-get 'Remediation r)))))
     (alist-get 'Reports json))))

(defun ide-k8s--lint-target ()
  "What the on-demand scanners check: the chart directory, else this file."
  (if-let* ((root (and (memq (ide-k8s-file-kind) '(helm-template helm-chart)) (ide-helm-chart-root))))
      (directory-file-name root)
    (ide-k8s--file-or-error)))

(defun ide-k8s-kube-linter ()
  "Run kube-linter on this manifest (or chart) and list the findings."
  (interactive)
  (let ((target (ide-k8s--lint-target)))
    (ide-lang-exe-or-error "kube-linter")
    (ide-k8s--call
     (list "kube-linter" "lint" "--format" "json" target)
     (lambda (_code out err)
       (condition-case e
           (let ((entries (ide-k8s--parse-kube-linter out)))
             (if entries (ide-k8s--show-report "kube-linter" entries)
               (message "kube-linter: no findings")))
         (error (ide-k8s--show-text "*kube-linter error*" (format "%s\n%s\n%s" (error-message-string e) err out))))))))

(defun ide-k8s--map-rendered-line (file content line)
  "Line of the template FILE that rendered to CONTENT near rendered LINE, or nil.
Each template line becomes a pattern (every {{ ... }} matches anything); among the
lines that match, the one closest to LINE wins."
  (when (and content (file-readable-p file))
    (let ((want (string-trim content)) (n 0) best)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (l (split-string (buffer-string) "\n"))
          (setq n (1+ n))
          (let* ((l (string-trim l))
                 (parts (split-string l "{{[^}]*}}"))
                 (literal (cl-some (lambda (x) (not (string-empty-p (string-trim x)))) parts)))
            (when (and (not (string-empty-p l)) literal
                       (string-match-p (concat "\\`" (mapconcat #'regexp-quote parts ".*") "\\'") want)
                       (or (null best) (< (abs (- n line)) (abs (- best line)))))
              (setq best n)))))
      best)))

(defun ide-k8s--parse-trivy (output &optional chart-dir)
  "Report entries (FILE LINE SEVERITY MESSAGE) from `trivy config -f json' OUTPUT.
For a chart (CHART-DIR) trivy reports lines of the rendered manifest: they are mapped
back to the template source when a matching line is found, and labelled as rendered
lines otherwise."
  (let ((json (json-parse-string output :object-type 'alist :array-type 'list :null-object nil)))
    (cl-loop for res in (alist-get 'Results json)
             append (cl-loop
                     for m in (alist-get 'Misconfigurations res)
                     collect
                     (let* ((cause (alist-get 'CauseMetadata m))
                            (line (or (alist-get 'StartLine cause) 1))
                            (target (alist-get 'Target res))
                            (file (if chart-dir (expand-file-name target chart-dir) target))
                            (note ""))
                       (when chart-dir
                         (let* ((lines (alist-get 'Lines (alist-get 'Code cause)))
                                (first (cl-find-if (lambda (l) (alist-get 'IsCause l)) lines))
                                (mapped (ide-k8s--map-rendered-line file (alist-get 'Content first) line)))
                           (setq note (if mapped (format " (rendered line %d)" line)
                                        (format " (line %d of the rendered output)" line))
                                 line (or mapped line))))
                       (list file line
                             (pcase (alist-get 'Severity m)
                               ((or "CRITICAL" "HIGH") "error")
                               ("MEDIUM" "warning") (_ "note"))
                             (format "[%s] %s: %s%s" (alist-get 'ID m) (alist-get 'Title m)
                                     (alist-get 'Message m) note)))))))

(defun ide-k8s-trivy-command (target)
  "The trivy config scan command line for TARGET."
  (list "trivy" "config" "-f" "json" "--quiet" "--cache-dir" (ide-k8s--cache-dir "trivy") target))

(defun ide-k8s-trivy ()
  "Scan this file (or chart) for misconfigurations with trivy and list the findings."
  (interactive)
  (let ((target (ide-k8s--lint-target)))
    (ide-lang-exe-or-error "trivy")
    (message "trivy config %s ..." (file-name-nondirectory target))
    (ide-k8s--call
     (ide-k8s-trivy-command target)
     (lambda (_code out err)
       (condition-case e
           (let ((entries (ide-k8s--parse-trivy out (and (file-directory-p target) target))))
             (if entries (ide-k8s--show-report "trivy config" entries)
               (message "trivy: no misconfigurations found")))
         (error (ide-k8s--show-text "*trivy error*" (format "%s\n%s" (error-message-string e) err)))))
     (ide-lang-root))))

(defun ide-k8s-actionlint-project ()
  "Run actionlint on every workflow of the project."
  (interactive)
  (let* ((root (ide-lang-root))
         (files (directory-files (expand-file-name ".github/workflows" root) nil "\\.ya?ml\\'")))
    (unless files (user-error "No workflows in .github/workflows"))
    (ide-lang-exe-or-error "actionlint")
    (ide-lang-run "actionlint"
                  (append '("actionlint" "-no-color" "-oneline")
                          (mapcar (lambda (f) (concat ".github/workflows/" f)) files))
                  root)))

;;;; Schemas and docs --------------------------------------------------------------------------------------------------

(defun ide-k8s-insert-schema-comment ()
  "Insert a yaml-language-server schema comment for this document's custom resource."
  (interactive)
  (let* ((b (ide-k8s--doc-bounds))
         (kind (ide-k8s--field (car b) (cdr b) '("kind")))
         (api (ide-k8s--field (car b) (cdr b) '("apiVersion")))
         (url (and kind api (ide-k8s-crd-schema-url api kind))))
    (unless url (user-error "Not a custom resource with a group/version"))
    (save-excursion
      (goto-char (car b))
      (when (looking-at "---") (forward-line 1))
      (insert "# yaml-language-server: $schema=" url "\n"))))

(defun ide-k8s-docs ()
  "Open the documentation that fits this file in the in-Emacs browser."
  (interactive)
  (ide-lang-browse
   (pcase (ide-k8s-file-kind)
     ((or 'helm-template 'helm-chart) "https://helm.sh/docs/chart_template_guide/")
     ('gh-workflow "https://docs.github.com/en/actions/reference/workflows-and-actions/workflow-syntax")
     ('azure-pipeline "https://learn.microsoft.com/en-us/azure/devops/pipelines/yaml-schema/")
     ('compose "https://docs.docker.com/reference/compose-file/")
     ('kustomization "https://kubectl.docs.kubernetes.io/references/kustomize/kustomization/")
     (_ (let ((kind (ignore-errors (let ((b (ide-k8s--doc-bounds))) (ide-k8s--field (car b) (cdr b) '("kind"))))))
          (if kind (format "https://kubernetes.io/search/?q=%s" kind)
            "https://kubernetes.io/docs/reference/kubernetes-api/"))))))

(defun ide-k8s-kubel ()
  "Open kubel, the cluster browser (pods, logs, exec), for the chosen context."
  (interactive)
  (require 'kubel)
  (when ide-k8s-context (setq kubel-context ide-k8s-context))
  (kubel))

;;;; Run / debug hooks and menu ---------------------------------------------------------------------------------------------

(defun ide-k8s--yaml-buffer-p ()
  (derived-mode-p 'yaml-ts-mode 'yaml-mode 'ide-helm-mode 'ide-helm-text-mode))

(defun ide-k8s--run-command ()
  "S-<f10> for DevOps YAML: render a chart, dry-run a manifest, lint a workflow."
  (when (ide-k8s--yaml-buffer-p)
    (pcase (ide-k8s-file-kind)
      ((or 'helm-template 'helm-chart)
       (when-let* ((root (ide-helm-chart-root)))
         (mapconcat #'shell-quote-argument (ide-helm-command 'template root) " ")))
      ('k8s-manifest (mapconcat #'shell-quote-argument
                                (ide-k8s-kubectl-command 'apply-client buffer-file-name) " "))
      ('kustomization "kubectl kustomize .")
      ('gh-workflow (mapconcat #'shell-quote-argument (list "actionlint" "-oneline" buffer-file-name) " ")))))

(defun ide-k8s--debug ()
  "S-<f9> in a chart: helm install --dry-run --debug, which prints the computed values."
  (when (and (ide-k8s--yaml-buffer-p) (memq (ide-k8s-file-kind) '(helm-template helm-chart)))
    (ide-helm-install-dry-run)
    t))

(add-hook 'ide-run-command-functions #'ide-k8s--run-command)
(add-hook 'ide-debug-functions #'ide-k8s--debug)

(defun ide-k8s--in-chart-p () (and (ide-k8s--yaml-buffer-p) (ide-helm-chart-root) t))
(defun ide-k8s--is-gh-p () (eq (ide-k8s-file-kind) 'gh-workflow))

;;;###autoload (autoload 'ide-k8s-dispatch "lang-k8s" nil t)
(transient-define-prefix ide-k8s-dispatch ()
  "Helm, Kubernetes and CI YAML: render, validate, explore."
  [:if ide-k8s--in-chart-p
   ["Helm"
    ("l" "lint" ide-helm-lint)
    ("t" "template (c-u: this file)" ide-helm-template)
    ("i" "install --dry-run --debug" ide-helm-install-dry-run)
    ("u" "dependency update" ide-helm-dependency-update)
    ("p" "package" ide-helm-package)
    ("I" "upgrade --install" ide-helm-upgrade-install)
    ("X" "uninstall" ide-helm-uninstall)
    ("v" ide-helm-select-values
     :description (lambda () (format "values files (%s)"
                                     (let ((r (ide-helm-chart-root)))
                                       (if-let* ((c (and r (gethash r ide-helm--values-selection))))
                                           (string-join c ",") "defaults")))))
    ("r" "render + validate (kubeconform)" ide-helm-template-and-validate)]
   ["Navigate"
    ("j" "to .Values key" ide-helm-goto-values)
    ("J" "usages of key" ide-helm-values-usages)
    ("o" "values.yaml" ide-helm-open-values)
    ("O" "Chart.yaml" ide-helm-open-chart-yaml)]]
  [["kubectl"
    ("a" "apply --dry-run=client" ide-k8s-apply-dry-run-client)
    ("A" "apply --dry-run=server" ide-k8s-apply-dry-run-server)
    ("d" "diff" ide-k8s-diff)
    ("e" "explain field at point" ide-k8s-explain)
    ("k" "kustomize build" ide-k8s-kustomize)
    ("c" ide-k8s-set-context :description (lambda () (format "context (%s)" (or ide-k8s-context "current"))))
    ("n" ide-k8s-set-namespace :description (lambda () (format "namespace (%s)" (or ide-k8s-namespace "default"))))
    ("K" "kubel (cluster browser)" ide-k8s-kubel)]
   ["Check"
    ("L" "kube-linter" ide-k8s-kube-linter)
    ("s" "trivy config scan" ide-k8s-trivy)
    ("G" "actionlint on project" ide-k8s-actionlint-project)
    ("S" "CRD schema comment" ide-k8s-insert-schema-comment)]
   ["Server / docs"
    ("D" "docs" ide-k8s-docs)
    ("R" "reconnect server" eglot-reconnect)]])

(ide-lang-register-menu '(ide-helm-mode ide-helm-text-mode yaml-ts-mode yaml-mode) #'ide-k8s-dispatch)

(provide 'lang-k8s)
;;; lang-k8s.el ends here
