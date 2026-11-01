;;; lang-rust-test.el --- Tests for modules/11-languages/lang-rust.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --init-directory . --batch -l init.el -l tests/lang-rust-test.el -f ert-run-tests-batch-and-exit
;; Pure logic only (no server); the rust-analyzer round trip was checked by hand.

;;; Code:

(require 'ert)

(ert-deftest rust-runnable-becomes-a-cargo-command ()
  (let ((r '(:label "test a::b" :kind "cargo"
             :args (:cwd "/p" :cargoArgs ["test" "--package" "x" "--lib"]
                    :executableArgs ["a::b" "--exact"]))))
    (should (equal (ide-rust--runnable-command r)
                   '(("cargo" "test" "--package" "x" "--lib" "--" "a::b" "--exact") . "/p")))))

(ert-deftest rust-debug-builds-turn-run-and-test-into-builds ()
  (should (equal (ide-rust--debug-build-args '("run" "--bin" "x")) '("build" "--bin" "x")))
  (should (equal (ide-rust--debug-build-args '("test" "-p" "x")) '("test" "--no-run" "-p" "x"))))

(ert-deftest rust-artifact-is-the-last-executable ()
  (let ((out (concat "{\"reason\":\"compiler-artifact\",\"executable\":null}\n"
                     "{\"reason\":\"compiler-message\"}\n"
                     "{\"reason\":\"compiler-artifact\",\"executable\":\"/t/debug/x\"}\n"
                     "{\"reason\":\"build-finished\",\"success\":true}\n")))
    (should (equal (ide-rust--artifact out) "/t/debug/x"))
    (should-not (ide-rust--artifact "{\"reason\":\"build-finished\"}\n"))))

(ert-deftest rust-file-test-filter ()
  (progn
    (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) "/p/")))
      (let ((buffer-file-name "/p/tests/api.rs"))
        (should (equal (ide-rust--file-test-filter) '("--test" "api"))))
      (let ((buffer-file-name "/p/src/net/client.rs"))
        (should (equal (ide-rust--file-test-filter) '("net::client"))))
      (let ((buffer-file-name "/p/src/net/mod.rs"))
        (should (equal (ide-rust--file-test-filter) '("net"))))
      (let ((buffer-file-name "/p/src/lib.rs"))
        (should-not (ide-rust--file-test-filter))))))

(ert-deftest rust-test-name-qualified-by-modules ()
  (skip-unless (treesit-language-available-p 'rust))
  (with-temp-buffer
    (insert "mod a {\n  mod b {\n    #[test]\n    fn it_works() {\n      assert!(true);\n    }\n  }\n}\n")
    (rust-ts-mode)
    (goto-char (point-min)) (search-forward "assert")
    (should (equal (ide-rust--test-name-at-point) "a::b::it_works"))))

(ert-deftest rust-test-runner-nextest-falls-back-to-cargo ()
  (let ((ide-rust-test-runner 'nextest))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
      (should (equal (ide-rust--test-command "x") '("cargo" "test" "x"))))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/cargo-nextest")))
      (should (equal (ide-rust--test-command "x") '("cargo" "nextest" "run" "x"))))))

(ert-deftest rust-file-test-filter-in-a-workspace ()
  ;; Paths are relative to the member crate, which is selected by its manifest.
  (let ((root (file-name-as-directory (make-temp-file "rust-ws" t))))
    (unwind-protect
        (progn
          (dolist (f '("Cargo.toml" "crates/foo/Cargo.toml" "crates/foo/tests/x.rs"
                       "crates/foo/src/net/client.rs" "crates/foo/src/main.rs"
                       "crates/bar/Cargo.toml" "crates/bar/src/lib.rs" "src/lib.rs"))
            (make-directory (file-name-directory (expand-file-name f root)) t)
            (write-region "" nil (expand-file-name f root) nil 'silent))
          (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) root))
                    ((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                    ((symbol-function 'executable-find) (lambda (&rest _) nil)))
            (let ((buffer-file-name (expand-file-name "crates/foo/tests/x.rs" root)))
              (should (equal (ide-rust--file-test-filter)
                             '("--manifest-path" "crates/foo/Cargo.toml" "--test" "x")))
              (should (equal (ide-rust--run-command)
                             "cargo test --manifest-path crates/foo/Cargo.toml --test x")))
            (let ((buffer-file-name (expand-file-name "crates/foo/src/net/client.rs" root)))
              (should (equal (ide-rust--file-test-filter)
                             '("--manifest-path" "crates/foo/Cargo.toml" "net::client")))
              (should (equal (ide-rust--run-command)
                             "cargo run --manifest-path crates/foo/Cargo.toml")))
            ;; a library member has nothing to run: cargo picks at the root
            (let ((buffer-file-name (expand-file-name "crates/bar/src/lib.rs" root)))
              (should (equal (ide-rust--file-test-filter)
                             '("--manifest-path" "crates/bar/Cargo.toml")))
              (should (equal (ide-rust--run-command) "cargo run")))
            ;; the root package needs no selection
            (let ((buffer-file-name (expand-file-name "src/lib.rs" root)))
              (should-not (ide-rust--file-test-filter)))))
      (delete-directory root t))))

(ert-deftest rust-debugger-installed-means-the-adapter-exists ()
  ;; The download directory exists before the download finishes (or after it fails).
  (require 'dap-codelldb)
  (let* ((dir (make-temp-file "codelldb" t))
         (dap-codelldb-debug-path dir)
         (dap-codelldb-debug-program (expand-file-name "extension/adapter/codelldb" dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'ide-rust--pick-runnable)
                   (lambda (&rest _) (error "Should not get past the install check"))))
          (should-error (ide-rust-debug) :type 'user-error))
      (delete-directory dir t))))

(defmacro lang-rust-test--with-crates (files &rest body)
  "Run BODY with `root' a temp project holding FILES (name or (NAME . CONTENT))."
  (declare (indent 1))
  `(let ((root (file-name-as-directory (make-temp-file "rust-ws" t))))
     (unwind-protect
         (progn
           (dolist (f ,files)
             (let ((name (if (consp f) (car f) f)))
               (make-directory (file-name-directory (expand-file-name name root)) t)
               (write-region (if (consp f) (cdr f) "") nil (expand-file-name name root) nil 'silent)))
           (cl-letf (((symbol-function 'ide-lang-root) (lambda (&optional _) root))
                     ((symbol-function 'executable-find) (lambda (&rest _) nil)))
             (with-temp-buffer
               (setq major-mode 'rust-ts-mode)
               ,@body)))
       (delete-directory root t))))

(ert-deftest rust-binaries-of-a-member-are-chosen-by-file ()
  (lang-rust-test--with-crates
      '("Cargo.toml" ("crates/app/Cargo.toml" . "[package]\nname = \"app\"\nversion = \"0.1.0\"\n")
        "crates/app/src/main.rs" "crates/app/src/bin/tool.rs" "crates/app/src/bin/big/main.rs"
        "crates/app/src/bin/big/cli.rs")
    (cl-flet ((at (f) (expand-file-name f root)))
      (let ((buffer-file-name (at "crates/app/src/bin/tool.rs")))
        (should (equal (ide-rust--run-command) "cargo run --manifest-path crates/app/Cargo.toml --bin tool"))
        ;; a bin's tests are scoped to it, with no module filter (bin::tool matched nothing)
        (should (equal (ide-rust--file-test-filter)
                       '("--manifest-path" "crates/app/Cargo.toml" "--bin" "tool"))))
      (let ((buffer-file-name (at "crates/app/src/bin/big/cli.rs")))
        (should (equal (ide-rust--file-target) '(bin "big" ("cli"))))
        (should (equal (ide-rust--run-command) "cargo run --manifest-path crates/app/Cargo.toml --bin big")))
      (let ((buffer-file-name (at "crates/app/src/main.rs")))
        (should (equal (ide-rust--run-command) "cargo run --manifest-path crates/app/Cargo.toml --bin app"))))))

(ert-deftest rust-test-name-includes-the-file-module ()
  ;; In src/util.rs the test is util::tests::it; `--exact' needs the whole path.
  (skip-unless (treesit-language-available-p 'rust))
  (lang-rust-test--with-crates
      '("Cargo.toml" "crates/lib1/Cargo.toml" "crates/lib1/src/lib.rs"
        ("crates/lib1/src/net/mod.rs" . "#[cfg(test)]\nmod tests {\n    #[test]\n    fn it() {}\n}\n")
        ("crates/lib1/src/util.rs" . "#[cfg(test)]\nmod tests {\n    #[test]\n    fn it() {}\n}\n"))
    (dolist (case '(("crates/lib1/src/util.rs" . "util::tests::it")
                    ("crates/lib1/src/net/mod.rs" . "net::tests::it")))
      (with-temp-buffer
        (insert-file-contents (expand-file-name (car case) root))
        (setq buffer-file-name (expand-file-name (car case) root))
        (cl-letf (((symbol-function 'eglot-ensure) #'ignore)) (rust-ts-mode))
        (goto-char (point-min)) (search-forward "fn it")
        (should (equal (ide-rust--test-name-at-point) (cdr case)))
        (set-buffer-modified-p nil)
        (setq buffer-file-name nil)))))

(ert-deftest rust-file-targets ()
  (lang-rust-test--with-crates '("Cargo.toml" "src/main.rs" "src/a/b.rs")
    (cl-flet ((target (f) (let ((buffer-file-name (expand-file-name f root))) (ide-rust--file-target))))
      (should (equal (target "src/a/b.rs") '(main nil ("a" "b"))))
      (should (equal (target "src/a/mod.rs") '(main nil ("a"))))
      (should (equal (target "tests/x.rs") '(test "x" nil)))
      ;; a subdirectory of tests/ is a target only with a main.rs; else a shared helper
      (should-not (target "tests/common/mod.rs"))
      (write-region "" nil (expand-file-name "tests/x/main.rs" (progn (make-directory (expand-file-name "tests/x" root) t) root)))
      (should (equal (target "tests/x/util.rs") '(test "x" ("util"))))
      (should (equal (target "examples/ex.rs") '(example "ex" nil)))
      (should (equal (target "benches/b.rs") '(bench "b" nil)))
      (should (equal (target "build.rs") '(build nil nil))))))

(ert-deftest rust-run-commands-per-target-kind ()
  (lang-rust-test--with-crates
      '("Cargo.toml" ("crates/app/Cargo.toml" . "[package]\nname = \"app\"\n")
        "crates/app/src/main.rs" "crates/app/src/lib.rs" "crates/app/src/bin/tool.rs"
        "crates/app/examples/ex.rs" "crates/app/benches/b.rs" "crates/app/build.rs"
        "crates/app/tests/common/mod.rs" "crates/app/tests/it.rs")
    (dolist (case '(("crates/app/tests/common/mod.rs" . "cargo test --manifest-path crates/app/Cargo.toml")
                    ("crates/app/tests/it.rs" . "cargo test --manifest-path crates/app/Cargo.toml --test it")
                    ("crates/app/examples/ex.rs" . "cargo run --manifest-path crates/app/Cargo.toml --example ex")
                    ("crates/app/benches/b.rs" . "cargo bench --manifest-path crates/app/Cargo.toml --bench b")
                    ("crates/app/build.rs" . "cargo build --manifest-path crates/app/Cargo.toml")
                    ("crates/app/src/lib.rs" . "cargo run --manifest-path crates/app/Cargo.toml --bin app")))
      (let ((buffer-file-name (expand-file-name (car case) root)))
        (should (equal (ide-rust--run-command) (cdr case)))))
    ;; the helper's tests: all of the package's
    (let ((buffer-file-name (expand-file-name "crates/app/tests/common/mod.rs" root)))
      (should (equal (ide-rust--file-test-filter) '("--manifest-path" "crates/app/Cargo.toml"))))))

;;; lang-rust-test.el ends here
