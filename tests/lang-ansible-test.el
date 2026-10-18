;;; lang-ansible-test.el --- Tests for modules/11-languages/lang-ansible.el -*- lexical-binding: t; -*-

;;; Commentary:
;; The wiring: when ansible-xref turns on, and that Eglot still answers the rest.
;; The backend itself is tested in ansible-xref-test.el.

;;; Code:

(require 'ert)
(require 'xref)
(require 'eglot)
(require 'yaml-ts-mode)
(require 'lang-ansible)

(defun lang-ansible-test--backends (text)
  "Return `xref-backend-functions' of a `yaml-ts-mode' buffer holding TEXT, with Eglot added."
  (with-temp-buffer
    (insert text)
    (yaml-ts-mode)
    (add-hook 'xref-backend-functions #'eglot-xref-backend nil t)
    xref-backend-functions))

(ert-deftest lang-ansible-detects-plays-without-a-telling-filename ()
  (with-temp-buffer
    (insert "- hosts: web\n  tasks: []\n")
    (should (ide--ansible-file-p)))
  (with-temp-buffer
    (insert "key: value\nother: 1\n")
    (should-not (ide--ansible-file-p))))

(ert-deftest lang-ansible-xref-is-tried-before-eglot-in-ansible-buffers-only ()
  (let ((ansible (lang-ansible-test--backends "- hosts: web\n  tasks: []\n"))
        (plain (lang-ansible-test--backends "key: value\n")))
    (should (eq (car ansible) 'ansible-xref-backend))
    (should (memq 'eglot-xref-backend ansible))
    (should-not (memq 'ansible-xref-backend plain))))

(provide 'lang-ansible-test)
;;; lang-ansible-test.el ends here
