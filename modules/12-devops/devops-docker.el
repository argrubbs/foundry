;;; devops-docker.el --- Docker porcelain -*- lexical-binding: t; -*-

;;; Commentary:
;; docker.el plus helpers to attach ghostel shells and tail logs.

;;; Code:

(use-package docker
  :commands (docker docker-containers docker-images docker-volumes docker-networks)
  :init (setq docker-show-messages nil))

(use-package dockerfile-mode :mode "Dockerfile\\(\\..*\\)?\\'")

(declare-function ghostel "ghostel")
(defvar ghostel-shell)
(defvar ghostel-buffer-name)

(defun ide-docker-shell (container)
  "Open an interactive shell in running CONTAINER inside a ghostel terminal."
  (interactive
   (list (completing-read "Container: "
                          (process-lines "docker" "ps" "--format" "{{.Names}}") nil t)))
  (require 'ghostel)
  (let ((ghostel-shell (list "docker" "exec" "-it" container "sh" "-c"
                             "command -v bash >/dev/null && exec bash || exec sh"))
        (ghostel-buffer-name (format "*docker-shell:%s*" container)))
    (ghostel)))

(defun ide-docker-logs (container)
  "Stream live logs of CONTAINER into the bottom drawer."
  (interactive
   (list (completing-read "Container: "
                          (process-lines "docker" "ps" "-a" "--format" "{{.Names}}") nil t)))
  (let ((compilation-buffer-name-function (lambda (_) (format "*docker-logs:%s*" container))))
    (compilation-start (format "docker logs -f --tail 200 %s" (shell-quote-argument container))
                       'compilation-mode)))

(provide 'devops-docker)
;;; devops-docker.el ends here
