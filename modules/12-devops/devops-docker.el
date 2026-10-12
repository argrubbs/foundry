;;; devops-docker.el --- Docker porcelain -*- lexical-binding: t; -*-

;;; Commentary:
;; docker.el plus helpers to attach ghostel shells and tail logs.

;;; Code:

(declare-function ide-devops--terminal "lang-devops")
(declare-function ide-lang-run "00-lang-lib")

(use-package docker
  :commands (docker docker-containers docker-images docker-volumes docker-networks)
  :init (setq docker-show-messages nil))

(defun ide-docker-shell (container)
  "Open an interactive shell in running CONTAINER inside a ghostel terminal."
  (interactive
   (list (completing-read "Container: "
                          (process-lines "docker" "ps" "--format" "{{.Names}}") nil t)))
  (ide-devops--terminal
   (format "docker-shell:%s" container)
   (format "docker exec -it %s sh -c 'command -v bash >/dev/null && exec bash || exec sh'"
           (shell-quote-argument container))
   default-directory))

(defun ide-docker-logs (container)
  "Stream live logs of CONTAINER into the bottom drawer."
  (interactive
   (list (completing-read "Container: "
                          (process-lines "docker" "ps" "-a" "--format" "{{.Names}}") nil t)))
  (ide-lang-run (format "docker-logs:%s" container)
                (list "docker" "logs" "-f" "--tail" "200" container)))

(provide 'devops-docker)
;;; devops-docker.el ends here
