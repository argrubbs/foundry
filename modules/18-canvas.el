;;; 18-canvas.el --- Web pages on a canvas: canvas-browser -*- lexical-binding: t; -*-

;;; Commentary:
;; canvas-browser shows web pages, laid out by a headless chromium, as a
;; canvas image in an Emacs buffer: JavaScript, logins and video included.
;; It needs Emacs 32 with canvas images and dynamic modules, the
;; canvas-cairo module of canvas-diagram built on this machine, and the
;; programs chromium and Xvfb.  Under an Emacs without canvas images, such
;; as the system Emacs 31, this module does nothing, so McGeEmacs is the
;; same there as before.
;;
;; The three packages are clones side by side under `ide-canvas-directory':
;;
;;   git clone https://github.com/Daskeladden/canvas-keys
;;   git clone https://github.com/Daskeladden/canvas-diagram
;;   git clone https://github.com/Daskeladden/canvas-browser
;;   make -C canvas-diagram EMACS=emacs32      # builds canvas-cairo.so
;;
;;   M-x canvas-browser   open a URL in a page buffer
;;   browse-url           opens in canvas-browser when it is available
;;
;; `ide-canvas-doctor' says what, if anything, is still missing.

;;; Code:

(defvar browse-url-browser-function)
(declare-function canvas-browser-browse-url "canvas-browser" (url &rest args))

(defgroup ide-canvas nil "Web pages drawn on a canvas." :group 'tools)

(defcustom ide-canvas-directory (expand-file-name "~/dev/canvas/")
  "Directory that holds the canvas-keys, canvas-diagram and canvas-browser clones."
  :type 'directory)

(defcustom ide-canvas-browse-url t
  "When non-nil, `browse-url' opens pages in canvas-browser where it can."
  :type 'boolean)

(defcustom ide-canvas-chromium-arguments '("--ozone-platform=x11")
  "Arguments added to chromium's command line when it draws on canvas-browser's Xvfb.
Chromium picks Wayland whenever WAYLAND_DISPLAY is set, and then opens its
window on the desktop instead of on the hidden X display canvas-browser
gives it; pinning it to X11 sends it there.  Not used for a headless chromium."
  :type '(repeat string))

(defconst ide-canvas-packages '("canvas-keys" "canvas-diagram" "canvas-browser")
  "The clones under `ide-canvas-directory', each a directory for `load-path'.")

(defun ide-canvas-available-p ()
  "Non-nil when this Emacs shows canvas images and loads dynamic modules."
  (and (image-type-available-p 'canvas)
       (stringp module-file-suffix)
       t))

(defun ide-canvas--module-file ()
  "The canvas-cairo module as built in the canvas-diagram clone."
  (expand-file-name (concat "canvas-diagram/canvas-cairo" (or module-file-suffix ".so"))
                    ide-canvas-directory))

(defun ide-canvas--chromium-command-line (command)
  "Add `ide-canvas-chromium-arguments' to COMMAND, chromium's command line."
  (if (bound-and-true-p canvas-browser-headless)
      command
    (append command ide-canvas-chromium-arguments)))

(defun ide-canvas-doctor ()
  "Report what canvas-browser needs and whether this machine has it."
  (interactive)
  (let ((rows (list (cons "Emacs with canvas images and modules" (ide-canvas-available-p))
                    (cons "canvas-cairo module built" (file-exists-p (ide-canvas--module-file)))
                    (cons "chromium" (seq-some #'executable-find '("chromium" "chromium-browser" "google-chrome")))
                    (cons "Xvfb" (executable-find "Xvfb"))
                    (cons "websocket package" (locate-library "websocket")))))
    (dolist (name ide-canvas-packages)
      (push (cons (format "%s clone" name)
                  (file-directory-p (expand-file-name name ide-canvas-directory)))
            rows))
    (with-current-buffer (get-buffer-create "*ide-canvas-doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "canvas-browser prerequisites\n\n")
        (dolist (row (nreverse rows))
          (insert (format "  %s  %s\n" (if (cdr row) "ok     " "MISSING") (car row))))
        (insert "\nchromium and Xvfb:  sudo pacman -S chromium xorg-server-xvfb\n"
                (format "module:  make -C %s EMACS=%s\n"
                        (expand-file-name "canvas-diagram" ide-canvas-directory)
                        (expand-file-name invocation-name invocation-directory)))
        (special-mode))
      (display-buffer (current-buffer)))))

(when (ide-canvas-available-p)
  (dolist (name ide-canvas-packages)
    (add-to-list 'load-path (expand-file-name name ide-canvas-directory)))
  ;; transient comes with Emacs; websocket is what canvas-browser speaks
  ;; the DevTools protocol over.
  (use-package websocket)
  (autoload 'canvas-browser "canvas-browser" "Open URL in a page buffer of its own." t)
  (autoload 'canvas-browser-browse-url "canvas-browser"
    "Open URL in a page buffer of its own, as `browse-url' asks a browser to.")
  (autoload 'canvas-browser-embed "canvas-browser")
  (autoload 'canvas-browser-install-ublock "canvas-browser" nil t)
  (advice-add 'canvas-browser-cdp--command-line :filter-return
              #'ide-canvas--chromium-command-line)
  (when ide-canvas-browse-url
    (setq browse-url-browser-function #'canvas-browser-browse-url)))

(provide '18-canvas)
;;; 18-canvas.el ends here
