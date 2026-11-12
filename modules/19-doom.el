;;; 19-doom.el --- DOOM on a canvas -*- lexical-binding: t; -*-

;;; Commentary:
;; Daniel Mendler's doom-game (NonGNU ELPA) runs DOOM inside Emacs: the
;; doomgeneric port draws every frame into an Emacs 32 canvas image through
;; a native module.  The package builds that module itself the first time
;; `M-x doom' runs: it downloads a pinned doomgeneric source archive with
;; curl, unpacks it with unzip and compiles doomgeneric_emacs.so with gcc,
;; next to its own files in the package's install directory.  Compiler output and
;; errors go to the ` *doom-log*' buffer.
;;
;; DOOM also needs a WAD data file.  doom-game looks for *.wad in the
;; directory Emacs was started in and in /usr/share/games/doom/, which is
;; where the AUR packages put theirs: `yay -S doom1-wad' (the shareware
;; episode, doom1.wad) or `yay -S freedoom' (freedoom1.wad, freedoom2.wad).
;; `ide-doom-wad' names a WAD anywhere else.
;;
;; The module is linked with `ide-doom-link-flags', -Wl,-Bsymbolic by
;; default.  DOOM carries a SHA1 of its own whose functions have the names
;; of OpenSSL's, SHA1_Init, SHA1_Update and SHA1_Final.  An Emacs that has
;; libcrypto loaded already, as one built with xwidgets does, binds the
;; module's calls to OpenSSL's functions, which read DOOM's context as
;; theirs and crash Emacs in the WAD checksum.  Symbolic binding keeps the
;; module's calls on its own definitions.
;;
;; Emacs reports key presses and no releases, so doom-game releases every
;; key by a timer 0.05 s after the press and relies on the keyboard's
;; autorepeat to keep a held key down.  GNOME starts repeating only after
;; 0.5 s, so a held arrow moves once, stops and moves on later.
;; `ide-doom-key-hold' keeps a key down longer, until the repeats take over,
;; and every key gets a timer of its own, so a second key, fire or a turn,
;; no longer releases the first.  The repeats of the keyboard only ever
;; come for the last key pressed, though, so the first key still goes up
;; when its hold runs out.
;;
;; Like canvas-browser (18-canvas.el) this needs the canvas image type, so
;; under an Emacs without it, such as the system Emacs 31, the module does
;; nothing.
;;
;;   M-x doom                     start the game
;;   M-x ide-doom-doctor          what the game needs and whether it is there
;;   M-x ide-doom-rebuild-module  drop the built module; the next M-x doom builds it again

;;; Code:

(require '18-canvas)

(defvar doom-args)
(defvar doom--keys)

(defgroup ide-doom nil "DOOM on a canvas." :group 'games)

(defcustom ide-doom-wad nil
  "The WAD file DOOM plays, or nil to let doom-game look in its usual places.
Those are *.wad in `default-directory' when the game starts, then
/usr/share/games/doom/."
  :type '(choice (const :tag "Search the usual places" nil) file))

(defcustom ide-doom-link-flags "-Wl,-Bsymbolic"
  "LDFLAGS for the build of doom-game's native module, or \"\" for none.
See the commentary of 19-doom.el for why the module must bind its own
symbols."
  :type 'string)

(defcustom ide-doom-key-hold 0.5
  "Seconds a key stays pressed in DOOM after Emacs saw it pressed.
Emacs reports no key releases, so doom-game releases a key 0.05 s after
the press and relies on the keyboard's autorepeat to keep it pressed.
GNOME starts repeating after 0.5 s by default, so a held direction key
moves once, stops, and only then moves on.  A hold of about the repeat
delay keeps the key pressed until the repeats take over; a tap then moves
for that long, and a second key pressed within the hold is pressed
together with the first.  Nil keeps doom-game's own 0.05 s.  A change
applies at the next key."
  :type '(choice (const :tag "doom-game's own 0.05 s" nil) number))

(defconst ide-doom-wad-directory "/usr/share/games/doom/"
  "Where doom-game looks for a WAD after the current directory.")

(defun ide-doom--wads ()
  "The WAD files doom-game would find, `ide-doom-wad' first when set."
  (append (and ide-doom-wad (file-exists-p ide-doom-wad) (list ide-doom-wad))
          (file-expand-wildcards (expand-file-name "*.wad" ide-doom-wad-directory))))

(defun ide-doom--module-file ()
  "The native module doom-game built, or nil."
  (locate-file (concat "doomgeneric_emacs" module-file-suffix) (cons nil load-path)))

(defun ide-doom--with-link-flags (fn &rest args)
  "Call FN, `doom', with `ide-doom-link-flags' as LDFLAGS for its module build."
  (let ((process-environment
         (if (string-empty-p ide-doom-link-flags)
             process-environment
           (cons (concat "LDFLAGS=" ide-doom-link-flags) process-environment))))
    (apply fn args)))

(defvar ide-doom--held nil
  "The DOOM keys held down, an alist of (CODES . TIMER) with their release timers.")

(defun ide-doom--release (codes)
  "Release the key CODES in DOOM and forget their timer."
  (setq ide-doom--held (assoc-delete-all codes ide-doom--held))
  (setq doom--keys (append doom--keys codes)))

(defun ide-doom--press (codes)
  "Press the key CODES in DOOM, or keep them pressed, for `ide-doom-key-hold'.
Each key has a timer of its own, so pressing another key leaves this one
down until its hold runs out.  doom-game reads a code with bit 8 set as a
press and the bare code as a release."
  (let ((held (assoc codes ide-doom--held)))
    (if held
        (cancel-timer (cdr held))
      (setq doom--keys (append doom--keys (mapcar (lambda (k) (logior k #x100)) codes)))
      (setq held (cons codes nil))
      (push held ide-doom--held))
    (setcdr held (run-at-time (or ide-doom-key-hold 0.05) nil #'ide-doom--release codes))))

(defun ide-doom--key-command (fn key &optional name)
  "Call FN, `doom-key-command', and give its command a hold of its own.
doom-game builds the commands of `doom-mode-map' with `doom-key-command'
when it loads, so this advice reaches every one of them.  doom-game's
command keeps one key down at a time and releases it 0.05 s after the
press; the command put in its place presses through `ide-doom--press',
which holds every key for `ide-doom-key-hold' seconds on a timer of its
own."
  (let ((symbol (funcall fn key name))
        (keys (ensure-list key)))
    (defalias symbol
      (lambda ()
        (interactive nil doom-mode)
        (ide-doom--press keys))
      (documentation symbol))
    symbol))

(defun ide-doom-rebuild-module ()
  "Delete the native module doom-game built, so that the next `doom' builds it again.
Restart Emacs first when the game has run in this session: a module stays
loaded once it is."
  (interactive)
  (let ((module (ide-doom--module-file)))
    (if module
        (progn (delete-file module)
               (message "Deleted %s; the next M-x doom builds it again" module))
      (message "No DOOM module built yet; M-x doom builds it"))))

(defun ide-doom-doctor ()
  "Report what DOOM needs and whether this machine has it."
  (interactive)
  (let ((rows (list (cons "Emacs with canvas images and modules" (ide-canvas-available-p))
                    (cons "doom-game package" (locate-library "doom-game"))
                    (cons "curl" (executable-find "curl"))
                    (cons "unzip" (executable-find "unzip"))
                    (cons "gcc" (executable-find "gcc"))
                    (cons (format "native module built (%s)" (or (ide-doom--module-file) "not yet"))
                          (ide-doom--module-file))
                    (cons (format "WAD file (%s)" (or (car (ide-doom--wads)) "none"))
                          (ide-doom--wads)))))
    (with-current-buffer (get-buffer-create "*ide-doom-doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "DOOM prerequisites\n\n")
        (dolist (row rows)
          (insert (format "  %s  %s\n" (if (cdr row) "ok     " "MISSING") (car row))))
        (insert "\nWAD:  yay -S doom1-wad   (or: yay -S freedoom)\n"
                "The module is built on the first M-x doom; its output is in ` *doom-log*'.\n")
        (special-mode))
      (display-buffer (current-buffer)))))

(when (ide-canvas-available-p)
  (advice-add 'doom :around #'ide-doom--with-link-flags)
  (advice-add 'doom-key-command :around #'ide-doom--key-command)
  (use-package doom-game
    :config
    (when ide-doom-wad
      (setq doom-args (list "-iwad" (expand-file-name ide-doom-wad))))))

(provide '19-doom)
;;; 19-doom.el ends here
