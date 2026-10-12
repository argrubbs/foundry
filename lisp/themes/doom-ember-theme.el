;;; doom-ember-theme.el --- A forge-lit dark theme: warm charcoal, ember and steel -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Foundry's house theme.  Built on `doom-themes' so solaire-mode, doom-modeline,
;; org, magit and the rest are already styled.  Warm near-black surfaces,
;; molten-orange accents, amber types and strings picked to stay distinct
;; (functions, keywords, strings, types and constants never share a hue).

;;; Code:

(require 'doom-themes)

(def-doom-theme doom-ember
  "A dark theme: warm charcoal surfaces lit by ember orange."

  ;; name        default   256           16
  ((bg         '("#1c1815" "black"       "black"        ))
   (fg         '("#e3d9ca" "#e0e0e0"     "brightwhite"  ))

   (bg-alt     '("#161310" "black"       "black"        ))
   (fg-alt     '("#a89b8a" "#a0a0a0"     "white"        ))

   (base0      '("#100e0c" "black"       "black"        ))
   (base1      '("#191613" "#1e1e1e"     "brightblack"  ))
   (base2      '("#211d19" "#2e2e2e"     "brightblack"  ))
   (base3      '("#2a2520" "#262626"     "brightblack"  ))
   (base4      '("#3e362e" "#3f3f3f"     "brightblack"  ))
   (base5      '("#6f6355" "#525252"     "brightblack"  ))
   (base6      '("#8a7d6d" "#6b6b6b"     "brightblack"  ))
   (base7      '("#b0a391" "#979797"     "brightblack"  ))
   (base8      '("#ede4d6" "#dfdfdf"     "white"        ))

   (grey       base5)
   (red        '("#f2606a" "#ff6655" "red"          ))
   (orange     '("#ff8c42" "#ff8c42" "brightred"    ))
   (green      '("#a6c76a" "#99bb66" "green"        ))
   (teal       '("#4fc9b0" "#44b9b1" "brightgreen"  ))
   (yellow     '("#f5c056" "#f5c056" "yellow"       ))
   (blue       '("#6ab0f3" "#6ab0f3" "brightblue"   ))
   (dark-blue  '("#2f5d8c" "#2f5d8c" "blue"         ))
   (magenta    '("#d98cc4" "#d98cc4" "brightmagenta"))
   (violet     '("#a995e8" "#a995e8" "magenta"      ))
   (cyan       '("#5fd0dc" "#5fd0dc" "brightcyan"   ))
   (dark-cyan  '("#3c909c" "#3c909c" "cyan"         ))

   (highlight      orange)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      `(,(car (doom-blend orange bg 0.28)) ,@(cdr dark-blue)))
   (builtin        violet)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.2))
   (constants      magenta)
   (functions      orange)
   (keywords       blue)
   (methods        yellow)
   (operators      base7)
   (type           teal)
   (strings        green)
   (variables      base8)
   (numbers        yellow)
   (region         `(,(car (doom-blend orange bg 0.20)) ,@(cdr base3)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   (hl-line-bg   `(,(car (doom-blend orange bg 0.07)) ,@(cdr base2)))
   (paren-bg     `(,(car (doom-blend orange bg 0.35)) ,@(cdr base4)))
   (modeline-fg     fg)
   (modeline-fg-alt base6)
   (modeline-bg     base2)
   (modeline-bg-alt `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg))))

  ;;;; Face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange :weight 'bold)
   (mode-line :background modeline-bg :foreground modeline-fg)
   (mode-line-inactive :background modeline-bg-inactive :foreground modeline-fg-alt)
   (mode-line-emphasis :foreground highlight)
   (hl-line :background hl-line-bg)
   (cursor :background orange)
   (doom-modeline-bar :background orange)
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground yellow :weight 'bold)
   (window-divider :foreground base3)
   (window-divider-first-pixel :foreground base3)
   (window-divider-last-pixel :foreground base3)
   (internal-border :background bg)
   (show-paren-match :background paren-bg :foreground base8 :weight 'bold)
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground orange)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   (solaire-mode-line-face
    :inherit 'mode-line :background modeline-bg-alt)
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive :background modeline-bg-inactive-alt))

  ;;;; Variable overrides
  ())

;;; doom-ember-theme.el ends here
