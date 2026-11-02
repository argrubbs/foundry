#!/usr/bin/env bash
# Start Foundry.  --safe skips everything in user/ (use it if a user file breaks startup).
# --plain turns off the visual polish (modules/17-juice.el): stock doom-one, no extras.
case "${1:-}" in
  --safe)  shift; export IDE_NO_USER=1 ;;
  --plain) shift; export IDE_NO_JUICE=1 ;;
esac
# Emacs 32 (canvas images: canvas-browser, DOOM) when it is installed, else the
# system Emacs.  EMACS=/path/to/emacs picks another.
EMACS="${EMACS:-$(command -v emacs32 >/dev/null 2>&1 && echo emacs32 || echo emacs)}"
exec "$EMACS" --init-directory "$HOME/.config/emacs-foundry" "$@"
