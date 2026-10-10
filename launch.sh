#!/usr/bin/env bash
# Start Foundry.  --safe skips everything in user/ (use it if a user file breaks startup).
# --plain turns off the visual polish (modules/17-juice.el): stock doom-one, no extras.
case "${1:-}" in
  --safe)  shift; export IDE_NO_USER=1 ;;
  --plain) shift; export IDE_NO_JUICE=1 ;;
esac
exec emacs --init-directory "$HOME/.config/emacs-foundry" "$@"
