#!/usr/bin/env bash
# Start Foundry.  --safe skips everything in user/ (use it if a user file breaks startup).
if [ "${1:-}" = "--safe" ]; then shift; export IDE_NO_USER=1; fi
exec emacs --init-directory "$HOME/.config/emacs-foundry" "$@"
