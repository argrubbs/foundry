#!/usr/bin/env bash
# End-to-end test: start a real Foundry frame on a sample project and drive it
# like a user (tests/e2e/e2e.el).  Needs a display; a frame will appear.
#
#   tests/e2e/run.sh [--keep]     --keep leaves the sample project and report behind
#
# Your own user/ files are skipped (IDE_NO_USER=1).  State under var/ (recent
# files, history) is the real one, as for any session.
set -u
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
KEEP=0
[ "${1:-}" = "--keep" ] && KEEP=1

if [ -z "${DISPLAY:-}" ] && [ -z "${WAYLAND_DISPLAY:-}" ] && ! ls /run/user/"$(id -u)"/wayland-* >/dev/null 2>&1; then
  if command -v xvfb-run >/dev/null; then
    exec xvfb-run -a -s "-screen 0 1600x1000x24" "$0" "$@"
  fi
  echo "e2e: no display (set DISPLAY, or install xvfb-run)" >&2
  exit 3
fi

WORK=$(mktemp -d -t foundry-e2e.XXXXXX)
PROJECT=$WORK/project
REPORT=$WORK/report.txt
mkdir -p "$PROJECT/chart/templates"

cat > "$PROJECT/hello.py" <<'EOF'
def greet(name):
    return f"hello from {name}"


print(greet("e2e"))
EOF
printf 'print(undefined_name)\n' > "$PROJECT/bad.py"
printf 'console.log("app")\n' > "$PROJECT/app.js"
printf 'package main\n\nfunc main() {}\n' > "$PROJECT/main.go"
printf 'fn main() {\n    println!("hi");\n}\n' > "$PROJECT/lib.rs"
printf 'local x = 1\nprint(x)\n' > "$PROJECT/a.lua"
printf '#!/bin/sh\necho hi\n' > "$PROJECT/s.sh"
printf 'FROM alpine:3.20\nRUN echo hi\n' > "$PROJECT/Dockerfile"
printf 'resource "null_resource" "x" {}\n' > "$PROJECT/main.tf"
printf '[tool]\nname = "e2e"\n' > "$PROJECT/config.toml"
printf 'apiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: e2e\ndata:\n  k: v\n' > "$PROJECT/deploy.yaml"
printf 'apiVersion: v2\nname: e2e\nversion: 0.1.0\n' > "$PROJECT/chart/Chart.yaml"
printf 'apiVersion: v1\nkind: ConfigMap\nmetadata:\n  name: {{ .Release.Name }}\n' > "$PROJECT/chart/templates/d.yaml"
printf -- '- hosts: all\n  vars:\n    greeting: hi\n  tasks:\n    - debug:\n        msg: "{{ greeting }}"\n' > "$PROJECT/playbook.yml"
printf '# e2e sample\n\nbody text\n\n## Second\n\nunique-needle here\n' > "$PROJECT/README.md"
printf '*  oddly spaced item\n\n\n\nthree blank lines above\n' > "$PROJECT/notes.md"
printf 'Get-ChildItem\n' > "$PROJECT/a.ps1"

( cd "$PROJECT" && git init -q && git add -A \
  && git -c user.name=e2e -c user.email=e2e@example.com commit -qm "e2e sample" )

echo "e2e: project $PROJECT"
IDE_NO_USER=1 E2E_PROJECT=$PROJECT E2E_REPORT=$REPORT \
  timeout 300 emacs --init-directory "$ROOT" -l "$ROOT/tests/e2e/e2e.el" >"$WORK/emacs.out" 2>&1
code=$?

if [ -f "$REPORT" ]; then
  # the report ends with the session's messages; show them only on failure
  if [ "$code" -eq 0 ]; then sed '/^--- \*Messages\* ---$/,$d' "$REPORT"; else cat "$REPORT"; fi
else
  echo "e2e: no report written (exit $code)"; cat "$WORK/emacs.out"
fi

if [ "$KEEP" -eq 1 ] || [ "$code" -ne 0 ]; then
  echo "e2e: kept $WORK"
else
  rm -rf "$WORK"
fi
exit "$code"
