EMACS ?= emacs
ROOT  := $(CURDIR)
RUN   := $(EMACS) --init-directory $(ROOT)
# Prefer the mise-installed tools (node) when present.
export PATH := $(shell tr -d '()"' < var/mise/path.eld 2>/dev/null | tr ' ' ':'):$(PATH)

.PHONY: all packages compile test docs-check docs-update docs-ui run clean tools doctor desktop

all: packages compile test docs-check

packages:
	$(RUN) --batch -l init.el --eval '(progn (elpaca-process-queues) (elpaca-wait))'

compile:
	$(RUN) --batch -l init.el -l tests/compile.el

test:
	$(RUN) --batch -l init.el -l tests/smoke-test.el
	$(RUN) --batch -l init.el -l tests/ai-test.el
	$(RUN) --batch -L lisp/ansible-intelligence -l init.el -l tests/ansible-intelligence-tests.el -f ert-run-tests-batch-and-exit
	$(RUN) --batch -l init.el -l tests/ansible-integration-test.el

docs-check:
	IDE_NO_USER=1 $(RUN) --batch -l init.el -l tests/docs-check.el
	$(RUN) --batch -l init.el -l tests/docs-update-test.el

# Refresh docs/index.html from the live config without an AI agent (facts + docstrings).
docs-update:
	IDE_NO_USER=1 $(RUN) --batch -l init.el -f ide-docs-update-batch

# Drives docs/index.html in jsdom (needs node; jsdom is installed into .cache/ on first use).
docs-ui:
	@test -d .cache/jsdom/node_modules/jsdom || npm install --prefix .cache/jsdom --no-audit --no-fund --ignore-scripts jsdom
	JSDOM_PATH=$(ROOT)/.cache/jsdom/node_modules/jsdom node tests/docs-ui-test.js docs/index.html

tools:
	$(RUN) --batch -l init.el -f ide-mise-setup-batch

doctor:
	$(RUN) --batch -l init.el --eval '(ide-mise-doctor)'

# Generate foundry.desktop with absolute paths and install it for the current user.
desktop:
	sed 's|@ROOT@|$(ROOT)|g' foundry.desktop.in > foundry.desktop
	install -Dm644 foundry.desktop $(HOME)/.local/share/applications/foundry.desktop
	-update-desktop-database $(HOME)/.local/share/applications 2>/dev/null
	@echo "Installed $(HOME)/.local/share/applications/foundry.desktop"

run:
	./launch.sh

clean:
	find . -path ./elpaca -prune -o -name '*.elc' -print -delete
