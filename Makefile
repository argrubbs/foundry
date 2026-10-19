EMACS ?= emacs
ROOT  := $(CURDIR)
RUN   := $(EMACS) --init-directory $(ROOT)
# Prefer the mise-installed tools (node) when present.
export PATH := $(shell tr -d '()"' < var/mise/path.eld 2>/dev/null | tr ' ' ':'):$(PATH)

.PHONY: all packages compile test e2e docs docs-check docs-update run clean tools doctor desktop

all: packages compile test docs-check

packages:
	$(RUN) --batch -l init.el --eval '(progn (elpaca-process-queues) (elpaca-wait))'

compile:
	$(RUN) --batch -l init.el -l tests/compile.el

test:
	$(RUN) --batch -l init.el -l tests/run.el

# Drives a real Foundry frame like a user (needs a display; a window appears).
e2e:
	tests/e2e/run.sh

docs-check:
	IDE_NO_USER=1 $(RUN) --batch -l init.el -l tests/docs-check.el

# Refresh docs/index.html deterministically from live facts and docstrings.
docs: docs-update

docs-update:
	IDE_NO_USER=1 $(RUN) --batch -l init.el -l lisp/docs-update.el -f ide-docs-update-batch

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
	find . -path ./elpaca -prune -o -name '*.elc' -print -exec rm -f {} +
