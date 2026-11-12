# Emacs 32 when installed (see launch.sh), else the system Emacs.
EMACS ?= $(shell command -v emacs32 >/dev/null 2>&1 && echo emacs32 || echo emacs)
ROOT  := $(CURDIR)
RUN   := $(EMACS) --init-directory $(ROOT)
# Prefer the mise-installed tools (node) when present.
export PATH := $(shell tr -d '()"' < var/mise/path.eld 2>/dev/null | tr ' ' ':'):$(PATH)

.PHONY: all packages compile test e2e docs docs-check docs-update run clean tools voice voice-mac doctor desktop

all: packages compile test docs-check

packages:
	$(RUN) --batch -l init.el

compile:
	$(RUN) --batch -l init.el -l tests/compile.el

test:
	$(RUN) --batch -l init.el -l tests/run.el

# Drives a real McGeEmacs frame like a user (needs a display; a window appears).
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

# Speech-to-text for voice notes: whisper.cpp with Vulkan, where whisper.el looks for it.
# Needs cmake, a C++ compiler, glslc (shaderc), and the Vulkan and SPIR-V headers.
voice:
	test -d .cache/whisper.cpp || git clone --depth 1 https://github.com/ggml-org/whisper.cpp .cache/whisper.cpp
	cmake -S .cache/whisper.cpp -B .cache/whisper.cpp/build -DCMAKE_BUILD_TYPE=Release -DGGML_VULKAN=1
	# A small job count on purpose: each of the 155 shader steps runs up to 16 compilers of its own.
	cmake --build .cache/whisper.cpp/build -j 4 --config Release

# The same on macOS, where whisper.cpp uses Metal.  Needs Homebrew's cmake and ffmpeg.
voice-mac:
	@test "$$(uname -s)" = Darwin || { echo "voice-mac is for macOS; use make voice here."; exit 1; }
	@for tool in cmake ffmpeg; do command -v $$tool >/dev/null || { echo "$$tool is missing; run: brew install cmake ffmpeg"; exit 1; }; done
	test -d .cache/whisper.cpp || git clone --depth 1 https://github.com/ggml-org/whisper.cpp .cache/whisper.cpp
	cmake -S .cache/whisper.cpp -B .cache/whisper.cpp/build -DCMAKE_BUILD_TYPE=Release
	cmake --build .cache/whisper.cpp/build -j 4 --config Release

doctor:
	$(RUN) --batch -l init.el --eval '(ide-mise-doctor)'

# Generate mcgeemacs.desktop with absolute paths and install it for the current user.
desktop:
	sed 's|@ROOT@|$(ROOT)|g' mcgeemacs.desktop.in > mcgeemacs.desktop
	install -Dm644 mcgeemacs.desktop $(HOME)/.local/share/applications/mcgeemacs.desktop
	-update-desktop-database $(HOME)/.local/share/applications 2>/dev/null
	@echo "Installed $(HOME)/.local/share/applications/mcgeemacs.desktop"

run:
	./launch.sh

clean:
	find . \( -path ./elpaca -o -path ./elpa \) -prune -o -name '*.elc' -print -exec rm -f {} +
