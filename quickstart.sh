#!/usr/bin/env bash
# ==============================================================================
# Foundry Quickstart Installer
#
# One-line installation:
#   curl -fsSL https://raw.githubusercontent.com/argrubbs/foundry/main/quickstart.sh | bash
#
# Or with options:
#   curl -fsSL https://raw.githubusercontent.com/argrubbs/foundry/main/quickstart.sh | bash -s -- --help
# ==============================================================================

set -euo pipefail

# --- Configuration & Defaults ------------------------------------------------
FOUNDRY_REPO="${FOUNDRY_REPO:-https://github.com/argrubbs/foundry.git}"
FOUNDRY_BRANCH="${FOUNDRY_BRANCH:-main}"
FOUNDRY_DIR="${FOUNDRY_DIR:-${XDG_CONFIG_HOME:-$HOME/.config}/emacs-foundry}"
FOUNDRY_DESKTOP="${FOUNDRY_DESKTOP:-1}"
FOUNDRY_BIN="${FOUNDRY_BIN:-1}"
FOUNDRY_PACKAGES="${FOUNDRY_PACKAGES:-1}"
FOUNDRY_COMPILE="${FOUNDRY_COMPILE:-1}"
FOUNDRY_TOOLS="${FOUNDRY_TOOLS:-0}"
FOUNDRY_DRY_RUN="${FOUNDRY_DRY_RUN:-0}"

# --- Colors & Typography ------------------------------------------------------
if [ -t 1 ]; then
    BOLD='\033[1m'
    EMBER='\033[38;2;255;140;66m'   # #ff8c42
    GOLD='\033[38;2;245;192;86m'    # #f5c056
    GREEN='\033[38;2;166;199;106m'  # #a6c76a
    MUTED='\033[38;2;168;155;138m'  # #a89b8a
    RED='\033[38;2;242;96;106m'     # #f2606a
    CYAN='\033[38;2;95;208;220m'    # #5fd0dc
    RESET='\033[0m'
else
    BOLD='' EMBER='' GOLD='' GREEN='' MUTED='' RED='' CYAN='' RESET=''
fi

info()    { printf "${MUTED}[·]${RESET} %s\n" "$*"; }
step()    { printf "${EMBER}[+]${RESET} ${BOLD}%s${RESET}\n" "$*"; }
success() { printf "${GREEN}[✓]${RESET} ${BOLD}%s${RESET}\n" "$*"; }
warn()    { printf "${GOLD}[!]${RESET} %s\n" "$*"; }
error()   { printf "${RED}[✗]${RESET} ${BOLD}%s${RESET}\n" "$*" >&2; }

banner() {
    printf "${EMBER}"
    cat << 'EOF'
  ███████╗ ██████╗ ██╗   ██╗███╗   ██╗██████╗ ██████╗ ██╗   ██╗
  ██╔════╝██╔═══██╗██║   ██║████╗  ██║██╔══██╗██╔══██╗╚██╗ ██╔╝
  █████╗  ██║   ██║██║   ██║██╔██╗ ██║██║  ██║██████╔╝ ╚████╔╝ 
  ██╔══╝  ██║   ██║██║   ██║██║╚██╗██║██║  ██║██╔══██╗  ╚██╔╝  
  ██║     ╚██████╔╝╚██████╔╝██║ ╚████║██████╔╝██║  ██║   ██║   
  ╚═╝      ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝╚═════╝ ╚═╝  ╚═╝   ╚═╝   
EOF
    printf "${RESET}"
    printf "${GOLD}  Foundry${RESET} ${MUTED}· The modern IDE workflow, inside Emacs.${RESET}\n\n"
}

usage() {
    cat << EOF
Foundry Quickstart Installer

Usage:
  quickstart.sh [options]
  curl -fsSL https://raw.githubusercontent.com/argrubbs/foundry/main/quickstart.sh | bash -s -- [options]

Options:
  --dir <path>       Target directory (default: ~/.config/emacs-foundry)
  --branch <branch>  Git branch to clone (default: main)
  --repo <url>       Git repository URL (default: https://github.com/argrubbs/foundry.git)
  --tools            Pre-install all CLI tools and language servers via mise (~2 GB)
  --no-packages      Skip pre-installing Elpaca packages
  --no-compile       Skip byte-compiling Emacs Lisp files
  --no-desktop       Skip installing desktop application launcher
  --no-bin           Skip creating ~/.local/bin/foundry symlink
  --dry-run          Show actions without making changes
  -h, --help         Show this help message

Environment Variables:
  FOUNDRY_DIR, FOUNDRY_REPO, FOUNDRY_BRANCH, FOUNDRY_TOOLS,
  FOUNDRY_PACKAGES, FOUNDRY_COMPILE, FOUNDRY_DESKTOP, FOUNDRY_BIN
EOF
    exit 0
}

# --- CLI Arguments Parsing ----------------------------------------------------
parse_args() {
    while [ $# -gt 0 ]; do
        case "$1" in
            --dir)
                FOUNDRY_DIR="$2"; shift 2 ;;
            --branch)
                FOUNDRY_BRANCH="$2"; shift 2 ;;
            --repo)
                FOUNDRY_REPO="$2"; shift 2 ;;
            --tools)
                FOUNDRY_TOOLS=1; shift ;;
            --no-packages)
                FOUNDRY_PACKAGES=0; shift ;;
            --no-compile)
                FOUNDRY_COMPILE=0; shift ;;
            --no-desktop)
                FOUNDRY_DESKTOP=0; shift ;;
            --no-bin)
                FOUNDRY_BIN=0; shift ;;
            --dry-run)
                FOUNDRY_DRY_RUN=1; shift ;;
            -h|--help)
                usage ;;
            *)
                error "Unknown option: $1"
                usage ;;
        esac
    done
}

# --- Check Prerequisites ------------------------------------------------------
check_prerequisites() {
    step "Checking prerequisites..."

    local missing=0

    # git
    if command -v git >/dev/null 2>&1; then
        info "Found git: $(git --version)"
    else
        error "git is not installed."
        missing=1
    fi

    # make
    if command -v make >/dev/null 2>&1; then
        info "Found make: $(make --version 2>&1 | head -n1)"
    else
        error "make is not installed."
        missing=1
    fi

    # curl
    if command -v curl >/dev/null 2>&1; then
        info "Found curl: $(curl --version 2>&1 | head -n1)"
    else
        error "curl is not installed."
        missing=1
    fi

    # emacs
    if command -v emacs >/dev/null 2>&1; then
        local emacs_ver_str
        emacs_ver_str=$(emacs --version 2>&1 | head -n1)
        info "Found Emacs: $emacs_ver_str"

        local major_ver
        major_ver=$(echo "$emacs_ver_str" | grep -oE '[0-9]+(\.[0-9]+)+' | head -n1 | cut -d. -f1 || echo "0")
        if [ "$major_ver" -lt 29 ]; then
            error "Foundry requires Emacs 29.1 or newer (detected Emacs $major_ver)."
            missing=1
        fi
    else
        error "emacs is not installed."
        missing=1
    fi

    if [ "$missing" -ne 0 ]; then
        echo ""
        error "Missing required dependencies. Please install them and re-run:"
        if command -v apt-get >/dev/null 2>&1; then
            info "Debian/Ubuntu: sudo apt-get update && sudo apt-get install -y emacs git make curl"
        elif command -v dnf >/dev/null 2>&1; then
            info "Fedora: sudo dnf install -y emacs git make curl"
        elif command -v pacman >/dev/null 2>&1; then
            info "Arch Linux: sudo pacman -S --needed emacs git make curl"
        elif command -v brew >/dev/null 2>&1; then
            info "macOS: brew install emacs git make curl"
        fi
        exit 1
    fi
}

# --- Clone or Update Repository -----------------------------------------------
setup_repo() {
    if [ "$FOUNDRY_DRY_RUN" = "1" ]; then
        info "[dry-run] Would clone/update $FOUNDRY_REPO ($FOUNDRY_BRANCH) to $FOUNDRY_DIR"
        return 0
    fi

    if [ -d "$FOUNDRY_DIR/.git" ]; then
        step "Updating existing Foundry repository at $FOUNDRY_DIR..."
        local cur_url
        cur_url=$(git -C "$FOUNDRY_DIR" remote get-url origin 2>/dev/null || echo "")
        info "Current remote: $cur_url"
        info "Fetching branch '$FOUNDRY_BRANCH'..."
        git -C "$FOUNDRY_DIR" fetch origin "$FOUNDRY_BRANCH"
        git -C "$FOUNDRY_DIR" checkout "$FOUNDRY_BRANCH"
        git -C "$FOUNDRY_DIR" pull --ff-only origin "$FOUNDRY_BRANCH" || {
            warn "Could not fast-forward existing repository; keeping local working tree."
        }
    elif [ -d "$FOUNDRY_DIR" ] && [ -z "$(ls -A "$FOUNDRY_DIR" 2>/dev/null)" ]; then
        step "Cloning Foundry into empty directory $FOUNDRY_DIR..."
        git clone --branch "$FOUNDRY_BRANCH" "$FOUNDRY_REPO" "$FOUNDRY_DIR"
    elif [ -e "$FOUNDRY_DIR" ]; then
        error "Target directory '$FOUNDRY_DIR' exists and is not empty."
        error "Please move or remove it, or specify a different location using --dir <path>."
        exit 1
    else
        step "Cloning Foundry into $FOUNDRY_DIR..."
        mkdir -p "$(dirname "$FOUNDRY_DIR")"
        git clone --branch "$FOUNDRY_BRANCH" "$FOUNDRY_REPO" "$FOUNDRY_DIR"
    fi

    chmod +x "$FOUNDRY_DIR/launch.sh"
}

# --- Package Installation & Compilation ---------------------------------------
bootstrap_foundry() {
    if [ "$FOUNDRY_DRY_RUN" = "1" ]; then
        info "[dry-run] Would pre-fetch Elpaca packages and byte-compile"
        return 0
    fi

    if [ "$FOUNDRY_PACKAGES" = "1" ]; then
        step "Pre-installing Elpaca packages (ensures fast initial launch)..."
        make -C "$FOUNDRY_DIR" packages
        success "Packages installed."
    fi

    if [ "$FOUNDRY_COMPILE" = "1" ]; then
        step "Byte-compiling Foundry configuration..."
        make -C "$FOUNDRY_DIR" compile
        success "Byte-compilation complete."
    fi
}

# --- Desktop & CLI Integration ------------------------------------------------
setup_integrations() {
    if [ "$FOUNDRY_DRY_RUN" = "1" ]; then
        info "[dry-run] Would configure CLI launcher and desktop entry"
        return 0
    fi

    # CLI symlink: ~/.local/bin/foundry
    if [ "$FOUNDRY_BIN" = "1" ]; then
        local bin_dir="$HOME/.local/bin"
        mkdir -p "$bin_dir"
        ln -sf "$FOUNDRY_DIR/launch.sh" "$bin_dir/foundry"
        success "Created CLI command: $bin_dir/foundry"

        if [[ ":$PATH:" != *":$bin_dir:"* ]]; then
            warn "$bin_dir is not currently in your PATH."
            info "Consider adding it to your shell profile (~/.bashrc or ~/.zshrc):"
            info "  export PATH=\"\$HOME/.local/bin:\$PATH\""
        fi
    fi

    # Desktop entry (Linux)
    if [ "$FOUNDRY_DESKTOP" = "1" ] && [ "$(uname -s)" = "Linux" ]; then
        step "Installing desktop application launcher..."
        make -C "$FOUNDRY_DIR" desktop || warn "Could not install desktop entry; continuing."
        success "Desktop launcher installed (Foundry is in your application menu)."
    fi

    # Optional mise tools
    if [ "$FOUNDRY_TOOLS" = "1" ]; then
        step "Installing developer CLI tools & language servers via mise (~2 GB)..."
        make -C "$FOUNDRY_DIR" tools
        success "Developer tools installed."
    fi
}

# --- Validation ---------------------------------------------------------------
verify_setup() {
    if [ "$FOUNDRY_DRY_RUN" = "1" ]; then
        info "[dry-run] Would run smoke verification"
        return 0
    fi

    step "Verifying installation..."
    IDE_NO_USER=1 emacs --init-directory "$FOUNDRY_DIR" --batch -l "$FOUNDRY_DIR/early-init.el" -l "$FOUNDRY_DIR/init.el" --eval '(message "Foundry core initialized successfully.")'
    success "Foundry core verified cleanly."
}

# --- Completion Summary -------------------------------------------------------
print_summary() {
    printf "\n"
    success "Foundry is installed and ready!"
    printf "\n"
    printf "${BOLD}How to launch:${RESET}\n"
    if [ -x "$HOME/.local/bin/foundry" ]; then
        printf "  ${EMBER}foundry${RESET}                    # Direct terminal launcher\n"
        printf "  ${EMBER}foundry --safe${RESET}             # Safe mode (skips user/ config)\n"
    fi
    printf "  ${EMBER}%s/launch.sh${RESET}\n\n" "$FOUNDRY_DIR"

    if [ "$FOUNDRY_TOOLS" = "0" ]; then
        printf "${BOLD}Language tools & CLIs (mise):${RESET}\n"
        printf "  ${MUTED}Foundry bundles 54 tools & language servers via mise.${RESET}\n"
        printf "  ${MUTED}Run ${EMBER}make -C %s tools${RESET}${MUTED} or press ${GOLD}C-c w 8 m${RESET}${MUTED} in Emacs to install them.${RESET}\n\n" "$FOUNDRY_DIR"
    fi

    printf "${BOLD}Getting started:${RESET}\n"
    printf "  ${GOLD}C-c h d${RESET}    Open interactive documentation & keybinding explorer\n"
    printf "  ${GOLD}C-c w 8${RESET}    Services, tools, Docker, Kubernetes & DevOps drawer\n"
    printf "  ${GOLD}C-c a M${RESET}    Select AI model (local Ollama / Claude / Gemini)\n"
    printf "  ${GOLD}C-c u n${RESET}    Create custom user config in user/\n\n"
    printf "${MUTED}Enjoy coding in Foundry!${RESET}\n"
}

# --- Main Entry Point ---------------------------------------------------------
main() {
    banner
    parse_args "$@"
    check_prerequisites
    setup_repo
    bootstrap_foundry
    setup_integrations
    verify_setup
    print_summary
}

main "$@"
