# Foundry

A sandboxed, full-featured Emacs IDE: tool windows, Magit with Azure DevOps, Kubernetes, Docker,
Terraform, AI and Org, installed entirely under one directory.


## Quick start

```bash
curl -fsSL https://raw.githubusercontent.com/argrubbs/foundry/main/quickstart.sh | bash
```

Or clone manually:

```bash
git clone https://github.com/argrubbs/foundry.git ~/.config/emacs-foundry
cd ~/.config/emacs-foundry
./launch.sh
```

Nothing is written outside this directory (`var/`, `.cache/`, `elpaca/`).

## Make it yours (`user/`)

Add your own packages, tools and settings without touching the core. Everything
in `user/` loads **after** the core modules (alphabetically), is never overwritten,
and an error in one file only shows a warning (start **Foundry (safe mode)** from your launcher, or `./launch.sh --safe`, to skip
them all).

| Key | Command | |
|---|---|---|
| `C-c u a` | `ide-user-add-package` | Add a package (optionally from a GitHub repo) to `user/packages.el` and install it |
| `C-c u i` | `ide-user-install-example` | Copy a shipped example from `examples/` into `user/` |
| `C-c u n` | `ide-user-new-file` | New `user/NAME.el` from a template |
| `C-c u e` / `C-c u r` | `ide-user-open` / `ide-user-reload` | Open the folder / reload without restarting |

`user/mise.toml` (a `[tools]` table) adds CLI tools to `M-x ide-mise-setup`.
Keep data in `ide-var-dir` / `ide-cache-dir`, keys on `ide-keys-mode-map` (`C-c x` is
free for you) and dock a tool's buffers with `(ide-add-drawer-buffer REGEXP)`.
Examples (Rust, Elfeed, settings, Org extras, a GitHub package, extra tools) and short
recipes are in the docs page (`C-c h d`, "Make it yours").

## Juice

Visual polish lives in `modules/17-juice.el` and is built to be backed out of. The house theme
(`doom-ember`, warm charcoal with ember orange) is in `lisp/themes/`; the rest are independent
*effects* you can switch on and off at runtime. Choices persist in `var/juice.eld`.

| Key | Command | |
|---|---|---|
| `C-c j j` | `ide-juice-toggle` | Toggle one effect (font, ligatures, frame, modeline, pulse, scroll, bell, indent-guides, todo, cursorline, splash, theme, transparency) |
| `C-c j t` / `C-c j n` | `ide-juice-pick-theme` / `ide-juice-next-theme` | Pick a theme with live preview / cycle |
| `C-c j m` | `ide-juice-mode` | Master switch: undo every effect, or bring them back |
| `C-c j s` | `ide-juice-splash` | The start screen |
| `C-c j r` | `ide-juice-reset` | Forget your choices and use the defaults |

Ways out, gentlest first: toggle one effect; `C-c j m`; `./launch.sh --plain` (or `IDE_NO_JUICE=1`),
which gives stock doom-one with nothing extra; `git switch main` if you keep this on its own branch.
`transparency` is off by default (needs a compositor).

## Languages

Every language is meant to be worked in without leaving Emacs: server, formatting, linting,
running, tests, debugging, a REPL and docs where the language has them. **`C-c m` opens the menu for
the language of the buffer**, `S-<f10>` runs it and `S-<f9>` debugs it. The code is in
`modules/11-languages/` (`00-lang-lib.el` holds the shared pieces); each has tests in `tests/lang-*-test.el`.

| Language | Server | Format / lint | Also |
|---|---|---|---|
| Rust | rust-analyzer | rustfmt, clippy | cargo menu, test at point, related tests, macro expansion, codelldb debugging, docs.rs, evcxr REPL |
| TypeScript / JavaScript / HTML / CSS | `tsc --lsp` (TS 7), html/css servers | Prettier, ESLint | package scripts (npm/pnpm/yarn/bun), vitest and jest at point, Node debugging (also through tsx and vitest), Node REPL, Emmet, live preview in the in-Emacs browser |
| SQL | sqls | sql-formatter, sqlfluff | connections (`.sql-connections.eld`, never committed), run statement/region/file, EXPLAIN, schema browsing, CSV export; SQLite, PostgreSQL, MariaDB/MySQL and SQL Server |
| Lua | lua-language-server | stylua | REPL, busted at point, `.luarc.json` presets |
| PowerShell | PowerShell Editor Services | PSScriptAnalyzer | Pester at point, debugger, REPL, `Get-Help` in Emacs |
| Helm / Kubernetes / CI YAML | helm-ls, yaml-language-server | yamllint, actionlint, kubeconform | helm lint/template/install, `kubectl explain` for the field at point, apply/diff dry runs, kube-linter, trivy |
| TOML | taplo | taplo | sort keys, schemas |
| Markdown | marksman | markdownlint, Prettier | live pandoc preview, Markdown/Org conversion, TOC, run a fenced block, link checker |
| Bash / Dockerfile / Compose / Terraform | bash-language-server, docker-langserver, terraform-ls | shfmt, shellcheck, hadolint, tflint | build/run/scan images, compose, trivy |

Notes:

- Extra linters are Flymake backends next to the language server. Emacs 31 refuses to run a backend
  in files it does not trust, so linters that execute project code (ESLint, markdownlint,
  sqlfluff) wait until you run `M-x ide-lang-trust-project` for that project; the plain
  analysers (shellcheck, hadolint, yamllint, ...) run anywhere.
- Tools come from `mise.toml` (`M-x ide-mise-setup`, `M-x ide-mise-doctor`). Cargo, rustup and the Go,
  uv, npm and pip caches are kept inside this directory too (`ide-contain-tool-state`).
- Debuggers are downloaded on demand: `M-x ide-rust-install-debugger`, `M-x ide-web-install-debugger`,
  `M-x ide-powershell-install-pses`.
- Not included yet: Nix, and Lua debugging.
- Known gaps: sqls cannot load columns on SQL Server (a bug in sqls), and `kubectl` apply and diff need
  a reachable cluster. `kind` is installed for a throwaway local one.

## Ansible

YAML that looks like Ansible (a playbook, role, `group_vars`/`host_vars`, or a file with
`hosts:`/`tasks:`/`roles:`) gets `ansible-language-server` through Eglot, `ansible-doc`,
and **ansible-intelligence** (`lisp/ansible-intelligence/`), which follows variables:

- `M-.` / `M-?` on a Jinja variable jump to its definition (play vars, role defaults, vars files,
  `register`, `set_fact`, static includes) or list its references. Elsewhere the language server answers.
- Variable completion (after Eglot's candidates), Eldoc, Imenu and conservative Flymake diagnostics.
- It needs the `jinja` tree-sitter grammar (installed with the other grammars; `M-x ide-treesit-install-grammars`).
  Without it, it falls back to a lexical parser and reports no unresolved-variable diagnostics.
- Tests: `tests/ansible-intelligence-tests.el` (the package) and `tests/ansible-integration-test.el` (the wiring
  with Eglot), both run by `make test`.

## AI

- Inline ghost-text completion: minuet (Ollama by default; `M-x ide-ai-switch-backend`).
- Chat and region actions: gptel (`C-c a i`, `C-c a r`, `C-c a c`).
- Privacy: files that look like secrets (`.env`, `*.tfvars`, keys, kubeconfig, `*secrets*.yaml`; see
  `ide-ai-private-file-regexps`) are never sent to hosted models, whole-buffer sends to them ask first, and
  `M-x ide-ai-mark-private` protects any other buffer. Local Ollama is unaffected.
- Provider and model: `C-c a M` (`ide-ai-set-model`) picks the provider (Ollama, Claude, Gemini) and
  model with completion, like agent-shell's picker: inside a chat or agent buffer it changes that
  session, elsewhere it sets and remembers the default. Set `ide-ai-default-model` for a fixed default;
  add your own endpoints to `ide-ai-providers`.
- Autonomous agent: **gptel-agent** (`C-c a a`): file, shell, Elisp and web tools
  with confirmation, sub-agents, and Agent Skills.  Skills (`SKILL.md`) are found in
  `~/.claude/skills`, the project's `.claude/skills` and installed Claude Code
  plugins (`~/.claude/plugins/cache/*/*/*/skills`); add more via
  `gptel-agent-skill-dirs`.  Use a strong model for agent work; small local models
  are unreliable at tool use.

## Documentation

`docs/index.html` is an offline, interactive guide: tool-window explorer,
searchable keybindings with a which-key style "Try keys" mode, transient-menu
walkthroughs, workflows, feature catalogue and architecture.

| Key | Command | |
|---|---|---|
| `C-c h d` | `ide-docs-open` | Open the page (in-Emacs xwidget view when Emacs has it, else your browser; `C-u` picks a section) |
| `C-c h s` | `ide-docs-search` | Open the keybinding explorer filtered by a query |
| `C-c h c` | `ide-docs-for-command` | Look a command up in the docs |
| `C-c h u` | `ide-docs-update` | Sync the page with the live config (`C-u` = dry run) |
| `C-c w 8 h` / `C-c w 8 H` | | Same open / update from the Services menu |

### Keeping it current (`M-x ide-docs-update` / `make docs`)

1. **Facts** are read from the running Emacs: keymaps, transient menus, module
   headers, the Makefile, `mise.toml`, test counts.
2. They are **merged deterministically** into the page's JSON data: removals,
   renames and counts are exact, existing wording is preserved.
3. **Docstrings** are the sole source of truth: descriptions for new or changed
   items are extracted directly from symbol docstrings (`ide-docs--doc1`).
4. The candidate page is **validated in a fresh Emacs** (`tests/docs-check.el`).
   Only a passing page replaces `docs/index.html`; the previous one is kept in
   `.cache/docs-backups/`.

Run `make docs` (or `make docs-update`) from the command line, or `C-c h u` (`M-x ide-docs-update`) inside Emacs.
`make docs-check` verifies the page and the updater; `make docs-ui` drives the
page in jsdom.

## Desktop launcher

`make desktop` generates `foundry.desktop` (absolute paths) and installs it to
`~/.local/share/applications/`, so "Foundry" appears in your application menu.

## Tools via mise (portable)

CLIs, language servers and debuggers are declared in `mise.toml` and installed
by [mise](https://mise.jdx.dev) into `var/mise/`:

| Command | What it does |
|---|---|
| `M-x ide-mise-setup` (or `C-c w 8 m`) | Downloads a checksum-verified mise into the sandbox if none is on PATH, then runs `mise install` in the bottom drawer and refreshes PATH |
| `M-x ide-mise-doctor` (or `C-c w 8 M`) | Lists every expected executable and where it was found |
| `M-x ide-mise-edit-manifest` | Edit `mise.toml` to add/remove tools |
| `make tools` / `make doctor` | Same, headless (CI, new machines) |

The tool PATH is cached in `var/mise/path.eld`, so startup never spawns mise.
A mise already on your PATH is used as-is, but its state is still redirected
into the sandbox, so your global mise setup is untouched.

## Make targets

`make compile` (strict byte-compile), `make test` (ERT smoke tests), `make docs`,
`make docs-check`, `make docs-ui`, `make packages`, `make tools`, `make doctor`.
`init.el` is intentionally not byte-compiled (elpaca requirement).

## Credentials

Azure DevOps: `AZURE_DEVOPS_EXT_PAT`, an auth-source entry for `dev.azure.com`,
or `az login`.  Confluence: `confluence-site`, `CONFLUENCE_EMAIL`,
`CONFLUENCE_API_TOKEN`.
