# Foundry

A sandboxed, full-featured Emacs IDE: tool windows, Magit with Azure DevOps, Kubernetes, Docker,
Terraform, AI and Org, installed entirely under one directory.


Run: `./launch.sh` (or `emacs --init-directory ~/.config/emacs-foundry`).
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

### Keeping it current (`M-x ide-docs-update`)

1. **Facts** are read from the running Emacs: keymaps, transient menus, module
   headers, the Makefile, `mise.toml`, test counts.
2. They are **merged deterministically** into the page's JSON data: removals,
   renames and counts are exact, existing wording is preserved.
3. Only **new or changed** items go to your AI agent (gptel by default; whatever
   backend and model you have configured, after you confirm) for titles and
   descriptions, as strictly validated JSON. The agent never writes HTML.
   If it fails or times out, docstrings are used instead.
4. The candidate page is **validated in a fresh Emacs** (`tests/docs-check.el`).
   Only a passing page replaces `docs/index.html`; the previous one is kept in
   `.cache/docs-backups/`.

Other entry points: `make docs-update` (facts + docstrings, no agent),
`M-x ide-docs-update-with-agent` (starts a gptel-agent session with the task,
which you review and send), `ide-docs-agent-function` (plug in any other agent).
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

`make compile` (strict byte-compile), `make test` (ERT smoke tests),
`make docs-check`, `make docs-ui`, `make packages`, `make tools`, `make doctor`.
`init.el` is intentionally not byte-compiled (elpaca requirement).

## Credentials

Azure DevOps: `AZURE_DEVOPS_EXT_PAT`, an auth-source entry for `dev.azure.com`,
or `az login`.  Confluence: `confluence-site`, `CONFLUENCE_EMAIL`,
`CONFLUENCE_API_TOKEN`.
