# user/ — your packages, tools and configuration

Everything here is yours; the IDE never overwrites it.

- `*.el` files load **after** the core modules, in alphabetical order. Name them
  `10-foo.el`, `20-bar.el` to control order. An error in one file shows a warning
  and the rest still load. Start **Foundry (safe mode)** from your launcher, or `./launch.sh --safe`, to skip them.
- `mise.toml` adds CLI tools (`[tools]` section) to `M-x ide-mise-setup`.
- Packages declared with `use-package` install into the sandbox (`elpaca/`).

Quick ways in: `C-c u a` add a package, `C-c u i` install a shipped example,
`C-c u n` new file, `C-c u r` reload. See `docs/index.html` (`C-c h d`) for examples.
