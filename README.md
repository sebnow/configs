# configs

Personal Home Manager configuration,
structured as a Nix flake using [flake-parts][] and [import-tree][].

## Prerequisites

- [Nix][] with flakes enabled

## Usage

Apply the configuration:

```sh
nix run home-manager/master -- switch --flake .
```

## Structure

The flake uses the [Dendritic Pattern][]:
every file under `modules/` is a flake-parts module,
automatically imported via import-tree.

### Modules

| Module            | Purpose                                                                                  |
| ----------------- | ---------------------------------------------------------------------------------------- |
| `agentic`         | Claude Code and Pi coding agent setup with custom skills, agents, hooks, and permissions |
| `camunda-modeler` | Camunda Modeler overlay and desktop entry                                                |
| `catppuccin`      | Catppuccin theming (system-wide)                                                         |
| `flake-parts`     | Flake-parts module system plumbing                                                       |
| `fonts`           | Iosevka Nerd Font installation and fontconfig                                            |
| `gnome`           | GNOME Terminal settings                                                                  |
| `go`              | Go environment variables (GOBIN, GOPATH, GOMODCACHE)                                     |
| `home-manager`    | Home Manager configuration scaffolding                                                   |
| `ipfs`            | Kubo IPFS daemon as a systemd user service                                               |
| `neovim`          | Neovim nightly with per-language tooling                                                 |
| `obsidian`        | Obsidian with nixGL wrapping                                                             |
| `overlays`        | nixGL overlay                                                                            |
| `shell`           | Bash, Zsh, bat, eza, fzf, starship                                                       |
| `source-control`  | Jujutsu (primary) and Git with delta                                                     |
| `terminals`       | Ghostty, Kitty, Alacritty                                                                |
| `tmux`            | tmux with vi keybindings                                                                 |

### Packages

| Package           | Description                                  |
| ----------------- | -------------------------------------------- |
| `camunda-modeler` | Camunda Modeler with token simulation plugin |
| `pi-coding-agent` | Pi coding agent CLI (from npm)               |
| `skills-ref`      | Agent Skills reference library CLI           |

[Nix]: https://nixos.org
[flake-parts]: https://github.com/hercules-ci/flake-parts
[import-tree]: https://github.com/vic/import-tree
[Dendritic Pattern]: https://github.com/vic/import-tree
