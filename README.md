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

### niri session setup

Install the system-level packages from Arch's `extra` repository:

```sh
sudo pacman -S niri xwayland-satellite
```

`niri` ships the compositor binary, `niri-session`, the GDM session entry,
the systemd user units, and the xdg-desktop-portal config.
`xwayland-satellite` provides XWayland support for X11 apps running under niri.
Both packages come from pacman rather than home-manager:
GDM and other multi-user services discover their artifacts via `/usr/share/`,
which is outside the user scope home-manager manages.

The runtime config lives in `modules/niri/config.kdl`.
Edit that file in the repo, then run `home-manager switch` to deploy the change.

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
| `niri`            | niri Wayland compositor config and noctalia-shell integration                            |
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

[Nix]: https://nixos.org
[flake-parts]: https://github.com/hercules-ci/flake-parts
[import-tree]: https://github.com/vic/import-tree
[Dendritic Pattern]: https://github.com/vic/import-tree
