# Dendritic Pattern Examples

## User Module

A user module configures everything about a user
across all configuration classes:

```nix
# modules/users/vic.nix
let
  userName = "vic";
in
{
  flake.modules = {
    nixos.${userName} = {
      users.users.${userName} = { isNormalUser = true; };
    };

    darwin.${userName} = {
      system.primaryUser = userName;
    };

    homeManager.${userName} = {
      home.username = userName;
      # shell, git, editor, dotfiles...
    };
  };
}
```

`userName` shares across all classes through `let`,
not through `specialArgs`.

## Incremental Features

Split a growing feature into sub-files
without introducing dependencies between them:

```
modules/
  editor/
    basic.nix       # Core editor config
    lsp.nix         # Language server setup
    ai.nix          # AI completion
    _experiments.nix # Disabled (underscore prefix)
```

Each file contributes to the same aspect independently.
Prefix with `_` to temporarily disable during refactoring.

## Sharing Values via Flake-Parts Options

For values that multiple aspect files need,
define flake-parts options instead of `specialArgs`:

```nix
# modules/network-config.nix
{ lib, ... }:
{
  options.network = {
    domain = lib.mkOption {
      type = lib.types.str;
      default = "example.com";
    };
  };
}
```

```nix
# modules/ssh.nix
{ config, ... }:
{
  flake.modules.nixos.ssh = {
    # Access shared value through config
    services.openssh.banner = "Welcome to ${config.network.domain}";
  };
}
```

This replaces the `specialArgs` pattern entirely.
All modules share the same top-level `config` namespace.

## The deferredModule Type

`flake.modules.<class>.<aspect>` uses the `deferredModule` type.
This means multiple files can contribute to the same aspect
and their values merge:

```nix
# modules/ssh/server.nix
{
  flake.modules.nixos.ssh = {
    services.openssh.enable = true;
  };
}

# modules/ssh/hardening.nix
{
  flake.modules.nixos.ssh = {
    services.openssh.settings.PermitRootLogin = "no";
    services.openssh.settings.PasswordAuthentication = false;
  };
}
```

Both modules contribute to `flake.modules.nixos.ssh`.
The `deferredModule` type merges them
using standard Nixpkgs module merge semantics.

Aspect values can also be functions
to access the lower-level module arguments:

```nix
# modules/shell.nix
{ config, ... }:
{
  flake.modules.nixos.shell = nixosArgs: {
    programs.fish.enable = true;
    users.users.${config.username}.shell =
      nixosArgs.config.programs.fish.package;
  };
}
```

Here `config` is the flake-parts top-level config,
while `nixosArgs.config` is the NixOS evaluation config.

## Community Sharing with Dendrix

Repos can share subsets of their configuration:

```
modules/
  community/       # Shared publicly via Dendrix
    ai.nix
    macos-keys.nix
  hosts/           # Private, host-specific
    myhost/
      hardware.nix
  users/           # Private, user-specific
    vic/
      secrets.nix
```

Import community aspects from other repos:

```nix
# modules/ai.nix
{ inputs, ... }:
{
  imports = [ inputs.dendrix.some-repo.ai ];
}
```

Dendrix discovers aspects and classes from community repos,
enabling reuse of generic, host-independent configurations.

## Feature-Centric Naming

Name files around usability concerns, not technical layers:

Prefer:
- `macos-like-bindings.nix`
- `scrolling-desktop.nix`
- `tui.nix`
- `cli.nix`
- `ai.nix`

Avoid:
- `keybindings.nix` (too generic)
- `desktop.nix` (too broad)
- `packages.nix` (organized by type, not feature)

File paths serve as documentation.
A reader scanning `modules/` should understand
what the system does from file names alone.
