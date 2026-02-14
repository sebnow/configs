---
name: dendritic-nix
description: "Use when structuring Nix flake configurations with flake-parts. Applies the Dendritic Pattern: aspect-oriented modules where every file is a flake-parts module organizing config by feature, not by class. Triggers: nix flake structure, flake-parts modules, multi-host configs, NixOS/home-manager/darwin organization."
---

# Dendritic Pattern for Nix Flakes

The Dendritic Pattern is an aspect-oriented approach
to structuring Nix configurations using flake-parts.
Every Nix file is a flake-parts module of the same type.
Files organize by feature (aspect), not by configuration class.

## Core Rules

Every file in `./modules/` is a flake-parts module.
No exceptions, no other file types.
This eliminates the question "what kind of Nix file is this?"

Each file implements a single feature across all configuration classes
(NixOS, home-manager, nix-darwin, nixvim)
that the feature applies to.
The file path names the feature.

Values share through `let` bindings and flake-parts options,
never through `specialArgs` or `extraSpecialArgs`.

Lower-level configurations (NixOS, home-manager, nix-darwin)
store as option values using `flake.modules.<class>.<aspect>`,
typically with the `deferredModule` type
which provides beneficial merge semantics.

## Required Scaffolding

`flake.modules` is not a built-in flake-parts option.
It lives in `flake-parts.flakeModules.modules` (an extra)
and must be explicitly imported.
Without it, multiple aspect files defining `flake.modules`
produces: `The option 'flake.modules' is defined multiple times`.

Every dendritic repo needs at minimum:

```nix
# modules/flake-parts.nix
{ inputs, ... }:
{
  imports = [
    inputs.flake-parts.flakeModules.modules
  ];
}
```

For NixOS configurations,
a `nixos.nix` module wires `configurations.nixos`
to `flake.nixosConfigurations` and auto-generates checks:

```nix
# modules/nixos.nix
{ lib, config, ... }:
{
  options.configurations.nixos = lib.mkOption {
    type = lib.types.lazyAttrsOf (
      lib.types.submodule {
        options.module = lib.mkOption {
          type = lib.types.deferredModule;
        };
      }
    );
  };

  config.flake = {
    nixosConfigurations = lib.mapAttrs
      (name: { module }: lib.nixosSystem { modules = [ module ]; })
      config.configurations.nixos;

    checks =
      config.flake.nixosConfigurations
      |> lib.mapAttrsToList (
        name: nixos: {
          ${nixos.config.nixpkgs.hostPlatform.system} = {
            "configurations:nixos:${name}" =
              nixos.config.system.build.toplevel;
          };
        }
      )
      |> lib.mkMerge;
  };
}
```

Equivalent modules can be created for darwin, home-manager, etc.

## Structure

```
flake.nix           # Minimal: inputs + mkFlake + import-tree ./modules
modules/
  flake-parts.nix   # Imports flakeModules.modules (required)
  nixos.nix         # Wires configurations.nixos to flake outputs
  ssh.nix           # SSH config across all classes
  vim.nix           # Editor config across all classes
  vpn.nix           # VPN config across all classes
  desktop.nix       # Host composition from aspects
  users/
    vic.nix         # User-specific config across all classes
```

`flake.nix` stays minimal — a manifest of dependencies.
All configuration logic lives in `./modules/`.

Files can be freely renamed, moved, split, or reorganized.
Paths carry no semantic meaning beyond naming the feature.
Prefix a path segment with `_` to exclude it from auto-import.

## Anatomy of an Aspect Module

A single file configures one feature across all relevant classes:

```nix
# modules/ssh.nix
{ config, ... }:
let
  scpPort = 2277;
in {
  flake.modules = {
    nixos.ssh = {
      services.openssh.enable = true;
      services.openssh.ports = [ scpPort ];
      networking.firewall.allowedTCPPorts = [ scpPort ];
    };

    darwin.ssh = {
      # macOS built-in SSH server config
    };

    homeManager.ssh = {
      # ~/.ssh/config, authorized_keys, private key secrets
    };
  };

  perSystem = { pkgs, ... }: {
    # Custom packages using SSH facilities
  };
}
```

The `let` binding shares `scpPort` across all classes
without `specialArgs`.
Everything needed for SSH lives in one file.

The `flake.modules` option (from `flakeModules.modules`)
is typed `lazyAttrsOf (lazyAttrsOf deferredModule)`,
keyed by class then aspect name.
The `deferredModule` type merges values
when multiple files contribute to the same aspect.

## Host Composition

Hosts compose by selecting aspects through `config.flake.modules`:

```nix
# modules/desktop.nix
{ config, ... }:
let
  inherit (config.flake.modules) nixos;
in
{
  configurations.nixos.desktop.module = {
    imports = [
      nixos.admin
      nixos.ssh
      nixos.vpn
      nixos.shell
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };
}
```

This uses the `configurations.nixos` option from the scaffolding `nixos.nix`,
which wires to `flake.nixosConfigurations` and auto-generates checks.
Aspects are referenced through `config.flake.modules.<class>`,
not through `inputs.self`.
Adding a feature to a host means adding one import to the list.

## Auto-Importing with import-tree

Because every file is a flake-parts module,
bulk-import the entire `./modules/` directory:

```nix
# flake.nix
{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    import-tree.url = "github:vic/import-tree";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; }
      (inputs.import-tree ./modules);
}
```

`import-tree` recursively imports all `.nix` files,
ignoring paths containing `/_`.
No manual import lists to maintain.

## Flake Input Management with flake-file

Modules can declare their own required flake inputs:

```nix
# modules/vim.nix
{ inputs, ... }:
{
  flake-file.inputs.nixvim.url = "github:nix-community/nixvim";

  flake.modules.homeManager.vim = {
    # use inputs.nixvim
  };
}
```

`vic/flake-file` manages `flake.nix` automatically,
keeping input declarations close to the code that uses them.
This is optional — inputs can stay in `flake.nix`.

## Anti-Patterns

Never use `specialArgs` or `extraSpecialArgs`:
Share values through `let` bindings within aspect files
or through flake-parts level options.

Never organize by configuration class:
Do not create `nixos/`, `home-manager/`, `darwin/` directories
with separate files per class.
One file per feature, spanning all relevant classes.

Never manually list imports in `flake.nix`:
Use `import-tree` or equivalent auto-import.
Manual import lists become maintenance burden.

Never scatter a feature across multiple files by class:
If SSH config lives in `nixos/ssh.nix` and `home-manager/ssh.nix`,
these should be one file: `modules/ssh.nix`.

## When to Apply

Use the Dendritic Pattern when:
- Managing multiple configuration classes
  (NixOS + home-manager, NixOS + darwin)
- Sharing values across configuration classes
- Structuring a flake-parts based configuration
- Setting up a new multi-host Nix flake

The pattern is not a framework or library.
It is a convention for organizing flake-parts modules.
It requires no code dependencies beyond flake-parts itself.

## Further Reading

See @examples.md for detailed patterns
including user modules, incremental features,
and community sharing through Dendrix.

## References

- Pattern definition: github.com/mightyiam/dendritic
- Dendrix community configs: dendrix.oeiuwq.com
- Auto-import library: github.com/vic/import-tree
- Flake input management: github.com/vic/flake-file
