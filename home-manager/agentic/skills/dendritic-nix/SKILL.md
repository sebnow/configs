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

## Structure

```
flake.nix          # Minimal: inputs + mkFlake + import-tree ./modules
modules/
  ssh.nix          # SSH config across all classes
  vim.nix          # Editor config across all classes
  vpn.nix          # VPN config across all classes
  hosts.nix        # Host compositions from aspects
  users/
    vic.nix        # User-specific config across all classes
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
{ inputs, config, ... }:
let
  scpPort = 2277;
in {
  flake.modules.nixos.ssh = {
    services.openssh.enable = true;
    services.openssh.ports = [ scpPort ];
    networking.firewall.allowedTCPPorts = [ scpPort ];
  };

  flake.modules.darwin.ssh = {
    # macOS built-in SSH server config
  };

  flake.modules.homeManager.ssh = {
    # ~/.ssh/config, authorized_keys, private key secrets
  };

  perSystem = { pkgs, ... }: {
    # Custom packages using SSH facilities
  };
}
```

The `let` binding shares `scpPort` across all classes
without `specialArgs`.
Everything needed for SSH lives in one file.

## Host Composition

Hosts compose by selecting aspects:

```nix
# modules/hosts.nix
{
  flake.nixosConfigurations.my-host =
    inputs.nixpkgs.lib.nixosSystem {
      system = "aarch64-linux";
      modules = with inputs.self.modules.nixos;
        [ ai ssh vpn scrolling-desktop ];
    };

  flake.darwinConfigurations.my-mac =
    inputs.nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = with inputs.self.modules.darwin;
        [ ai ssh vpn ];
    };
}
```

Aspects are reusable across hosts and platforms.
Adding a feature to a host means adding one name to the list.

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
