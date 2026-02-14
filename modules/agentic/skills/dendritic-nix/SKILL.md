---
name: dendritic-nix
description: "Applies the Dendritic Pattern for structuring Nix flake configurations with flake-parts. Every file is a flake-parts module organized by feature (aspect), not by configuration class. Use when structuring Nix flakes, creating flake-parts modules, composing multi-host configs, organizing NixOS/home-manager/nix-darwin configuration, or sharing values across configuration classes. Triggers: nix flake structure, flake-parts modules, multi-host configs, aspect modules. Do NOT use for general Nix language questions, nixpkgs packaging, or NixOS module authoring unrelated to flake structure."
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

## Instructions

### Step: Verify Required Scaffolding

`flake.modules` is not a built-in flake-parts option.
It lives in `flake-parts.flakeModules.modules` (an extra)
and must be explicitly imported.
Without it, multiple aspect files defining `flake.modules`
produces: `The option 'flake.modules' is defined multiple times`.

Before creating aspect modules, verify a module imports this extra:

```nix
# modules/flake-parts.nix
{ lib, inputs, ... }:
{
  imports = [ inputs.flake-parts.flakeModules.modules ];
  config.flake.modules = lib.mkDefault {};
}
```

Also verify a wiring module exists for each configuration class in use.
A wiring module defines a `configurations.<class>` option
and maps it to flake outputs.

For example, a NixOS wiring module:

```nix
# modules/nixos.nix
{ lib, config, ... }:
{
  options.configurations.nixos = lib.mkOption {
    type = lib.types.lazyAttrsOf (lib.types.submodule {
      options.module = lib.mkOption {
        type = lib.types.deferredModule;
      };
    });
    default = {};
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

Equivalent wiring modules follow the same shape
for home-manager and nix-darwin,
mapping to `perSystem.legacyPackages.homeConfigurations`
or `flake.darwinConfigurations` respectively.

### Step: Create Aspect Modules

Each file implements a single feature across all relevant configuration classes.
Place all modules under `./modules/`.

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

The `flake.modules` option is typed `lazyAttrsOf (lazyAttrsOf deferredModule)`,
keyed by class then aspect name.
The `deferredModule` type merges values
when multiple files contribute to the same aspect.

### Step: Compose Hosts

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
      nixos.ssh
      nixos.vpn
      nixos.shell
    ];
    nixpkgs.hostPlatform = "x86_64-linux";
  };
}
```

Adding a feature to a host means adding one import to the list.
Aspects are referenced through `config.flake.modules.<class>`,
not through `inputs.self`.
The same composition pattern applies to home-manager, nix-darwin,
or any other configuration class with a wiring module.

### Step: Auto-Import with import-tree

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
    {
      imports = [ (inputs.import-tree ./modules) ];
    };
}
```

`import-tree` recursively imports all `.nix` files,
ignoring paths containing `/_`.
No manual import lists to maintain.

### Step: Share Values Across Aspects

For values needed by multiple aspect files,
define flake-parts options — never use `specialArgs` or `extraSpecialArgs`:

```nix
# modules/network-config.nix
{ lib, ... }:
{
  options.network.domain = lib.mkOption {
    type = lib.types.str;
    default = "example.com";
  };
}
```

```nix
# modules/ssh.nix
{ config, ... }:
{
  flake.modules.nixos.ssh = {
    services.openssh.banner = "Welcome to ${config.network.domain}";
  };
}
```

For values shared within a single file, use `let` bindings.

### Optional: Flake Input Management with flake-file

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

Never use `specialArgs` or `extraSpecialArgs`.
Share values through `let` bindings or flake-parts options.

Never organize by configuration class.
Do not create `nixos/`, `home-manager/`, `darwin/` directories.
One file per feature, spanning all relevant classes.

Never manually list imports in `flake.nix`.
Use `import-tree` or equivalent auto-import.

Never scatter a feature across multiple files by class.
If SSH config lives in `nixos/ssh.nix` and `home-manager/ssh.nix`,
consolidate into one file: `modules/ssh.nix`.

The pattern is not a framework or library.
It is a convention for organizing flake-parts modules.
It requires no code dependencies beyond flake-parts itself.

## Common Issues

### `The option 'flake.modules' is defined multiple times`

Cause: Missing import of `inputs.flake-parts.flakeModules.modules`.
Solution: Verify `modules/flake-parts.nix` imports the extra.
See the scaffolding step above.

### Aspect module has no effect on host configuration

Cause: The aspect is defined but not imported in the host composition module.
Solution: Add the aspect to the host's `imports` list
via `config.flake.modules.<class>.<aspect>`.

### Values not shared between configuration classes

Cause: Using `specialArgs` or passing values through function arguments
instead of `let` bindings or flake-parts options.
Solution: Define shared values in `let` at the top of the aspect file,
or create a flake-parts option if multiple files need the value.

### Aspect value needs access to lower-level module arguments

Cause: The aspect body needs NixOS/home-manager `config` or `pkgs`.
Solution: Write the aspect value as a function:

```nix
flake.modules.nixos.shell = nixosArgs: {
  users.users.admin.shell = nixosArgs.config.programs.fish.package;
};
```

Here the outer `config` is flake-parts top-level config,
while `nixosArgs.config` is the NixOS evaluation config.

## Examples

### Example: Adding a new feature aspect

Input: "Add tmux configuration to the flake."
Actions:
1. Create `modules/tmux.nix` with `flake.modules.homeManager.tmux`
2. Add `homeManager.tmux` to host composition imports
Result: Tmux config available to hosts that import it.

### Example: Splitting a growing feature

Input: "The neovim module is too large."
Actions:
1. Create `modules/neovim/default.nix` for core config
2. Create `modules/neovim/_lang/go.nix` for language-specific config
   (prefixed with `_` so import-tree skips it;
   `default.nix` imports it explicitly or the `_` prefix
   allows toggling by renaming)
Result: Feature split into sub-files without changing the aspect interface.

## Further Reading

See [examples](references/examples.md) for detailed patterns
including user modules, incremental features,
deferredModule merge semantics,
and community sharing through Dendrix.

## References

- Pattern definition: github.com/mightyiam/dendritic
- Auto-import library: github.com/vic/import-tree
- Flake input management: github.com/vic/flake-file
- Dendrix community configs: dendrix.oeiuwq.com
