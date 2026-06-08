{ config, inputs, ... }:
let
  inherit (config.flake.modules) homeManager;
  overlays = builtins.attrValues config.flake.overlays;
in
{
  configurations.nixos."devvm" = {
    system = "x86_64-linux";
    module =
      { pkgs, lib, ... }:
      {
        imports = [ inputs.home-manager.nixosModules.home-manager ];

        nixpkgs.overlays = overlays;

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        fileSystems."/" = {
          device = "/dev/vda";
          fsType = "ext4";
        };

        hardware.graphics.enable = true;
        security.polkit.enable = true;
        # Required for GSettings dconf backend (GIO_EXTRA_MODULES); without this
        # libadwaita cannot read color-scheme from dconf and defaults to light.
        programs.dconf.enable = true;

        # Provide a DRM/KMS-capable GPU for Wayland compositing.
        # Without a virtio-vga device, x86 QEMU defaults to std VGA which has no
        # DRM device and niri's smithay backend hangs at initialisation.
        # virtio-vga-gl enables Virgil3D (host-accelerated 3D via virgl).
        virtualisation.vmVariant.virtualisation.qemu.options = [
          "-device virtio-vga-gl"
          "-display gtk,gl=on"
        ];

        services.pipewire = {
          enable = true;
          alsa.enable = true;
          pulse.enable = true;
        };

        # Auto-login sebnow directly into a niri session.
        services.greetd = {
          enable = true;
          settings.default_session = {
            command = "${pkgs.niri}/bin/niri --session";
            user = "dev";
          };
        };

        environment.systemPackages = [ pkgs.niri ];

        users.users.dev = {
          isNormalUser = true;
          extraGroups = [
            "wheel"
            "video"
            "audio"
          ];
          initialPassword = "devvm";
        };

        networking.hostName = "devvm";
        time.timeZone = "UTC";
        i18n.defaultLocale = "en_US.UTF-8";
        system.stateVersion = "26.05";

        nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "claude-code" ];

        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          backupFileExtension = "backup";
          users.dev =
            { ... }:
            {
              imports = [
                homeManager.agentic
                homeManager.catppuccin
                homeManager.fonts
                homeManager.go
                homeManager.neovim
                homeManager.niri
                homeManager.shell
                homeManager.source-control
                homeManager.terminals
              ];

              home.username = "dev";
              home.homeDirectory = "/home/dev";
              home.stateVersion = "26.05";
              home.packages = with pkgs; [
                bottom
                fd
                jq
              ];

              programs.ghostty.enable = true;
              programs.ghostty.isDefault = true;
              programs.direnv = {
                enable = true;
                nix-direnv.enable = true;
              };

              fonts.fontconfig.enable = true;
            };
        };
      };
  };
}
