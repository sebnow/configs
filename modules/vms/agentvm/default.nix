{ ... }:
{
  configurations.nixos."agentvm" = {
    system = "x86_64-linux";
    module =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        claude = "${pkgs.claude-code}/bin/claude";
        git = "${pkgs.git}/bin/git";
        rm = "${pkgs.coreutils}/bin/rm";
      in
      {
        nixpkgs.config.allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [ "claude-code" ];

        boot.loader.systemd-boot.enable = true;
        boot.loader.efi.canTouchEfiVariables = true;

        fileSystems."/" = {
          device = "/dev/vda";
          fsType = "ext4";
        };

        virtualisation.vmVariant.virtualisation = {
          graphics = false;
          memorySize = 4096;
          cores = 2;
          # Applies only when the disk image is first created.
          diskSize = 20480;
          forwardPorts = [
            {
              from = "host";
              host.address = "127.0.0.1";
              host.port = 2222;
              guest.port = 22;
            }
          ];
        };

        services.openssh.enable = true;

        # Agents get a project's toolchain from its flake with nix develop.
        nix.settings.experimental-features = [
          "nix-command"
          "flakes"
        ];
        # Lets prebuilt binaries run, such as Go toolchain downloads and
        # npm packages with native executables.
        programs.nix-ld.enable = true;

        users.users.admin = {
          isNormalUser = true;
          extraGroups = [ "wheel" ];
          initialPassword = "agentvm";
        };

        users.users.orchestrator = {
          isNormalUser = true;
          group = "orchestrator";
        };
        users.groups.orchestrator = { };

        # Claude Code keeps its login in ~orch-agent/.claude.
        users.users.orch-agent = {
          isNormalUser = true;
          group = "orch-agent";
        };
        users.groups.orch-agent = { };

        systemd.tmpfiles.rules = [
          # The daemon sets 0711 itself when it runs with a harness user.
          "d /var/lib/orchestrator 0711 orchestrator orchestrator -"
          "d /srv/orchestrator/workspaces 0755 orch-agent orch-agent -"
        ];

        # The daemon runs git and rm by the absolute paths it finds on its
        # PATH, so these must match the service's path below.
        security.sudo.extraConfig = ''
          Cmnd_Alias ORCH_CLAUDE = ${claude}
          Cmnd_Alias ORCH_GIT = ${git}
          Cmnd_Alias ORCH_RM = ${rm}
          Cmnd_Alias ORCH_HARNESS = ORCH_CLAUDE, ORCH_GIT, ORCH_RM
          Defaults!ORCH_HARNESS !requiretty, umask=0077
          Defaults!ORCH_CLAUDE env_keep += "PATH SSH_AUTH_SOCK"
          Defaults!ORCH_GIT env_keep += "PATH SSH_AUTH_SOCK GIT_TERMINAL_PROMPT GIT_CONFIG_GLOBAL GIT_CONFIG_NOSYSTEM GIT_SSH_COMMAND"
          Defaults!ORCH_CLAUDE !secure_path
          orchestrator ALL = (orch-agent) CWD=* NOPASSWD: ORCH_HARNESS
        '';

        # The daemon binary is installed by hand at /usr/local/bin/orchestrator-daemon.
        # Deployment inputs arrive as systemd credentials from the
        # hypervisor, keeping the token out of the Nix store:
        #   orchestrator.server-url  the server's https daemon address
        #   orchestrator.ca          the server's CA certificate (PEM)
        #   orchestrator.enrol-token "<daemon-id>:<secret>", first start only
        systemd.services.orchestrator-daemon = {
          description = "orchestrator daemon";
          wantedBy = [ "multi-user.target" ];
          wants = [ "network-online.target" ];
          after = [ "network-online.target" ];
          # sudo comes from /run/wrappers. claude inherits this PATH, so it
          # also holds the agents' tools. git and coreutils come first so
          # that git and rm resolve to the paths in the sudoers rule.
          path = [
            "/run/wrappers"
            pkgs.git
            pkgs.coreutils
            pkgs.openssh
            pkgs.bash
            pkgs.findutils
            pkgs.gnugrep
            pkgs.gnused
            pkgs.gawk
            pkgs.diffutils
            pkgs.patch
            pkgs.which
            pkgs.file
            pkgs.tree
            pkgs.procps
            pkgs.ripgrep
            pkgs.fd
            pkgs.jq
            pkgs.yq-go
            pkgs.curl
            pkgs.gnutar
            pkgs.gzip
            pkgs.bzip2
            pkgs.xz
            pkgs.zstd
            pkgs.unzip
            pkgs.zip
            pkgs.gnumake
            pkgs.gcc
            pkgs.pkg-config
            pkgs.jujutsu
            pkgs.python3
            config.nix.package
          ];
          script = ''
            creds=''${CREDENTIALS_DIRECTORY:?no orchestrator.* credentials were passed to the VM}
            enrol=()
            if [ -e "$creds/orchestrator.enrol-token" ]; then
              enrol=(-enrol-token "$(< "$creds/orchestrator.enrol-token")")
            fi
            exec /usr/local/bin/orchestrator-daemon \
              -server "$(< "$creds/orchestrator.server-url")" \
              -ca "$creds/orchestrator.ca" \
              "''${enrol[@]}" \
              -state-dir /var/lib/orchestrator \
              -harness-user orch-agent \
              -workspace-dir /srv/orchestrator/workspaces \
              -claude ${claude}
          '';
          serviceConfig = {
            User = "orchestrator";
            Group = "orchestrator";
            ImportCredential = "orchestrator.*";
            Restart = "on-failure";
            RestartSec = 10;
          };
        };

        networking.hostName = "agentvm";
        time.timeZone = "UTC";
        i18n.defaultLocale = "en_US.UTF-8";
        system.stateVersion = "26.05";
      };
  };
}
