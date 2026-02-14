{ ... }:
{
  flake.modules.homeManager.ipfs = { pkgs, lib, ... }: {
    home.packages = [ pkgs.kubo ];

    home.activation.ipfsInit = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      if [ ! -d "$HOME/.ipfs" ]; then
        ${pkgs.kubo}/bin/ipfs init
      fi
    '';

    systemd.user.services.ipfs = {
      Unit = {
        Description = "IPFS Daemon";
        After = [ "network.target" ];
      };
      Service = {
        ExecStart = "${pkgs.kubo}/bin/ipfs daemon --enable-gc";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = [ "default.target" ];
      };
    };
  };
}
