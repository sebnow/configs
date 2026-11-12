{ ... }:
{
  flake.modules.homeManager.ipfs =
    { pkgs, lib, ... }:
    {
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
          # The default gateway port (8080) collides with common dev servers.
          ExecStartPre = "${pkgs.kubo}/bin/ipfs config Addresses.Gateway /ip4/127.0.0.1/tcp/8480";
          ExecStart = "${pkgs.kubo}/bin/ipfs daemon --enable-gc";
          Restart = "on-failure";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    };
}
