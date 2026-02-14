{ ... }:
{
  flake.modules.homeManager.go = { config, ... }: {
    home.sessionVariables = {
      GOBIN = "${config.home.homeDirectory}/.local/bin";
      GOPATH = "${config.xdg.dataHome}/go";
      GOMODCACHE = "${config.xdg.cacheHome}/go/pkg/mod";
      GOTELEMETRY = "off";
    };
  };
}
