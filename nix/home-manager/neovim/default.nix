{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./completion.nix
    ./git.nix
    ./language.nix
    ./telescope.nix
    ./ui.nix
  ];

  config = {
    programs.neovim = {
      enable = true;
      defaultEditor = true;
      extraLuaConfig = "require('sebnow.settings')";
    };

    xdg.configFile."nvim/lua/sebnow/settings.lua" = {
      source = ./lua/sebnow/settings.lua;
    };
  };
}
