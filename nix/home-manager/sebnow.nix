{
  config,
  pkgs,
  lib,
  ...
}: {
  imports = [
    ./base.nix
    ./alacritty
    ./fonts
    ./gnome
    ./k9s
    ./kitty
  ];

  home.username = "sebnow";
  home.homeDirectory = "/home/sebnow";
  home.packages = with pkgs; [
    bottom
    fd
    jq
    restic
    #obsidian # TODO: nixpkgs.config.allowUnfree = true;
  ];

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "23.05"; # Please read the comment before changing.

  home.sessionVariables = {
    BROWSER = "firefox";
  };

  fonts.fontconfig.enable = true;

  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  programs.k9s.enable = true;
  programs.kitty.enable = true;
}
