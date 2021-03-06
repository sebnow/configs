{ config, pkgs, ... }:

{
  home.stateVersion = "21.03";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  xdg.enable = true;
  xdg.mime.enable = true;
  targets.genericLinux.enable = true;

  home.username = "sebnow";
  home.homeDirectory = "/home/sebnow";
  home.sessionVariables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "alacritty";
  };

  home.packages = with pkgs; [
    direnv
    emacs
    fd
    git
    keychain
    nixfmt
    openssh
    ripgrep
    unzip
  ];

  programs.alacritty.enable = true;
  programs.fzf.enable = true;
  programs.jq.enable = true;
  programs.starship.enable = true;

  programs.bat = {
    enable = true;
    config = { theme = "base16"; };
  };

  programs.bash = {
    enable = true;
    bashrcExtra = builtins.readFile ../bashrc;
  };

  programs.tmux = {
    enable = true;
    extraConfig = builtins.readFile ../tmux.conf;
  };

  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;
  };

  # TODO: programs.keychain

  services.syncthing.enable = true;
}
