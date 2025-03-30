{pkgs, ...}: {
  config = {
    programs.neovim = {
      extraPackages = with pkgs; [
        nixd
        nixfmt-rfc-style
      ];
    };
    xdg.configFile."nvim/lsp/nixd.lua".source = ./nixd.lua;
  };
}
