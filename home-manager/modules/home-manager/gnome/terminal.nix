{
  config,
  pkgs,
  ...
}: {
  config = {
    programs.gnome-terminal = {
        enable = true;
        showMenubar = false;

        profile."8b2d8148-8199-4efa-9d4a-df71d346c75e" = {
            default = true;
            visibleName = "default";
            font = "Iosevka Term 12";
        };
    };
  };
}
