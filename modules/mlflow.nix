{ ... }:
{
  flake.overlays.mlflow-server = final: prev: {
    mlflow-server = prev.callPackage ../pkgs/mlflow-server { };
  };

  flake.modules.homeManager.mlflow =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.mlflow-server
      ];

      programs.git.ignores = [
        ".claude/mlflow/"
        "/mlruns"
      ];
    };
}
