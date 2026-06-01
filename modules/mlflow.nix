{ ... }:
{
  flake.overlays.mlflow-server = final: prev: {
    mlflow-server = prev.callPackage ../pkgs/mlflow-server { };
  };

  flake.modules.homeManager.mlflow =
    { pkgs, config, ... }:
    {
      home.packages = [
        pkgs.mlflow-server
      ];

      # Set MLFLOW_EXPERIMENT_NAME per project in .claude/settings.json or .claude/settings.local.json
      programs.claude-code.settings = {
        env = {
          MLFLOW_CLAUDE_TRACING_ENABLED = "true";
          MLFLOW_TRACKING_URI = "sqlite:///${config.xdg.dataHome}/claude/mlflow.db";
        };
        hooks.Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "mlflow autolog claude stop-hook";
              }
            ];
          }
        ];
      };

      programs.git.ignores = [
        ".claude/mlflow/"
        "/mlruns"
      ];
    };
}
