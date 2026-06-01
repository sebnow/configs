{
  fetchurl,
  unzip,
  python3Packages,
}:

# Override of nixpkgs `mlflow-server`.
#
# Fixes two issues with the upstream package:
#
# 1. Subprocess imports. mlflow's `.mlflow-wrapped` injects `sys.path` into
#    the parent process only, so any subprocess that re-invokes
#    `sys.executable` (the bare Nix interpreter) cannot import mlflow's
#    runtime dependencies. Upstream works around this by forcing `cmd[0]`
#    in `_exec_cmd` to a hard-coded `gunicornMlflow` wrapper, which breaks
#    mlflow 3.x — uvicorn and `_job_runner` go through the same code path
#    and end up receiving uvicorn arguments fed into gunicorn. This
#    override drops the hack and instead injects `PYTHONPATH` at every
#    subprocess boundary so child interpreters reconstruct the parent's
#    import path.
#
# 2. Missing UI assets. nixpkgs builds mlflow from the GitHub source
#    tarball, which ships the React UI sources but not the compiled JS
#    bundle (`mlflow/server/js/build/index.html` and friends). The PyPI
#    wheel ships the prebuilt bundle, so we fetch the wheel and graft the
#    `build/` directory into the source tree before the install step.

let
  py = python3Packages;
  version = py.mlflow.version;
  wheel = fetchurl {
    url = "https://files.pythonhosted.org/packages/py3/m/mlflow/mlflow-${version}-py3-none-any.whl";
    hash = "sha256-4cKO1MSFV8xSx2bxfxylgmdT3fJB1D8w+ZxF9+prPOA=";
  };
in
py.toPythonApplication (
  py.mlflow.overridePythonAttrs (old: {

    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ unzip ];

    propagatedBuildInputs = old.dependencies ++ [
      py.boto3
      py.mysqlclient
    ];

    postPatch = (old.postPatch or "") + ''
      substituteInPlace mlflow/utils/process.py --replace-fail \
        "env = env or os.environ.copy()" \
        "env = env or os.environ.copy(); env.setdefault('PYTHONPATH', os.pathsep.join(sys.path))"

      substituteInPlace mlflow/server/jobs/utils.py --replace-fail \
        "**os.environ," \
        "**os.environ, 'PYTHONPATH': os.pathsep.join(sys.path),"

      unzip -q -o ${wheel} 'mlflow/server/js/build/*' -d .
    '';
  })
)
