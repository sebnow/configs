{
  python3Packages,
}:

# Override of nixpkgs `mlflow-server`.
#
# Upstream patches only `mlflow/server/__init__.py` to propagate PYTHONPATH
# into the uvicorn/gunicorn subprocess env. That misses the `_exec_cmd` path
# in `mlflow/utils/process.py` (used by job runners and other callers) and
# `mlflow/server/jobs/utils.py`. This override additionally patches those two
# files so every subprocess boundary receives PYTHONPATH.
#
# Note: upstream switched to building from the PyPI wheel in 3.12.0, so the
# prebuilt JS bundle is already included — no need to graft it manually.

let
  py = python3Packages;
in
py.toPythonApplication (
  py.mlflow.overridePythonAttrs (old: {

    propagatedBuildInputs = old.dependencies ++ [
      py.boto3
      py.mysqlclient
    ];

    postInstall = (old.postInstall or "") + ''
      substituteInPlace $out/lib/python*/site-packages/mlflow/utils/process.py \
        --replace-fail \
        "env = env or os.environ.copy()" \
        "env = env or os.environ.copy(); env.setdefault('PYTHONPATH', os.pathsep.join(sys.path))"

      substituteInPlace $out/lib/python*/site-packages/mlflow/server/jobs/utils.py \
        --replace-fail \
        "**os.environ," \
        "**os.environ, 'PYTHONPATH': os.pathsep.join(sys.path),"
    '';
  })
)
