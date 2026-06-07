{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:

# Builds a single nono pack from the `always-further/nono-packs` monorepo,
# substituting for `nono pull <namespace>/<name>` so packs can be installed
# declaratively.
#
# A pack is just a subdirectory of the source repo, tagged as
# `<name>-v<version>` (e.g. `claude-v0.0.16`). The output of this derivation
# is that pack's contents — typically a Claude Code marketplace plugin with
# `.claude-plugin/`, `hooks/`, `skills/`, `wiring/`, and `policy.json`.
#
# Note: this bypasses the registry's sigstore verification. Pack integrity
# rests on the `hash` pinned here instead.
{
  name,
  version,
  hash,
  owner ? "always-further",
  repo ? "nono-packs",
  subdir ? name,
  tag ? "${name}-v${version}",
}:
stdenvNoCC.mkDerivation {
  pname = "nono-pack-${name}";
  inherit version;

  src = fetchFromGitHub {
    inherit owner repo hash;
    rev = tag;
  };

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall
    if [ ! -d "${subdir}" ]; then
      echo "nono-pack: subdir '${subdir}' missing in source tree" >&2
      ls -la
      exit 1
    fi
    mkdir -p "$out"
    cp -r "${subdir}/." "$out/"
    runHook postInstall
  '';

  meta = {
    description = "nono pack ${name}@${version}";
    homepage = "https://github.com/${owner}/${repo}/tree/${tag}/${subdir}";
    license = lib.licenses.asl20;
  };
}
