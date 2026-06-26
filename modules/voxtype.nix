{ inputs, ... }:
{
  flake.modules.homeManager.voxtype =
    { pkgs, lib, ... }:
    let
      toml = pkgs.formats.toml { };

      # nixGL wrappers inject the host GPU drivers nix's loaders can't find on
      # a foreign distro: nixVulkanIntel sets VK_ICD_FILENAMES (mesa, incl. the
      # AMD radv ICD) so whisper.cpp's Vulkan backend and the OSD's wgpu both
      # see the GPU instead of failing; nixGLIntel adds GL/EGL.
      nixgl = inputs.nixgl.packages.${pkgs.stdenv.hostPlatform.system};

      # nixpkgs ships only the OSD launcher, not a frontend, and building the
      # osd-native frontend together with gpu-vulkan breaks whisper.cpp's Vulkan
      # shader link. So build the frontend on its own (no vulkan). The feature
      # must go through cargoBuildFlags: buildRustPackage bakes `buildFeatures`
      # at the function level, so overrideAttrs can't reach it.
      voxtypeOsdNative = pkgs.voxtype.overrideAttrs (o: {
        cargoBuildFlags = (o.cargoBuildFlags or [ ]) ++ [
          "--features"
          "osd-native"
        ];
        buildInputs = (o.buildInputs or [ ]) ++ [
          pkgs.wayland
          pkgs.libxkbcommon
        ];
        nativeBuildInputs = (o.nativeBuildInputs or [ ]) ++ [ pkgs.pkg-config ];
        doCheck = false;
      });

      # Expose only the OSD frontend binary so the launcher finds it on PATH.
      # Grafting it into the daemon's store path would force a rebuild of the
      # Vulkan daemon, whose whisper.cpp Vulkan shader link is flaky and
      # intermittently fails (undefined matmul_id_* refs); the running build is
      # a good one, so we leave it untouched.
      osdBin = pkgs.runCommand "voxtype-osd-bin" { } ''
        mkdir -p $out/bin
        ln -s ${voxtypeOsdNative}/bin/voxtype-osd-native $out/bin/voxtype-osd-native
      '';

      # nixpkgs voxtype vendors whisper.cpp; vulkanSupport adds the gpu-vulkan
      # build feature so transcription runs on the (AMD) GPU via Vulkan.
      voxtype = pkgs.voxtype.override { vulkanSupport = true; };

      # Manage the model declaratively instead of `voxtype setup --download`.
      whisperModel = pkgs.fetchurl {
        name = "ggml-base.en.bin";
        url = "https://huggingface.co/ggerganov/whisper.cpp/resolve/main/ggml-base.en.bin";
        sha256 = "00nhqqvgwyl9zgyy7vk9i3n017q2wlncp5p7ymsk0cpkdp47jdx0";
      };

      # voxtype's deserializer requires whole sections ([hotkey] [audio]
      # [output]) with no serde defaults, so a hand-trimmed config fails to
      # parse. Merge our overrides onto voxtype's own shipped defaults: the
      # file stays valid and version-matched while we only state what differs.
      defaults = fromTOML (builtins.readFile "${voxtype.src}/config/default.toml");

      # On non-NixOS, the host ALSA config routes "default" through a PipeWire
      # plugin that nixpkgs' patched alsa-lib looks for under ALSA_PLUGIN_DIR.
      # Provide both the pulse and pipewire plugins so whichever it references
      # resolves; without this the daemon fails with "Audio connection failed".
      alsaPlugins = pkgs.symlinkJoin {
        name = "voxtype-alsa-plugins";
        paths = [
          pkgs.alsa-plugins
          pkgs.pipewire
        ];
      };
    in
    {
      home.packages = [ voxtype ];

      # Run the daemon declaratively rather than via `voxtype setup` (which
      # writes a unit imperatively). Needs `input` group membership at the
      # system layer for evdev hotkey access.
      systemd.user.services.voxtype = {
        Unit = {
          Description = "Voxtype push-to-talk voice-to-text daemon";
          Documentation = "https://voxtype.io";
          PartOf = [ "graphical-session.target" ];
          After = [
            "graphical-session.target"
            "pipewire.service"
            "pipewire-pulse.service"
          ];
        };
        Service = {
          Type = "simple";
          ExecStart = "${nixgl.nixGLIntel}/bin/nixGLIntel ${nixgl.nixVulkanIntel}/bin/nixVulkanIntel ${voxtype}/bin/voxtype -q daemon";
          Environment = [
            "ALSA_PLUGIN_DIR=${alsaPlugins}/lib/alsa-lib"
            # Let the OSD launcher find the separately-built native frontend.
            "PATH=${osdBin}/bin:${voxtype}/bin:/usr/bin:/bin"
            # The OSD's wgpu dlopen()s libvulkan.so.1 at runtime, which ignores
            # DT_RUNPATH, so the loader must be on LD_LIBRARY_PATH (the nixGL
            # wrappers append to it). Without this wgpu finds no Vulkan adapter.
            "LD_LIBRARY_PATH=${pkgs.vulkan-loader}/lib"
          ];
          Restart = "on-failure";
          RestartSec = 5;
        };
        Install.WantedBy = [ "graphical-session.target" ];
      };

      xdg.configFile."voxtype/config.toml".source = toml.generate "voxtype-config.toml" (
        lib.recursiveUpdate defaults {
          hotkey = {
            # default.toml ships enabled = false; turn the hotkey on.
            enabled = true;
            key = "WEV_53";
            modifiers = [
              "LEFTCTRL"
              "WEV_133"
            ];
          };

          whisper = {
            # Absolute path -> use this file directly (skips the
            # ~/.local/share lookup), so the Nix model is the source of truth.
            model = "${whisperModel}";
            language = "en";
            # Transcribe chunks while recording continues, and optimise the
            # context window for short clips -> text appears far sooner.
            eager_processing = true;
            context_window_optimization = true;
          };

          # Feedback is off by default; with no OSD cue either, capture starts
          # silently and the first word gets clipped. A start beep signals when
          # to speak.
          audio.feedback = {
            enabled = true;
            theme = "subtle";
            volume = 0.45;
          };

          # Only the native frontend is built (gtk4 would pull GTK runtime deps);
          # select it explicitly so the launcher doesn't probe for gtk4 first.
          osd.frontend = "native";
        }
      );
    };
}
