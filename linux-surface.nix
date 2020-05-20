{ config, lib, pkgs, ... }:

{
  # TODO: move into nixos module, set up a nixos option for surface model, use
  # that to enable IPTS, libwacom and other model-dependent config. right now my
  # setup should mostly handle switching between 4.19 and 5.6, but it doesn't
  # handle different surface models at all.
  hardware = {
    firmware = lib.optional
      (lib.versionOlder config.boot.kernelPackages.kernel.version "5.4")
      pkgs.ipts;
  };

  nixpkgs.overlays = [
    # only use for kernels pre-5.4
    (self: super: { ipts = super.callPackage ./pkgs/ipts { }; })
    # Limit patched libwacom to Xorg. Everything still works afaict
    # this avoids huge rebuilds of stuff like qt that depend on libwacom
    (self: super:
      let
        /* nix-shell -I nixpkgs=channel:nixos-unstable \
           -p nix-prefetch-github --run \
           "nix-prefetch-github linux-surface libwacom-surface > libwacom-surface.json"
        */
        libwacomSurface = super.fetchFromGitHub {
          inherit (lib.importJSON ./libwacom-surface.json)
            owner repo rev sha256;
        };
      in {
        # I believe this is for desktop environments that depend on
        # xf86inputlibinput, but otherwise the xorg overlay covers everything
        xf86inputlibinput = super.xf86inputlibinput.override {
          libinput = self.libinput-surface;
        };
        xorg = super.xorg // {
          xf86inputlibinput = super.xorg.xf86inputlibinput.override {
            libinput = self.libinput-surface;
          };
        };
        libinput-surface =
          super.libinput.override { libwacom = self.libwacom-surface; };
        libwacom-surface = super.libwacom.overrideAttrs (oldAttrs: {
          # TODO: clean up this godforsaken mess
          patches = oldAttrs.patches or [ ]
            ++ (map (name: "${libwacomSurface}/${name}") (builtins.concatLists
              (builtins.filter builtins.isList
                (map (builtins.match ".*([[:digit:]]{4}.*)") (builtins.attrNames
                  (lib.filterAttrs (k: v: v == "regular")
                    (builtins.readDir "${libwacomSurface}/")))))));
        });
      })
    (self: super:
      let
        overlayKernel = version:
          let
            /* Can do pinning via these files:
               nix-shell -I nixpkgs=channel:nixos-unstable \
               -p nix-prefetch-github nix-prefetch-scripts --run \
               "nix-prefetch-github linux-surface linux-surface > linux-surface.json; \
               nix-prefetch-url 'mirror://kernel/linux/kernel/v4.x/linux-4.19.123.tar.xz' > linux-4.19.txt; \
               nix-prefetch-url 'mirror://kernel/linux/kernel/v5.x/linux-5.6.13.tar.xz' > linux-5.6.txt"
            */
            linuxSurface = super.fetchFromGitHub {
              inherit (lib.importJSON ./linux-surface.json)
                owner repo rev sha256;
            };
            fullVersion = with builtins;
              head (match ".*([[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+).*"
                (head (match
                  ".*(KERNEL_VERSION: [[:digit:]]+\\.[[:digit:]]+\\.[[:digit:]]+).*"
                  (readFile ("${linuxSurface}/.github/workflows/debian"
                    + (if version == "4.19" then "_lts" else "") + ".yml")))));
          in super."linux_${
            builtins.replaceStrings [ "." ] [ "_" ] version
          }".override {
            argsOverride = {
              version = fullVersion;
              modDirVersion = fullVersion;
              extraMeta.branch = version;
              src = super.fetchurl {
                url = "mirror://kernel/linux/kernel/v${
                    lib.versions.major fullVersion
                  }.x/linux-${fullVersion}.tar.xz";
                sha256 = lib.fileContents (./linux- + "${version}.txt");
              };

              structuredExtraConfig = let
                # TODO: clean up code
                origConf = builtins.readFile
                  "${linuxSurface}/configs/surface-${version}.config";

                flatten = x:
                  if builtins.isList x then
                    builtins.concatMap (y: flatten y) x
                  else
                    [ x ];

                kernelValues = with lib.kernel; {
                  y = yes;
                  n = no;
                  m = module;
                };

                tokenize = sep: str:
                  let x = flatten (builtins.split sep str);
                  in if builtins.length x < 2 then
                    null
                  else {
                    name = builtins.head x;
                    value = kernelValues."${builtins.head (builtins.tail x)}";
                  };

                parseFile = with builtins;
                  sep: str:
                  (listToAttrs (map (tokenize sep) (flatten (filter isList
                    (map (match ".*(CONFIG_.*[mny]$)")
                      (flatten (split "\n" str)))))));
              in with lib.kernel;
              (parseFile "=" origConf) // {
                # https://github.com/NixOS/nixpkgs/issues/88073
                SERIAL_DEV_BUS = yes;
                SERIAL_DEV_CTRL_TTYPORT = yes;

              }
              // (if lib.versionOlder config.boot.kernelPackages.kernel.version
              "5.4" then {
                # https://github.com/linux-surface/linux-surface/issues/61
                PINCTRL_INTEL = yes;
                PINCTRL_SUNRISEPOINT = yes;
              } else
                { });

              # get patches from linux-surface patches directory
              # convert to attrset format nix expects
              kernelPatches = let
                mapDir = f: p:
                  builtins.attrValues
                  (builtins.mapAttrs (k: _: f p k) (builtins.readDir p));
                patch = dir: file: {
                  name = file;
                  patch = dir + "/${file}";
                };
              in mapDir patch "${linuxSurface}/patches/${version}";
            };
          };
      in {
        linux_4_19 = overlayKernel "4.19";
        linux_5_6 = overlayKernel "5.6";
      })
  ];

  boot = {
    # is this necessary?
    kernelModules =
      [ "hid" "hid_sensor_hub" "hid_generic" "usbhid" "hid_multitouch" ];
  };

}
