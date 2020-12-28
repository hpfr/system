{ config, lib, pkgs, ... }:

{
  # move into nixos module, set up a nixos option for surface model, use
  # that to enable IPTS, libwacom and other model-dependent config.

  nixpkgs.overlays = [
    # post-5.4
    (self: super: { iptsd = super.callPackage ../pkgs/iptsd { }; })
    # Limit patched libwacom to Xorg. Everything still works afaict
    # this avoids huge rebuilds of stuff like qt that depend on libwacom
    (self: super:
      let
        /* Pin with
           nix-shell -I nixpkgs=channel:nixos-unstable \
           -p nix-prefetch-github --run \
           "nix-prefetch-github --rev 'TAG' linux-surface libwacom-surface > libwacom-surface.json"
        */
        libwacomSurface = super.fetchFromGitHub {
          inherit (lib.importJSON ../libwacom-surface.json)
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
        overlayKernel = versionArg:
          let
            /* Pin with:
               nix-shell -I nixpkgs=channel:nixos-unstable \
               -p nix-prefetch-github nix-prefetch-scripts --run \
               "nix-prefetch-github linux-surface linux-surface > linux-surface.json; \
               nix-prefetch-url 'mirror://kernel/linux/kernel/v5.x/linux-5.9.8.tar.xz' > linux-5.9.txt"
            */
            linuxSurface = super.fetchFromGitHub {
              inherit (lib.importJSON ../linux-surface.json)
                owner repo rev sha256;
            };
            hasMinor =
              if (builtins.length (builtins.splitVersion versionArg)) > 2 then
                true
              else
                false;
            version = if hasMinor then
              lib.versions.majorMinor versionArg
            else
              versionArg;
            fullVersion = if hasMinor then versionArg else null;
          in super."linux_${
            builtins.replaceStrings [ "." ] [ "_" ] version
          }".override {
            argsOverride = (if fullVersion != null then {
              version = fullVersion;
              modDirVersion = fullVersion;
              extraMeta.branch = version;
              src = super.fetchurl {
                url = "mirror://kernel/linux/kernel/v${
                    lib.versions.major fullVersion
                  }.x/linux-${fullVersion}.tar.xz";
                sha256 = lib.fileContents (../linux- + "${fullVersion}.txt");
              };
            } else
              { }) // {

                structuredExtraConfig = let
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
                    (listToAttrs (map (tokenize sep) (filter (str: str != "")
                      (flatten (map (builtins.split "^CONFIG_") (flatten
                        (filter isList (map (match "^(CONFIG_.*[mny]).*")
                          (flatten (split "\n" str))))))))));
                in with lib.kernel;
                (parseFile "=" origConf) // {
                  # https://github.com/NixOS/nixpkgs/issues/88073
                  SERIAL_DEV_BUS = yes;
                  SERIAL_DEV_CTRL_TTYPORT = yes;

                  # https://github.com/linux-surface/linux-surface/issues/61
                  PINCTRL_INTEL = yes;
                  PINCTRL_SUNRISEPOINT = yes;
                };

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
        # can specify a minor version such as 5.9.11, otherwise nixpkgs minor
        # version will be used
        linux_5_9 = overlayKernel "5.9";
      })
  ];

  systemd.services = {
    # not necessary for every model
    # https://github.com/linux-surface/linux-surface/wiki/Known-Issues-and-FAQ#sleep-script
    # only for kernels running original IPTS firmware
    surface-sleep = {
      enable = lib.versionOlder config.boot.kernelPackages.kernel.version "5.4";
      before = [ "suspend.target" ];
      wantedBy = [ "suspend.target" ];
      serviceConfig.Type = "oneshot";
      path = with pkgs; [ procps kmod bluez ];
      script = ''
        # Disable bluetooth if no device is connected
        if ps cax | grep bluetoothd && ! bluetoothctl info; then
          bluetoothctl power off
        fi

        ## If you have spontaneous wakeups, you may want to disable
        ## bluetooth completely, regardless if any devices are connected or not.
        ## Note that you may be required to re-connect your devices after resume
        ## if you choose this change.
        # if ps cax | grep bluetoothd; then
        #   bluetoothctl power off
        # fi
      '';
    };
    surface-wake = {
      enable = lib.versionOlder config.boot.kernelPackages.kernel.version "5.4";
      after = [ "post-resume.target" ];
      wantedBy = [ "post-resume.target" ];
      serviceConfig.Type = "oneshot";
      path = with pkgs; [ procps kmod bluez ];
      script = ''
        # Restart bluetooth
        if ps cax | grep bluetoothd; then
          bluetoothctl power on
        fi
      '';
    };

    /* not necessary for every model. kernels >= 5.5 only, see
       https://github.com/linux-surface/linux-surface/wiki/Known-Issues-and-FAQ#aspm-related-issue
       disables some power-saving states for stability at the cost of battery.
       when upgrading to >= 5.5 from 4.x, nixos tries to immediately run this
       since multi-user.target has already been reached. it fails because the
       file doesn't exist. to avoid, use nixos-rebuild boot, not switch. you
       have to reboot for the new kernel anyway
    */
    surface-aspm = {
      enable =
        lib.versionAtLeast config.boot.kernelPackages.kernel.version "5.5";
      description =
        "Disable power-saving states for stability in Surface models";
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "oneshot";
      # the device ID and states that need to be disabled may change per device
      script = ''
        echo 0 | tee /sys/bus/pci/drivers/mwifiex_pcie/0000:01:00.0/link/l1_2_aspm
      '';
    };

    surface-iptsd = {
      enable =
        lib.versionAtLeast config.boot.kernelPackages.kernel.version "5.4";
      description = "Intel Precise Touch & Stylus Daemon";
      documentation = [ "https://github.com/linux-surface/iptsd" ];
      after = [ "dev-ipts-0.device" ];
      wants = [ "dev-ipts-0.device" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig.Type = "simple";
      path = [ pkgs.iptsd ];
      script = ''
        iptsd
      '';
    };
  };

  # # not working with meson flag -Dsystemd=true
  # systemd.packages = [ pkgs.iptsd ];
  # services.udev.packages = [ pkgs.iptsd ];
  services.udev.extraRules = ''
    # iptsd
    KERNEL=="ipts/*", TAG+="systemd";
  '';

  environment.etc."ipts.conf".text = ''
    [Config]
    # BlockOnPalm = false
    # TouchThreshold = 10
    # StabilityThreshold = 0.1
    #
    ## The following values are device specific
    ## and will be loaded from /usr/share/ipts
    ##
    ## Only set them if you need to provide custom
    ## values for new devices that are not yet supported
    #
    # InvertX = false
    # InvertY = false
    # Width = 0
    # Height = 0
  '';
}
