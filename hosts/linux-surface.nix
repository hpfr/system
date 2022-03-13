{ config, lib, pkgs, ... }:

{
  nixpkgs.overlays = [
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
  };
}
