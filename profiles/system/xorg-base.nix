{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.xorg-base;
in {
  options.profiles.system.xorg-base.enable =
    mkEnableOption "my system-level Xorg GUI base configuration";

  config = mkIf cfg.enable {
    profiles.system.gui-base.enable = true;

    services = {
      xserver = {
        enable = true;
        layout = "us";
        libinput.enable = true;
        displayManager.lightdm.enable = true;
        desktopManager.xterm.enable = false;
        # TODO: test how much of this is necessary with home-manager
        windowManager.i3 = {
          enable = true;
          package = pkgs.i3-gaps;
        };
      };

      redshift = {
        enable = true;
        temperature.night = 3000;
      };
    };
  };
}
