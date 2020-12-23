{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gnome;
in {
  options.profiles.system.gnome.enable =
    mkEnableOption "my system-level GNOME base configuration";

  config = mkIf cfg.enable {
    profiles.system.wayland-base.enable = true;
    services.xserver.desktopManager.gnome3.enable = true;
    environment = {
      systemPackages = with pkgs; [ gnome3.gnome-tweaks ];
      gnome3.excludePackages = with pkgs.gnome3; [
        gedit
        totem
        gnome-music
        simple-scan
      ];
    };
  };
}
