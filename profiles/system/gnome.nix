{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gnome;
in {
  options.profiles.system.gnome.enable =
    mkEnableOption "my system-level GNOME base configuration";

  config = mkIf cfg.enable {
    profiles.system.wayland-base.enable = true;
    services.xserver.desktopManager.gnome3.enable = true;

    # Disable gnome-keyring entirely in favor of KeePassXC
    services.gnome3.gnome-keyring.enable = lib.mkForce false;

    qt5 = {
      style = "adwaita-dark";
      platformTheme = "gnome";
    };

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
