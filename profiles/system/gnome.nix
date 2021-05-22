{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gnome;
in {
  options.profiles.system.gnome.enable =
    mkEnableOption "my system-level GNOME base configuration";

  config = mkIf cfg.enable {
    profiles.system.wayland-base.enable = true;
    services.xserver.desktopManager.gnome.enable = true;

    # Disable gnome-keyring entirely in favor of KeePassXC
    services.gnome.gnome-keyring.enable = lib.mkForce false;

    qt5 = {
      style = "adwaita-dark";
      platformTheme = "gnome";
    };

    environment = {
      systemPackages = with pkgs; [ gnome.gnome-tweaks ];
      gnome.excludePackages = with pkgs.gnome; [
        gedit
        totem
        gnome-music
        simple-scan
      ];
    };
  };
}
