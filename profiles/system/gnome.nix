{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gnome;
in {
  options.profiles.system.gnome.enable =
    mkEnableOption "my system-level GNOME base configuration";

  config = mkIf cfg.enable {
    profiles.system.wayland-base.enable = true;
    services = {
      xserver.desktopManager.gnome.enable = true;

      gnome = {
        # Disable gnome-keyring entirely in favor of KeePassXC
        gnome-keyring.enable = lib.mkForce false;
        gnome-remote-desktop.enable = true;
      };
    };

    # TODO: remove when this happens https://github.com/NixOS/nixpkgs/pull/163075
    hardware.pulseaudio.enable = false;

    qt5 = {
      style = "adwaita-dark";
      platformTheme = "gnome";
    };

    programs.kdeconnect = {
      enable = true;
      package = pkgs.gnomeExtensions.gsconnect;
    };

    environment = {
      systemPackages = with pkgs; [
        gnome.gnome-tweaks
        gnome.nautilus-python # gsconnect nautilus integration
      ];
      gnome.excludePackages = with pkgs.gnome; [
        epiphany # firefox, chromium
        gedit # emacs for text editing
        geary # emacs for mail
        totem # mpv/celluloid for media
        gnome-music # spotify/mpd+emacs
        simple-scan # I don't have a scanner
        gnome-terminal # gnome console
        pkgs.gnome-photos # gthumb actually lets you browse directories
      ];
    };
  };
}
