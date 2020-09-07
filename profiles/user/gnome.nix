{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gnome;
in {
  options.profiles.user.gnome.enable =
    mkEnableOption "my user-level GNOME configuration";
  config = mkIf cfg.enable {
    profiles.user.wayland-base.enable = true;
    # declaratively manage gsettings
    dconf.enable = true;

    # Disable gnome-keyring ssh-agent in favor of the default agent
    xdg.configFile."autostart/gnome-keyring-ssh.desktop".text = ''
      ${lib.fileContents
      "${pkgs.gnome3.gnome-keyring}/etc/xdg/autostart/gnome-keyring-ssh.desktop"}
      Hidden=true
    '';
  };
}
