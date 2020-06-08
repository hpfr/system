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
  };
}
