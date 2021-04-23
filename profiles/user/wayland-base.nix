{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.wayland-base;
in {
  options.profiles.user.wayland-base.enable =
    mkEnableOption "my user-level Wayland base configuration";
  config = mkIf cfg.enable {
    profiles.user.gui-base.enable = true;

    # # numerous QT apps can't handle this
    # home.sessionVariables.QT_QPA_PLATFORM = "wayland";

    programs.firefox.package = pkgs.firefox-wayland;
  };
}
