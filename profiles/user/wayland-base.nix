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
    home.packages = [ pkgs.wl-clipboard ];

    programs.firefox.package = pkgs.firefox-wayland;

    # minimal terminal emulator
    programs.foot = {
      enable = true;
      settings = {
        main = {
          term = "xterm-256color";
          font = "monospace:size=9";
        };
        csd = { # client-side decorations (title bar size)
          size = 32;
          button-width = 32;
        };
      };
    };
  };
}
