{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.wayland-base;
in {
  options.profiles.user.wayland-base.enable =
    mkEnableOption "my user-level Wayland base configuration";
  config = mkIf cfg.enable {
    profiles.user.gui-base.enable = true;

    systemd.user.services.bgcron.Service.Environment = mkDefault "PATH=${
        with pkgs;
        lib.makeBinPath [ coreutils libnotify swaybg xdg-user-dirs ]
      }";

    # # numerous QT apps can't handle this
    # home.sessionVariables.QT_QPA_PLATFORM = "wayland";
    home.sessionVariables = {
      MOZ_DBUS_REMOTE = 1;
      TERMINAL = "foot";
    };
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
