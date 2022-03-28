{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.wayland-base;
in {
  options.profiles.user.wayland-base.enable =
    mkEnableOption "my user-level Wayland base configuration";
  config = mkIf cfg.enable {
    profiles.user = {
      gui-base.enable = true;
      foot.enable = lib.mkDefault true;
    };

    systemd.user.services.bgcron.Service.Environment = mkDefault "PATH=${
        with pkgs;
        lib.makeBinPath [ coreutils libnotify swaybg xdg-user-dirs ]
      }";

    # # numerous QT apps can't handle this
    # home.sessionVariables.QT_QPA_PLATFORM = "wayland";

    home.packages = [ pkgs.wl-clipboard ];
  };
}
