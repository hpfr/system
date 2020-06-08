{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.sway;
in {
  options.profiles.system.sway.enable =
    mkEnableOption "my system-level Sway base configuration";

  config = mkIf cfg.enable {
    profiles.system.wayland-base.enable = true;

    services.xserver.displayManager.sessionPackages = [ pkgs.sway ];

    programs.sway = {
      enable = true;
      extraPackages = with pkgs; [
        swaybg
        swayidle
        swaylock
        wl-clipboard
        xwayland
      ];
      wrapperFeatures.gtk = true;
    };
  };
}
