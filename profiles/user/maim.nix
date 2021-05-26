{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.maim;
in {
  options.profiles.user.maim.enable = mkEnableOption "my maim configuration";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.maim ];
    # https://github.com/naelstrof/maim/issues/172
    services.picom.blurExclude = [ "class_g = 'slop'" ];
    services.sxhkd.keybindings = {
      "Print" =
        ''maim ~/documents/pictures/"$(date '+%F-%Hh%Mm%S')"-screenshot.png'';
      "shift + Print" =
        if config.profiles.user.rofi.enable then "maimpick" else null;
    };
  };
}
