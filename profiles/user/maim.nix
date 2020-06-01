{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.maim;
in {
  options.profiles.user.maim.enable = mkEnableOption "my maim configuration";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.maim ];
    # https://github.com/naelstrof/maim/issues/172
    services.picom.blurExclude = [ "class_g = 'slop'" ];
  };
}
