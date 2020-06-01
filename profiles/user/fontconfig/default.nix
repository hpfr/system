{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.fontconfig;
in {
  options.profiles.user.fontconfig.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    # TODO: improve home-manager fontconfig module?
    xdg.configFile."fontconfig" = {
      source = ./files;
      recursive = true;
    };
  };
}
