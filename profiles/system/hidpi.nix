{ config, lib, pkgs, ... }:

with lib;
let cfg = config.profiles.system.hidpi;
in {
  options.profiles.system.hidpi.enable = mkEnableOption "hiDPI configuration";

  config = mkIf cfg.enable {
    console.font = "latarcyrheb-sun32";

    services.xserver.dpi = 192; # doesn't work with startx
    fonts.fontconfig.dpi = 192;

    environment.variables = {
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.5";
      QT_AUTO_SCREEN_SCALE_FACTOR = "1";
    };
  };
}
