{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.mpv;
in {
  options.profiles.user.mpv.enable = mkEnableOption "my mpv configuration";

  config = mkIf cfg.enable {
    programs.mpv = {
      enable = true;
      bindings = {
        h = "seek -5";
        j = "seek -60";
        k = "seek 60";
        l = "seek 5";

        # rebind lost l binding, matches across from L which loops whole file
        H = "ab-loop";
        # rebind lost j binding, move J to K
        J = "cycle sub";
        K = "cycle sub down";
      };
    };
  };
}
