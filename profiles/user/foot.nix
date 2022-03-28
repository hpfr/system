{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.foot;
in {
  options.profiles.user.foot.enable =
    mkEnableOption "my user-level Foot configuration";
  config = mkIf cfg.enable {
    home.sessionVariables.TERMINAL = "foot";

    programs.foot = {
      enable = true;
      settings = {
        main.font = "monospace:size=11";
        csd = { # client-side decorations (title bar size)
          size = 32;
          button-width = 32;
        };
        colors.alpha = 0.9;
      };
    };
  };
}
