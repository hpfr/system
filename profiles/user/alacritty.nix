{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.alacritty;
in {
  options.profiles.user.alacritty.enable = mkEnableOption ''
    Include my Alacritty config.
  '';

  config = mkIf cfg.enable {
    home.sessionVariables.TERMINAL = "alacritty";

    programs.alacritty = {
      enable = true;
      settings = {
        window.padding = {
          x = 8;
          y = 8;
        };
        background_opacity = 0.85;
        key_bindings = [
          {
            key = "C";
            mods = "Control|Shift";
            action = "Copy";
          }
          {
            key = "V";
            mods = "Control|Shift";
            action = "Paste";
          }
        ];
      };
    };

  };
}
