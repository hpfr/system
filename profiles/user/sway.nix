{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.sway;
in {
  options.profiles.user.sway.enable = mkEnableOption "my Sway configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      wayland-base.enable = true;
      waybar.enable = true;
    };

    home.packages = with pkgs; [ wofi ];

    wayland.windowManager.sway = {
      enable = true;
      config = mkMerge [
        (import ../../lib/i3-sway.nix { inherit config lib; })
        {
          keybindings =
            let mod = config.wayland.windowManager.sway.config.modifier;
            in lib.mkOptionDefault {
              "${mod}+d" = "exec --no-startup-id wofi --show drun";

              "${mod}+Shift+z" = "exec prompt 'Exit sway?' 'swaymsg exit'";
            };
        }
      ];
    };
  };
}
