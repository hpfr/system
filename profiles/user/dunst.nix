{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.dunst;
in {
  options.profiles.user.dunst.enable = mkEnableOption "my dunst configuration";

  config = mkIf cfg.enable {
    services.dunst = {
      enable = true;
      # https://github.com/dunst-project/dunst/blob/master/dunstrc
      settings = {
        global = {
          alignment = "left";
          follow = "keyboard";
          frame_width = 1;
          geometry = let
            barHeight = config.services.polybar.config."bar/main".height;
            gap = barHeight / 2;
          in "500x3-${toString gap}+${toString (barHeight + gap)}";
          shrink = true;
          padding = 5;
          separator_color = "#383838";
          frame_color = "#383838";
          word_wrap = true;
          font = "Monospace 10";
        };
        experimental.per_monitor_dpi = true;
        urgency_low = {
          background = "#282828";
          foreground = "#aaaaaa";
          timeout = 5;
        };
        urgency_normal = {
          background = "#282828";
          foreground = "#eeeeee";
          timeout = 10;
        };
        urgency_critical = {
          background = "#282828";
          foreground = "#e9cbbd";
          timeout = 0;
        };
      };
    };
  };
}
