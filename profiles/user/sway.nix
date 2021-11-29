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

    home.packages = with pkgs; [ wofi wdisplays ];

    wayland.windowManager.sway = {
      enable = true;
      config = mkMerge [
        (import ../../lib/i3-sway.nix { inherit config lib; })
        {
          keybindings =
            let mod = config.wayland.windowManager.sway.config.modifier;
            in lib.mkOptionDefault {
              # "${mod}+Shift+q" = "really kill";

              "${mod}+d" = "exec wofi --show drun";

              "${mod}+z" = "exec ${pkgs.swaylock}/bin/swaylock -f";
              "${mod}+Shift+z" =
                "exec ${pkgs.gui-scripts}/bin/prompt 'Exit sway?' 'swaymsg exit'";
              "${mod}+x" =
                "exec ${pkgs.gui-scripts}/bin/prompt 'Shutdown computer?' 'shutdown -h now'";
              "${mod}+shift+x" =
                "exec ${pkgs.gui-scripts}/bin/prompt 'Reboot computer?' 'reboot'";

              "Print" = ''
                exec ${pkgs.grim}/bin/grim -t png -g "$(${pkgs.slurp}/bin/slurp)" - | ${pkgs.wl-clipboard}/bin/wl-copy -t image/png'';

              "XF86AudioMicMute" =
                "exec ${pkgs.ponymix}/bin/ponymix --source toggle";
              "XF86AudioMute" = "exec ${pkgs.ponymix}/bin/ponymix toggle";
              "XF86AudioLowerVolume" =
                "exec ${pkgs.ponymix}/bin/ponymix decrease 5";
              "XF86AudioRaiseVolume" =
                "exec ${pkgs.ponymix}/bin/ponymix increase 5";

              "XF86AudioPlay" =
                "exec ${pkgs.playerctl}/bin/playerctl play-pause";
              "XF86AudioPrev" = "exec ${pkgs.playerctl}/bin/playerctl previous";
              "XF86AudioNext" = "exec ${pkgs.playerctl}/bin/playerctl next";

              "XF86MonBrightnessDown" =
                "exec ${pkgs.brillo}/bin/brillo -e -U 0.5";
              "XF86MonBrightnessUp" =
                "exec ${pkgs.brillo}/bin/brillo -e -A 0.5";
            };
        }
      ];
    };
  };
}
