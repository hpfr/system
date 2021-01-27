{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.i3;
in {
  options.profiles.user.i3.enable = mkEnableOption "my i3 configuration";

  config = mkIf cfg.enable {
    xsession.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      config = mkMerge [
        (import ../../lib/i3-sway.nix { inherit config lib; })
        {
          startup = [{
            command = "systemctl --user restart polybar";
            always = true;
            notification = false;
          }];
          keybindings =
            let mod = config.xsession.windowManager.i3.config.modifier;
            in lib.mkOptionDefault {
              # unbind keys handled by sxhkd
              "${mod}+Return" = null;
              "${mod}+d" = null;

              "${mod}+Shift+q" =
                "exec kill -9 $(xdotool getwindowfocus getwindowpid)";

              "${mod}+Shift+b" =
                "floating toggle; sticky toggle; exec --no-startup-id hover left";
              "${mod}+Shift+n" =
                "floating toggle; sticky toggle; exec --no-startup-id hover right";

              "${mod}+Shift+y" = "exec --no-startup-id i3resize left";
              "${mod}+Shift+u" = "exec --no-startup-id i3resize down";
              "${mod}+Shift+i" = "exec --no-startup-id i3resize up";
              "${mod}+Shift+o" = "exec --no-startup-id i3resize right";

              "${mod}+Shift+z" = "exec prompt 'Exit i3?' 'i3-msg exit'";
            };
        }
      ];
    };

    programs.autorandr.hooks.postswitch."restart-i3" =
      "${pkgs.i3}/bin/i3-msg restart";
  };
}
