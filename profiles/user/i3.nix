{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.i3;
in {
  options.profiles.user.i3.enable = mkEnableOption ''
    Include my i3 config.
  '';

  config = mkIf cfg.enable {

    xsession.windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      config = {
        bars = [ ];
        # https://github.com/rycee/home-manager/issues/195
        startup = [{
          command = "systemctl --user restart polybar";
          always = true;
          notification = false;
        }];
        window = {
          hideEdgeBorders = "smart";
          commands = [
            # {
            #   criteria.window_role = "GtkFileChooserDialog";
            #   command = "resize set 800 600; move position center";
            # }
            {
              criteria.class = "^Spotify$";
              command =
                "move scratchpad; resize set 1600 1000; move position center; scratchpad show";
            }
            {
              criteria = {
                class = "^KeePassXC$";
                title = " - KeePassXC$";
              };
              command = "resize set 1600 1000; move position center";
            }
          ];
        };
        floating.criteria = [
          { "title" = "(Dropdown)"; }
          { "title" = "Steam - Update News"; }
          { "title" = "Steam Keyboard"; }
          { "class" = "^KeePassXC$"; }
        ];
        # not released yet
        # workspaceAutoBackAndForth = true;
        gaps = {
          inner = 0;
          outer = 0;
          # mouseWarping = false;
          # smartBorders = "no_gaps";
          # smartGaps = true;
        };
        modifier = "Mod4";
        keybindings =
          let mod = config.xsession.windowManager.i3.config.modifier;
          in lib.mkOptionDefault {
            # unbind keys handled by sxhkd
            "${mod}+Return" = null;
            "${mod}+a" = null;
            "${mod}+d" = null;
            "${mod}+v" = null;
            "${mod}+s" = null;
            "${mod}+w" = null;
            "${mod}+e" = null;
            "${mod}+r" = null;
            "${mod}+Shift+c" = null;
            "${mod}+Shift+r" = null;
            "${mod}+Shift+e" = null;
            "${mod}+0" = "workspace 0";
            "${mod}+Shift+0" = "move container to workspace 0";

            "${mod}+q" = "kill";
            "${mod}+Shift+q" =
              "exec --no-startup-id kill -9 $(xdotool getwindowfocus getwindowpid)";

            "${mod}+t" = "split toggle";
            "${mod}+o" = "sticky toggle";
            # after switching to tabbed, mod+t to go back to split
            "${mod}+Shift+t" = "layout tabbed";

            "${mod}+g" = "workspace prev";
            "${mod}+semicolon" = "workspace next";
            "${mod}+Tab" = "workspace back_and_forth";
            "${mod}+backslash" = "workspace back_and_forth";
            "${mod}+minus" = "scratchpad show";

            "${mod}+Shift+b" =
              "floating toggle; sticky toggle; exec --no-startup-id hover left";
            "${mod}+Shift+n" =
              "floating toggle; sticky toggle; exec --no-startup-id hover right";

            "${mod}+h" = "focus left";
            "${mod}+Shift+h" = "move left 30";
            "${mod}+j" = "focus down";
            "${mod}+Shift+j" = "move down 30";
            "${mod}+k" = "focus up";
            "${mod}+Shift+k" = "move up 30";
            "${mod}+l" = "focus right";
            "${mod}+Shift+l" = "move right 30";

            "${mod}+Shift+y" = "exec --no-startup-id i3resize left";
            "${mod}+Shift+u" = "exec --no-startup-id i3resize down";
            "${mod}+Shift+i" = "exec --no-startup-id i3resize up";
            "${mod}+Shift+o" = "exec --no-startup-id i3resize right";

            "${mod}+Ctrl+h" = "move workspace to output left";
            "${mod}+Ctrl+j" = "move workspace to output down";
            "${mod}+Ctrl+k" = "move workspace to output up";
            "${mod}+Ctrl+l" = "move workspace to output right";
            "${mod}+Ctrl+Left" = "move workspace to output left";
            "${mod}+Ctrl+Down" = "move workspace to output down";
            "${mod}+Ctrl+Up" = "move workspace to output up";
            "${mod}+Ctrl+Right" = "move workspace to output right";

            "${mod}+Home" = "workspace 1";
            "${mod}+Shift+Home" = "move container to workspace 1";
            "${mod}+End" = "workspace 1";
            "${mod}+Shift+End" = "move container to workspace 1";

            "${mod}+F2" = "restart";
            "${mod}+Shift+z" =
              ''exec --no-startup-id prompt "Exit i3?" "i3-msg exit"'';
          };
      };
    };
  };
}
