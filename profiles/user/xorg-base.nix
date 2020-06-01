{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.xorg-base;
in {
  options.profiles.user.xorg-base.enable =
    mkEnableOption "my user-level Xorg GUI base configuration";

  config = {
    profiles.user = {
      gui-base.enable = true;

      # desktop environment
      i3.enable = true;
      rofi.enable = true;
      polybar.enable = true;
      dunst.enable = true;
      maim.enable = true;
    };

    home.packages = with pkgs; [
      sxhkd # wm agnostic keybindings for X

      # TODO package scripts with these dependencies
      xorg.xwininfo # query window information
      xorg.xprop # query window properties
      xorg.xdpyinfo # get info like DPI
      xdotool # manage windows in scripts
      xclip # manage clipboard in scripts
      libnotify # notify-send command
      libxml2 # xmllint for rofi-emoji

      # nixpkgs overlays not recognized on first build?
      xwallpaper # set wallpaper

      i3lock-fancy # screen locker. nothing about i3lock seems to depend on i3
      arandr # monitor layout GUI

      tdrop # WM- and TE-agnostic dropdown terminal panes

      barrier # software KVM. no wayland yet

      gui-scripts
    ];

    systemd.user = {
      timers.bgcron = {
        Unit.After = [ "graphical.target" ];
        Timer = {
          OnCalendar = "daily";
          Unit = "bgcron.service";
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
      services.bgcron = {
        Unit = {
          After = [ "graphical.target" ];
          Wants = [ "bgcron.timer" ];
        };
        Service = {
          Environment = "PATH=${
              with pkgs;
              lib.makeBinPath [ coreutils libnotify xwallpaper ]
            }";
          ExecStart = "${pkgs.gui-scripts}/bin/setbg";
        };
      };
    };

    xsession = {
      enable = true;
      initExtra = ''
        [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
        setbg ~/.config/wall # set background
        sxhkd &
        xset r rate 300 50 &	# faster hold key repeat rate
        # mpd >/dev/null 2>&1 &
        safeeyes &
        # acts as ssh agent and secret service
        keepassxc &
      '';

      pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
      };
    };

    # TODO: https://github.com/rycee/home-manager/pull/847
    xdg.configFile."sxhkd/sxhkdrc".source = ./sxhkd/sxhkdrc;

    services = {
      picom = {
        enable = true;
        # https://github.com/yshui/picom/blob/next/picom.sample.conf
        fade = true;
        fadeDelta = 4;
        # inactiveOpacity = "0.9";
        blur = true;
      };

      # hide pointer
      unclutter.enable = true;
    };
  };
}
