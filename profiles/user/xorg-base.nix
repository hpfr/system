{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.xorg-base;
in {
  options.profiles.user.xorg-base.enable =
    mkEnableOption "my user-level Xorg GUI base configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      gui-base.enable = true;
      alacritty.enable = true; # terminal emulator

      # pseudo desktop environment
      i3.enable = true;
      sxhkd.enable = true;
      autorandr.enable = true;
      rofi.enable = true;
      polybar.enable = true;
      dunst.enable = true;
      maim.enable = true;
    };

    home.packages = with pkgs; [
      # TODO: package scripts with these dependencies
      xorg.xwininfo # query window information
      xorg.xprop # query window properties
      xorg.xdpyinfo # get info like DPI
      xdotool # manage windows in scripts
      xclip # manage clipboard in scripts
      libnotify # notify-send command
      libxml2 # xmllint for rofi-emoji

      xwallpaper # set wallpaper

      i3lock-fancy # screen locker. nothing about i3lock seems to depend on i3
      arandr # monitor layout GUI

      tdrop # WM- and TE-agnostic dropdown terminal panes

      barrier # software KVM. no wayland yet

      gui-scripts
    ];

    systemd.user.services.bgcron.Service.Environment = "PATH=${
        with pkgs;
        lib.makeBinPath [ coreutils libnotify xwallpaper xdg-user-dirs ]
      }";

    xsession = {
      enable = true;
      # TODO: make systemd services
      initExtra = ''
        [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
        setbg ~/.config/wall # set background
        xset r rate 300 50 & # faster hold key repeat rate
        # acts as ssh agent and secret service
        keepassxc &
      '';

      pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
        size = 32;
      };
    };

    services = {
      picom = {
        enable = true;
        # https://github.com/yshui/picom/blob/next/picom.sample.conf
        fade = true;
        fadeDelta = 4;
        # inactiveOpacity = "0.9";
        settings = {
          blur = {
            method = "gaussian";
            size = 10;
            deviation = 5.0;
          };
        };
      };

      # hide pointer
      unclutter.enable = true;
    };

    programs.autorandr.hooks.postswitch."reset-background" =
      "${pkgs.gui-scripts}/bin/setbg ~/.config/wall";
  };
}
