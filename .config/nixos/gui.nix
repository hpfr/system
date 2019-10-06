{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  boot.kernelModules = [ "uinput" ]; # hw.steam-hw should do this automatically soon
  location.provider = "geoclue2"; # for redshift

  fonts.fontconfig.penultimate.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware = {
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull; # for bluetooth?
    };
    bluetooth.enable = true;
    opengl.driSupport32Bit = true; # for 32-bit games
    steam-hardware.enable = true;
  };

  networking.firewall = {
    allowedTCPPorts = [
      27036 27037 # steam in-home streaming
    ];
    allowedTCPPortRanges = [
      { from = 27015; to = 27030; } # steam login and download
    ];
    allowedUDPPorts = [
      27031 27036 # steam in-home streaming
      4380 # steam client?
    ];
    allowedUDPPortRanges = [
      { from = 27015; to = 27030; } # steam login and download
      { from = 27000; to = 27100; } # steam game traffic
    ];
  };

  services = {
    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      libinput.enable = true;
      displayManager.startx.enable = true;
      desktopManager.xterm.enable = false;
      # windowManager.i3.enable = true;
      # windowManager.i3.package = pkgs.i3-gaps;
    };

    dbus.packages = with pkgs; [ gnome3.dconf ];

    udev.extraRules = ''
      ######################################################################

      # IPTS Touchscreen (SP2017)
      SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Touchscreen", ENV{ID_INPUT_TOUCHSCREEN}="1", SYMLINK+="input/touchscreen"

      # IPTS Pen (SP2017)
      SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Pen", SYMLINK+="input/pen"

      ######################################################################
    '';

    flatpak = { # for Spotify, Discord, and Telegram
      enable = true;
    };
  };

  xdg.portal = { # for Flatpak
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # create group for steam controller
  # users.groups.steam-input = {};

  home-manager.users.lh = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      mpc_cli # mpd CLI
      pulsemixer # pulseaudio TUI

      google-fonts
      nerdfonts # warning: downloads almost 2 GiB
      emojione # emoji

      sxhkd # wm agnostic keybindings for X
      xorg.xwininfo # query window information
      xorg.xprop # query window properties
      xorg.xdpyinfo # get info like DPI
      xdotool # manage windows in scripts
      xclip # manage clipboard in scripts

      i3lock-fancy
      arandr # monitor layout GUI
      blueman # bluetooth GUI
      pavucontrol # pulseaudio GUI
      wpgtk # gtk GUI
      networkmanager_dmenu # connect to wifi from rofi
      rofi-systemd # manage services with rofi
      sxiv # simple x image viewer
      maim # lightweight screenshot utility

      virtmanager # manage server VM's remotely
      # x11_ssh_askpass # fill ssh password requests
      libreoffice # office suite
      keepassxc # password manager
      xournalpp # handwritten notes and PDF markup
      fractal # matrix gtk client

      steam
      wine # wine is not an emulator
    ];

    xsession = {
      enable = true;
      initExtra = ''
        # setbg &		# Set the background
        [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
        sxhkd &	# Bind keys
        xset r rate 300 50 &	# Speed xrate up
        mpdupdate &
        emacs &
      '';

      windowManager = {
        # command = "exec ${pkgs.i3}/bin/i3";
        i3 = {
          enable = true;
          package = pkgs.i3-gaps;
          config = null;
          extraConfig = builtins.readFile /home/lh/.config/i3/config-nix;
        };
      };
    };

    fonts.fontconfig.enable = true;

    gtk = {
      enable = true;
      theme = {
        package = pkgs.arc-theme;
        name = "arc";
      };
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "adwaita-icons";
      };
      font.name = "Sans 10";
      gtk3.extraConfig = {
        gtk-cursor-theme-size = 0;
        gtk-toolbar-style = "GTK_TOOLBAR_TEXT";
        gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
        gtk-button-images = 0;
        gtk-menu-images = 1;
        gtk-enable-event-sounds = 1;
        gtk-enable-input-feedback-sounds = 1;
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintfull";
        gtk-xft-rgba = "rgb";
        gtk-cursor-theme-name = "Adwaita";
      };
    };

    programs = {
      alacritty = {
        enable = true;
      };

      firefox = {
        enable = true;
        profiles.default = {
          name = "default";
          userChrome = builtins.readFile /home/lh/.config/firefox/userChrome.css;
        };
      };

      rofi = {
        enable = true;
        theme = "Arc-Dark";
        extraConfig = ''
          rofi.modi: window,run,ssh,drun,combi
          rofi.combi-modi: window,drun
        '';
      };

      zathura = {
        enable = true;
        extraConfig = builtins.readFile /home/lh/.config/zathura/nix;
      };

      mpv = {
        enable = true;
      };
    };

    services = {
      compton = {
        enable = true;
        fade = true;
        fadeDelta = 4;
        inactiveOpacity = "0.9";
      };

      dunst = {
        enable = true;
      };

      unclutter.enable = true;
      xcape.enable = true;

      polybar = {
        enable = true;
        package = pkgs.polybar.override {
          mpdSupport = true;
          pulseSupport = true;
          i3GapsSupport = true;
        };
        extraConfig = builtins.readFile /home/lh/.config/polybar/nix.conf;
        script = "";
      };
    };
  };
}
