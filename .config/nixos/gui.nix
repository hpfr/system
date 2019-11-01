{ config, pkgs, ... }:

{
  imports = [ ./base.nix ];

  boot.kernelModules =
    [ "uinput" ]; # hw.steam-hw should do this automatically soon
  location.provider = "geoclue2"; # for redshift

  fonts.enableDefaultFonts = false;
  fonts.fonts = with pkgs; [
    google-fonts
    nerdfonts # warning: downloads almost 2 GiB
    joypixels # emoji
  ];

  # fonts.fontconfig.penultimate.enable = true;
  fonts.fontconfig.defaultFonts = {
    serif = [ "Noto Serif" ];
    sansSerif = [ "Lato" ];
    monospace = [ "Hasklug Nerd Font" ];
    emoji = [ "JoyPixels" ];
  };

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
      # steam in-home streaming
      27036
      27037
    ];
    allowedTCPPortRanges = [{
      # steam login and download
      from = 27015;
      to = 27030;
    }];
    allowedUDPPorts = [
      # steam in-home streaming
      27031
      27036
      # steam client?
      4380
    ];
    allowedUDPPortRanges = [
      # steam login and download
      {
        from = 27015;
        to = 27030;
      }
      # steam game traffic
      {
        from = 27000;
        to = 27100;
      }
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

    redshift = {
      enable = true;
      temperature.night = 3000;
    };

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
      fltrdr # speedreader TUI

      sxhkd # wm agnostic keybindings for X
      xorg.xwininfo # query window information
      xorg.xprop # query window properties
      xorg.xdpyinfo # get info like DPI
      xdotool # manage windows in scripts
      xclip # manage clipboard in scripts
      libnotify # notify-send command
      libxml2 # xmllint for rofi-emoji
      imagemagick7 # image editing CLI and GUI
      adwaita-qt # make qt apps look like gtk apps

      i3lock-fancy
      arandr # monitor layout GUI
      blueman # bluetooth GUI
      pavucontrol # pulseaudio GUI
      wpgtk # gtk GUI
      networkmanager_dmenu # connect to wifi from rofi
      rofi-systemd # manage services with rofi
      sxiv # simple x image viewer
      maim # lightweight screenshot utility

      celluloid # mpv gtk frontend
      safeeyes # reminds user on eye health
      virtmanager # manage server VM's remotely
      # x11_ssh_askpass # fill ssh password requests
      # libreoffice # office suite. bloated, especially for surface
      keepassxc # password manager
      xournalpp # handwritten notes and PDF markup
      fractal # matrix gtk client

      sc-controller # use steam controller without steam
      steam
      # wine # wine is not an emulator
      protontricks # for problematic Steam Play games
    ];

    xsession = {
      enable = true;
      initExtra = ''
        [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
        # setbg & # set background
        sxhkd &
        xset r rate 300 50 &	# faster hold key repeat rate
        # mpd >/dev/null 2>&1 &
        safeeyes &
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

      pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
      };
    };

    fonts.fontconfig.enable = true;

    gtk = {
      enable = true;
      theme.name = "Adwaita";
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita-icons";
      };
      font.name = "Sans 10";
      gtk3.extraConfig = {
        gtk-application-prefer-dark-theme = true;
        gtk-toolbar-style = "GTK_TOOLBAR_ICONS";
        gtk-toolbar-icon-size = "GTK_ICON_SIZE_LARGE_TOOLBAR";
        gtk-button-images = 0;
        gtk-menu-images = 1;
        gtk-enable-event-sounds = 1;
        gtk-enable-input-feedback-sounds = 1;
        gtk-xft-antialias = 1;
        gtk-xft-hinting = 1;
        gtk-xft-hintstyle = "hintfull";
        gtk-xft-rgba = "rgb";
      };
    };

    # qt apps quit with "No GSettings schemas are installed on the system"
    # qt = {
    #   enable = true;
    #   platformTheme = "gnome"; # use gtk file chooser, etc with qt apps
    # };

    programs = {
      alacritty = { enable = true; };

      bash = {
        profileExtra = ''
          # autostart graphical server on tty1 login
          [ "$(tty)" = "/dev/tty1" ] && ! pgrep -x X >/dev/null && exec startx
        '';
      };

      firefox = {
        enable = true;
        profiles.default = {
          name = "default";
          settings = {
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            # https://reddit.com/r/firefox/comments/bcph6f/dark_about_pages_now_available_in_nightly/
            "browser.in-content.dark-mode" = true;
            "ui.systemUsesDarkTheme" = 1;
          };
          userChrome =
            builtins.readFile /home/lh/.config/firefox/userChrome.css;
        };
        profiles.clean = {
          name = "clean";
          id = 1;
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

      mpv.enable = true;
    };

    services = {
      compton = {
        enable = true;
        fade = true;
        fadeDelta = 4;
        # inactiveOpacity = "0.9";
      };

      dunst.enable = true;

      unclutter.enable = true;

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
