{ config, pkgs, ... }:

{
  imports = [ ./base.nix ];

  nixpkgs.overlays = [
    # not sure why this isn't the default, KPXC has it as their default
    (self: super: {
      keepassxc = super.keepassxc.override { withKeePassNetworking = true; };
    })
    (self: super: { xwallpaper = super.callPackage ./pkgs/xwallpaper { }; })
    (self: super: {
      gui-scripts = (super.runCommand "gui-scripts" {
        preferLocalBuild = true;
        allowSubstitutes = false;
      } ''
        shopt -s globstar
        for tool in ${./bin/gui}"/"**; do
          [ -f $tool ] && install -D -m755 $tool $out/bin/$(basename $tool)
        done
        patchShebangs $out/bin
      '');
    })
  ];

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
    };

    dbus.packages = with pkgs; [ gnome3.dconf ];

    udev.extraRules = ''
      # UDEV Rules for OnlyKey, https://docs.crp.to/linux.html
      #
      ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{ID_MM_DEVICE_IGNORE}="1"
      ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{MTP_NO_PROBE}="1"
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
      KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
      #
      # If you share your linux system with other users, or just don't like the
      # idea of write permission for everybody, you can replace MODE:="0666" with
      # OWNER:="yourusername" to create the device owned by you, or with
      # GROUP:="somegroupname" and mange access using standard unix groups.
      #
      # One requirement of TOTP (Time-based One-time Password) is having the correct
      # time. If OnlyKey is used on a system where the OnlyKey app is not running it
      # will display “NOTSET” instead of the OTP code. Because OnlyKey has no battery
      # it requires an app to send it the correct time to be able to generate TOTP
      # codes. If you have OnlyKey command-line utility installed, adding the
      # following will automatically set the current time on OnlyKey every time you
      # plug it: RUN+="/usr/local/bin/onlykey-cli settime"
      #
      # SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="/usr/local/bin/onlykey-cli settime"
      # KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="/usr/local/bin/onlykey-cli settime"
      #
      ##
    '';

    redshift = {
      enable = true;
      temperature.night = 3000;
    };

    # for proprietary apps like Spotify, Discord, and Slack
    flatpak.enable = true;
  };

  # for Flatpak
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  home-manager.users.lh = { config, pkgs, lib, ... }: {
    home = {
      packages = with pkgs; [
        sshfs

        pamixer # pulseaudio CLI (for volume keys)

        texlive.combined.scheme-medium # latex environment
        pandoc # convert document formats

        sxhkd # wm agnostic keybindings for X

        # TODO package scripts with these dependencies
        xorg.xwininfo # query window information
        xorg.xprop # query window properties
        xorg.xdpyinfo # get info like DPI
        xdotool # manage windows in scripts
        xclip # manage clipboard in scripts
        libnotify # notify-send command
        libxml2 # xmllint for rofi-emoji

        xwallpaper # set wallpaper
        imagemagick7 # image editing CLI and GUI
        adwaita-qt # make qt apps look like gtk apps

        i3lock-fancy
        arandr # monitor layout GUI
        blueman # bluetooth GUI
        pavucontrol # pulseaudio GUI
        wpgtk # gtk GUI
        networkmanager_dmenu # connect to wifi from rofi
        rofi-systemd # manage services with rofi
        nomacs # image viewer
        maim # lightweight screenshot utility
        bookworm # ebook reader

        tdrop # WM- and TE-agnostic dropdown terminal panes
        celluloid # mpv gtk frontend
        safeeyes # reminds user on eye health
        virtmanager # manage server VM's remotely
        # x11_ssh_askpass # fill ssh password requests
        # libreoffice # office suite. bloated, especially for surface
        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        riot-desktop # matrix electron client
        signal-desktop # signal client
        tdesktop # telegram client (FOSS)

        sc-controller # use steam controller without steam
        steam
        protontricks # for problematic Steam Play games
        # wine # wine is not an emulator

        gui-scripts
      ];

      sessionVariables = {
        TERMINAL = "alacritty";
        BROWSER = "firefox";
        READER = "zathura";
        # use this variable in scripts to generalize dmenu, rofi, etc
        MENU = "rofi -dmenu";
        SUDO_ASKPASS = "$HOME/.local/bin/tools/menupass";

        # GTK2_RC_FILES = "$HOME/.config/gtk-2.0/gtkrc-2.0";
        # for adwaita-qt
        QT_STYLE_OVERRIDE = "Adwaita-Dark";
      };

      file.".xinitrc".text = ''
        #!/bin/sh
        . $HOME/.xsession
      '';
    };

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
      '';

      windowManager.i3 = {
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
                criteria = {
                  class = "Firefox";
                  window_role = "PictureInPicture";
                };
                command = "sticky enable; border pixel 0";
              }
              {
                criteria.class = "^Spotify$";
                command = "move scratchpad; scratchpad show; resize 1600 1000";
              }
              {
                criteria = {
                  class = "^KeePassXC$";
                  title = " - KeePassXC$";
                };
                command = "move scratchpad; scratchpad show; resize 1200 800";
              }
            ];
          };
          floating.criteria = [
            { "title" = "(Dropdown)"; }
            { "title" = "Steam - Update News"; }
            { "title" = "Steam Keyboard"; }
            {
              "class" = "^Firefox$";
              "window_role" = "^About$";
            }
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
              "${mod}+Shift+Escape" =
                ''exec --no-startup-id prompt "Exit i3?" "i3-msg exit"'';
            };
        };
        extraConfig = ''
          no_focus [class="Firefox" window_role="PictureInPicture"]
        '';
      };

      pointerCursor = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
      };
    };

    fonts.fontconfig.enable = true;

    xdg = {
      dataFile = {
        img = {
          target = "applications/img.desktop";
          text = ''
            [Desktop Entry]
            Type=Application
            Name=Image viewer
            Exec=nomacs %u
          '';
        };
        # mail = {
        #   target = "applications/mail.desktop";
        #   text = ''
        #     [Desktop Entry]
        #     Type=Application
        #     Name=Mail
        #     Exec=emacsclient -e neomutt %u
        #   '';
        # };
        pdf = {
          target = "applications/pdf.desktop";
          text = ''
            [Desktop Entry]
            Type=Application
            Name=PDF reader
            Exec=zathura %u
          '';
        };
        # rss = {
        #   target = "applications/rss.desktop";
        #   text = ''
        #     [Desktop Entry]
        #     Type=Application
        #     Name=RSS feed addition
        #     Exec=emacsclient %U
        #   '';
        # };
        text = {
          target = "applications/text.desktop";
          text = ''
            [Desktop Entry]
            Type=Application
            Name=Text editor
            Exec=$EDITOR %u
          '';
        };
        # torrent = {
        #   target = "applications/torrent.desktop";
        #   text = ''
        #     [Desktop Entry]
        #     Type=Application
        #     Name=Torrent
        #     Exec=transadd %U
        #   '';
        # };
      };
      mimeApps = {
        enable = true;
        defaultApplications = {
          "x-scheme-handler/magnet" = "torrent.desktop;";
          "x-scheme-handler/mailto" = "mail.desktop;";
          "text/plain" = "text.desktop;";
          "text/x-shellscript" = "text.desktop;";
          "application/pdf" = "pdf.desktop;";
          "image/png" = "img.desktop;";
          "image/jpeg" = "img.desktop;";
          "image/gif" = "img.desktop;";
          "application/rss+xml" = "rss.desktop;";
          "x-scheme-handler/http" = "firefox.desktop;";
          "x-scheme-handler/https" = "firefox.desktop;";
          "x-scheme-handler/ftp" = "firefox.desktop;";
          "x-scheme-handler/chrome" = "firefox.desktop;";
          "text/html" = "firefox.desktop;";
          "application/x-extension-htm" = "firefox.desktop;";
          "application/x-extension-html" = "firefox.desktop;";
          "application/x-extension-shtml" = "firefox.desktop;";
          "application/xhtml+xml" = "firefox.desktop;";
          "application/x-extension-xhtml" = "firefox.desktop;";
          "application/x-extension-xht" = "firefox.desktop;";
        };
        associations.added = {
          "x-scheme-handler/http" = "firefox.desktop;";
          "x-scheme-handler/https" = "firefox.desktop;";
          "x-scheme-handler/ftp" = "firefox.desktop;";
          "x-scheme-handler/chrome" = "firefox.desktop;";
          "text/html" = "firefox.desktop;";
          "application/x-extension-htm" = "firefox.desktop;";
          "application/x-extension-html" = "firefox.desktop;";
          "application/x-extension-shtml" = "firefox.desktop;";
          "application/xhtml+xml" = "firefox.desktop;";
          "application/x-extension-xhtml" = "firefox.desktop;";
          "application/x-extension-xht" = "firefox.desktop;";
          "application/pdf" = "org.pwmt.zathura.desktop;xournalpp.desktop;";
        };
      };
      # TODO: this is temporary to escape the bare repo, modularize at some point
      configFile = {
        "fontconfig" = {
          source = cfg/fontconfig;
          recursive = true;
        };
        "networkmanager-dmenu/config.ini".source = cfg/nm-dmenu.ini;
        "sxhkd/sxhkdrc".source = cfg/sxhkdrc;
        "tridactyl/tridactylrc".source = cfg/tridactylrc;
      };
    };

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
      bash.profileExtra = ''
        # autostart graphical server on tty1 login
        [ "$(tty)" = "/dev/tty1" ] && ! pgrep -x X >/dev/null && exec startx
      '';

      ssh = {
        enable = true;
        matchBlocks = {
          cs = {
            hostname = "best-linux.cs.wisc.edu";
            user = "hupfer";
            # # home-manager hasn't implemented yet
            # setEnv  = { "TERM" = "xterm-256color" };
            # # CSL doesn't support key auth :(
            # identityFile = "~/.ssh/kpxc-id.pub";
            # identitiesOnly = true;
          };
          engr = {
            hostname = "best-tux.cae.wisc.edu";
            user = "liam";
            # setEnv  = { "TERM" = "xterm-256color" };
            identityFile = "~/.ssh/kpxc-id.pub";
            identitiesOnly = true;
          };
          monolith = {
            hostname = "10.10.10.9";
            user = "lh";
            identityFile = "~/.ssh/kpxc-id.pub";
            identitiesOnly = true;
          };
        };
      };

      alacritty = {
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

      firefox = {
        enable = true;
        profiles.default = {
          name = "default";
          settings = {
            "general.warnOnAboutConfig" = false;
            "browser.aboutConfig.showWarning" = false;
            "extensions.pocket.enabled" = false;
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
            # https://reddit.com/r/firefox/comments/bcph6f/dark_about_pages_now_available_in_nightly/
            "browser.in-content.dark-mode" = true;
            "ui.systemUsesDarkTheme" = 1;
            # the below option forces dark UI elements like text boxes but can break with bad webpages
            # "widget.content.allow-gtk-dark-theme" = true;
            # don't sync whether add-ons are enabled to disable some on Surface
            "services.sync.addons.ignoreUserEnabledChanges" = true;
            # open popups in new tabs, not new windows with no UI
            "browser.link.open_newwindow.restriction" = 0;
            # not officially supported yet on Nvidia proprietary drivers
            # but I haven't noticed bugs and it's much faster:
            # https://testdrive-archive.azurewebsites.net/Performance/Chalkboard/
            "gfx.webrender.all" = true;
            "gfx.webrender.enable" = true;
            # may improve perf?
            # "gfx.use-glx-texture-from-pixmap" = true;
            "extensions.formautofill.addresses.enabled" = false;
          };
          userChrome = builtins.readFile cfg/userchrome.css;
        };
        # profile for debugging
        profiles.clean = {
          name = "clean";
          id = 1;
          settings = {
            "general.warnOnAboutConfig" = false;
            "browser.aboutConfig.showWarning" = false;
            "extensions.pocket.enabled" = false;
            "extensions.formautofill.addresses.enabled" = false;
            # no history
            "browser.privatebrowsing.autostart" = true;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" =
              false;
            "browser.newtabpage.activity-stream.feeds.section.highlights" =
              false;
          };
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
        options = {
          page-padding = 1;
          statusbar-h-padding = 0;
          statusbar-v-padding = 0;
          selection-clipboard = "clipboard";
        };
        extraConfig = ''
          map u scroll half-up
          map d scroll half-down
          map D toggle_page_mode
          map r reload
          map R rotate
          map K zoom in
          map J zoom out
          map i recolor
          map p print
        '';
      };

      mpv = {
        enable = true;
        bindings = {
          h = "seek -5";
          j = "seek -60";
          k = "seek 60";
          l = "seek 5";

          # rebind lost l binding, matches across from L which loops whole file
          H = "ab-loop";
          # rebind lost j binding, move J to K
          J = "cycle sub";
          K = "cycle sub down";
        };
      };
    };

    services = {
      network-manager-applet.enable = true;

      picom = {
        enable = true;
        # https://github.com/yshui/picom/blob/next/picom.sample.conf
        fade = true;
        fadeDelta = 4;
        # inactiveOpacity = "0.9";
        opacityRule = [ "92:class_g = 'Emacs'" ];
        blur = true;
        blurExclude = [ "class_g = 'slop'" ];
      };

      dunst = {
        enable = true;
        # https://github.com/dunst-project/dunst/blob/master/dunstrc
        settings = {
          global = {
            alignment = "left";
            follow = "keyboard";
            frame_width = 1;
            geometry = "1000+780+0";
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

      unclutter.enable = true;

      polybar = {
        enable = true;
        package = pkgs.polybar.override {
          mpdSupport = true;
          pulseSupport = true;
          i3GapsSupport = true;
        };
        extraConfig = builtins.readFile cfg/polybar.conf;
        script = ''
          #!/usr/bin/env sh
          pkill polybar >/dev/null
          while pgrep -u $(id -u) -x polybar >/dev/null; do sleep 1; done
          polybar main &
        '';
      };
    };
  };
}
