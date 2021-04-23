{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gui-base;
in {
  options.profiles.user.gui-base.enable =
    mkEnableOption "my user-level GUI configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      base.enable = true;

      fontconfig.enable = true;

      # programs
      ssh.enable = true;
      firefox.enable = true; # web browser
      alacritty.enable = true; # terminal emulator
      zathura.enable = false; # pdf viewer
      mpv.enable = true; # video player
      nomacs.enable = true; # versatile photo viewer
      celluloid.enable = true; # mpv gtk frontend
    };

    home = {
      packages = with pkgs; [
        ponymix # pulseaudio CLI (for volume keys)
        playerctl # MPRIS controller (for media keys)
        libnotify # notify-send command
        libsecret # secret-tool command

        texlive.combined.scheme-full # latex environment
        pandoc # convert document formats

        imagemagick7 # image editing CLI and GUI
        qpdf # pdf manipulation CLI
        adwaita-qt # make qt apps look like gtk apps

        pavucontrol # pulseaudio GUI
        wpgtk # gtk GUI

        safeeyes # reminds user on eye health
        syncthingtray # syncthing tray

        vscodium # for draw.io extension

        # virtualization
        libvirt # manage VM's
        virtmanager # manage VM's graphically
        remmina # usable RDP client
        openconnect_pa # cs.wisc.edu VPN

        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        calibre # ebook manager
        # nur.repos.onny.foliate # ebook reader

        # messaging
        element-desktop # matrix electron client
        signal-desktop # signal client
        tdesktop # telegram client (FOSS)

        # gaming
        libstrangle # fps limiter
        sc-controller # use steam controller without steam
        protontricks # for problematic Steam Play games
        moltengamepad # hotplugging controller shim
        # wine # wine is not an emulator

        python-onlykey # OnlyKey CLI
      ];

      sessionVariables = {
        QT_STYLE_OVERRIDE = "Adwaita-Dark"; # for adwaita-qt
        SUDO_ASKPASS = "${pkgs.gnome3.seahorse}/libexec/seahorse/ssh-askpass";
      };
    };

    xdg = {
      enable = true;
      # see application profiles for file associations and defaults
      mimeApps.enable = true;
      userDirs = {
        enable = true;
        desktop = "$HOME/desktop";
        documents = "$HOME/documents";
        download = "$HOME/downloads";
        music = "$HOME/documents/music";
        pictures = "$HOME/documents/pictures";
        publicShare = "$HOME/documents/public";
        templates = "$HOME/documents/templates";
        videos = "$HOME/documents/videos";
        createDirectories = true;
      };
    };

    gtk = {
      enable = true;
      theme.name = "Adwaita";
      iconTheme = {
        package = pkgs.gnome3.adwaita-icon-theme;
        name = "Adwaita-icons";
      };
      font.name = "Sans";
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

    services.network-manager-applet.enable = true;
  };
}
