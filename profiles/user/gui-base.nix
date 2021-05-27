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
        xdg-user-dirs # xdg-user-dir command for finding custom dirs

        texlive.combined.scheme-full # latex environment
        pandoc # convert document formats

        onlykey-cli

        imagemagick7 # image editing CLI and GUI
        qpdf # pdf manipulation CLI

        pavucontrol # pulseaudio GUI
        wpgtk # gtk GUI
        gnome.gnome-sound-recorder # audio recording

        safeeyes # reminds user on eye health
        nextcloud-client

        vscodium # for draw.io extension

        # virtualization
        libvirt # manage VM's
        virtmanager # manage VM's graphically
        remmina # usable RDP client
        openconnect # cs.wisc.edu VPN

        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        calibre # ebook manager
        mcomix3 # comic reader
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
      ];

      sessionVariables = {
        SUDO_ASKPASS = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
        CALIBRE_USE_SYSTEM_THEME = 1;
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
        package = pkgs.gnome.adwaita-icon-theme;
        name = "Adwaita-icons";
      };
      font = {
        name = "Sans";
        size = 10;
      };
      gtk3.extraConfig.gtk-application-prefer-dark-theme = true;
    };

    # qt apps quit with "No GSettings schemas are installed on the system"
    qt = {
      enable = true;
      platformTheme = "gnome"; # use gtk file chooser, etc with qt apps
      style = {
        name = "adwaita-dark";
        package = pkgs.adwaita-qt;
      };
    };

    services.network-manager-applet.enable = true;
  };
}
