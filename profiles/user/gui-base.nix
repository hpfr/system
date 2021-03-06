{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gui-base;
in {
  options.profiles.user.gui-base.enable =
    mkEnableOption "my user-level GUI configuration";

  config = mkIf cfg.enable {
    profiles.user = {
      local-base.enable = true;

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
        desktop-file-utils # validate desktop files and update db

        texlive.combined.scheme-full # latex environment
        pandoc # convert document formats

        onlykey-cli
        libimobiledevice # iOS device utils

        imagemagick7 # image editing CLI and GUI
        qpdf # pdf manipulation CLI

        pavucontrol # pulseaudio GUI
        gnome.gnome-sound-recorder # audio recording
        piper # mouse configuration via libratbag

        safeeyes # reminds user on eye health
        nextcloud-client

        # vscodium # for draw.io extension
        drawio

        # virtualization
        libvirt # manage VM's
        virtmanager # manage VM's graphically
        remmina # usable RDP client
        openconnect # cs.wisc.edu VPN

        # applications
        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        calibre # ebook manager
        foliate # ebook reader
        mcomix3 # comic reader
        anki # spaced repetition software
        zotero # research citation manager

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
      configFile."autostart/org.keepassxc.KeePassXC.desktop".text = ''
        ${fileContents
        "${pkgs.keepassxc}/share/applications/org.keepassxc.KeePassXC.desktop"}
      '';
      # need time to unlock keepassxc database
      configFile."autostart/com.nextcloud.desktopclient.nextcloud.desktop".text =
        builtins.replaceStrings [''
          Exec=nextcloud
        ''] [''
          Exec=sh -c "sleep 60 && nextcloud"
        ''] ''
          ${fileContents
          "${pkgs.nextcloud-client}/share/applications/com.nextcloud.desktopclient.nextcloud.desktop"}
        '';
    };

    gtk = {
      enable = true;
      theme.name = "Adwaita-dark";
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

    programs = {
      mangohud.enable = true;
      vscode = {
        enable = true;
        package = pkgs.vscode-fhsWithPackages (ps: with ps; [ ]);
      };
    };

    services.network-manager-applet.enable = true;
  };
}
