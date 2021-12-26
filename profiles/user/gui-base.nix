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
      gthumb.enable = true; # versatile photo viewer
      celluloid.enable = true; # mpv gtk frontend
      remmina.enable = true; # remote machine connections
    };

    home = {
      packages = with pkgs; [
        ponymix # pulseaudio CLI (for volume keys)
        playerctl # MPRIS controller (for media keys)
        libnotify # notify-send command
        libsecret # secret-tool command
        xdg-user-dirs # xdg-user-dir command for finding custom dirs
        desktop-file-utils # validate desktop files and update db

        (texlive.combine {
          inherit (texlive)
            collection-basic collection-latex collection-latexrecommended
            collection-xetex dvipng dvisvgm latexmk synctex
            # mla.cls
            enumitem preprint newtx titlesec xstring csquotes hanging biblatex
            enotez fontaxes biblatex-mla translations;
        })
        tectonic # new and improved latex environment
        biber # biblatex backend
        pandoc # convert document formats

        # hardware
        onlykey-cli
        libimobiledevice # iOS device utils
        sc-controller # use steam controller without steam

        # documents
        imagemagick7 # image editing CLI and GUI
        qpdf # pdf manipulation CLI
        diff-pdf # pdf comparison
        ocrmypdf # OCR tool wrapping tesseract
        zbar # QR decode
        qrencode

        pavucontrol # pulseaudio GUI
        gnome.gnome-sound-recorder # audio recording
        songrec # shazam client
        piper # mouse configuration via libratbag
        dragon-drop # drag and drop from the terminal (and emacs)

        safeeyes # reminds user on eye health
        nextcloud-client

        drawio

        # virtualization
        libvirt # manage VM's
        virtmanager # manage VM's graphically
        openconnect # cs.wisc.edu VPN
        wineWowPackages.stable # windows translation layer, 32 and 64 bit
        winetricks

        # applications
        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        calibre # ebook manager
        foliate # ebook reader
        zotero # research citation manager

        # messaging
        tdesktop # telegram client (FOSS)

        # gaming
        libstrangle # fps limiter
        moltengamepad # hotplugging controller shim
      ];

      sessionVariables = {
        SUDO_ASKPASS = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
        CALIBRE_USE_SYSTEM_THEME = 1;
      };
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
        Service.ExecStart = "${pkgs.gui-scripts}/bin/setbg";
      };
    };

    xdg = {
      enable = true;
      # see application profiles for file associations and defaults
      mimeApps.enable = true;
      userDirs = {
        enable = true;
        desktop = "$HOME/documents/desktop";
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
        package = pkgs.papirus-icon-theme;
        name = "Papirus-Dark";
      };
    };

    qt = {
      enable = true;
      platformTheme = "gnome"; # use gtk file chooser, etc with qt apps
      style = {
        name = "adwaita-dark";
        package = pkgs.adwaita-qt;
      };
    };

    programs = {
      git.extraConfig.credential.helper = "libsecret";
      mangohud.enable = true;
    };

    services.network-manager-applet.enable = true;
  };
}
