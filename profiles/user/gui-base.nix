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

        tectonic # new and improved latex environment
        biber # biblatex backend
        pandoc # convert document formats

        # hardware
        onlykey-cli
        solo2-cli
        libimobiledevice # iOS device utils

        # documents
        imagemagick # image editing CLI and GUI
        libjxl # jpeg xl tools
        qpdf # pdf manipulation CLI
        diff-pdf # pdf comparison
        ocrmypdf # OCR tool wrapping tesseract
        poppler_utils # pdfimages
        zbar # QR decode
        qrencode
        fontforge-gtk # font editor

        opensnitch-ui # application firewall
        pavucontrol # pulseaudio GUI
        gnome.gnome-sound-recorder # audio recording
        mousai # song recognition
        piper # mouse configuration via libratbag
        xdragon # drag and drop from the terminal (and emacs)
        d-spy # d-bus explorer

        nextcloud-client

        # virtualization
        libvirt # manage VM's
        virt-manager # manage VM's graphically
        openconnect # cs.wisc.edu VPN
        distrobox # other distro userlands

        # applications
        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup
        rnote # modern handwritten notes
        calibre # ebook manager
        foliate # ebook reader
        zotero # research citation manager
        shortwave # internet radio

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

      # compose key definitions
      file.".XCompose".text = ''
        include "%L"

        XCOMM smart quotes
        <Multi_key> <l> <q>	: "“"
        <Multi_key> <q> <l>	: "“"
        <Multi_key> <r> <q>	: "”"
        <Multi_key> <q> <r>	: "”"
        <Multi_key> <l> <a>	: "‘"
        <Multi_key> <a> <l>	: "‘"
        <Multi_key> <r> <a>	: "’"
        <Multi_key> <a> <r>	: "’"
        <Multi_key> <apostrophe> <p>	: "′"
        <Multi_key> <p> <apostrophe>	: "′"
        <Multi_key> <quotedbl> <p>	: "″"
        <Multi_key> <p> <quotedbl>	: "″"

        XCOMM combining accents
        <Multi_key> <apostrophe> <apostrophe>	: "́"
        <Multi_key> <grave> <grave>	: "́"
        <Multi_key> <comma> <comma>	: "̧"
        <Multi_key> <asciicircum> <asciicircum>	: "̂"
        <Multi_key> <quotedbl> <quotedbl>	: "̈"
      '';
    };

    # emoji and special character input
    dconf = {
      enable = true;
      settings = {
        "desktop/ibus/panel/emoji" = {
          unicode-hotkey = [ "<Super><Shift>u" ];
          hotkey = [ "<Super>period" ];
          has-partial-match = true;
          partial-match-length = 3;
          # match all containing
          partial-match-condition = 2;
        };
      };
    };

    systemd.user = {
      timers.bgcron = {
        Unit.After = [ "graphical-session.target" ];
        Timer = {
          OnCalendar = "daily";
          Unit = "bgcron.service";
          Persistent = true;
        };
        Install.WantedBy = [ "timers.target" ];
      };
      services.bgcron = {
        Unit = {
          After = [ "graphical-session.target" ];
          Wants = [ "bgcron.timer" ];
        };
        Service.ExecStart = "${pkgs.gui-scripts}/bin/setbg";
      };
    };

    xdg = {
      enable = true;
      # see application profiles for file associations and defaults
      mimeApps = {
        enable = true;
        # calibre includes a viewer I never want to use
        defaultApplications = {
          "application/vnd.oasis.opendocument.text" = "writer.desktop";
          "application/vnd.openxmlformats-officedocument.wordprocessingml.document" =
            "writer.desktop";
          "application/rtf" = "writer.desktop";
          "text/rtf" = "writer.desktop";
          "application/x-cbr" = "com.github.johnfactotum.foliate";
          "application/x-cbz" = "com.github.johnfactotum.foliate";
        };
      };

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
        ''
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
      cursorTheme = {
        package = pkgs.capitaine-cursors;
        name = "capitaine-cursors";
        size = 32;
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

      texlive = {
        enable = true;
        extraPackages = tpkgs: {
          inherit (tpkgs)
            collection-basic collection-latex collection-latexrecommended
            collection-xetex collection-luatex dvipng dvisvgm latexmk synctex
            fontsetup
            # archival and deps
            pdfx xmpincl
            # org export
            wrapfig ulem capt-of
            # list formatting
            enumitem
            # citations
            biblatex biblatex-apa biblatex-ieee
            # arbitrary text placement
            textpos
            # auctex preview (pdfcrop includes pdf2dsc)
            preview pdfcrop
            # koma
            xpatch
            # kaobook
            xifthen ifmtarg options imakeidx hyphenat needspace placeins
            marginnote sidenotes changepage chngcntr footmisc footnotebackref
            tikzpagenodes ifoddpage multirow floatrow etoc libertinus-fonts
            mdframed zref subfiles todonotes algorithm2e relsize ccicons
            glossaries mfirstuc xfor datatool nomencl abstract

            # building inputnormalization
            l3build

            # mla.cls
            preprint newtx titlesec xstring csquotes hanging enotez fontaxes
            biblatex-mla translations;
        };
      };
    };

    services = {
      network-manager-applet.enable = true;
      # notifications for earlyoom
      systembus-notify.enable = true;
      # eye health
      safeeyes.enable = true;
    };
  };
}
