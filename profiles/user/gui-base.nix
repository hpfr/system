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
      firefox.enable = true;
      alacritty.enable = true;
      zathura.enable = true;
      mpv.enable = true;
      nomacs.enable = true;
    };

    home = {
      packages = with pkgs; [
        pamixer # pulseaudio CLI (for volume keys)

        texlive.combined.scheme-medium # latex environment
        pandoc # convert document formats

        imagemagick7 # image editing CLI and GUI
        qpdf # pdf manipulation CLi
        adwaita-qt # make qt apps look like gtk apps

        blueman # bluetooth GUI
        pavucontrol # pulseaudio GUI
        wpgtk # gtk GUI
        bookworm # ebook reader

        celluloid # mpv gtk frontend
        safeeyes # reminds user on eye health
        syncthingtray # syncthing tray

        # virtualization
        libvirt # manage VM's
        virtmanager # manage VM's graphically
        remmina # usable RDP client

        keepassxc # password manager
        xournalpp # handwritten notes and PDF markup

        # messaging
        element-desktop # matrix electron client
        signal-desktop # signal client
        tdesktop # telegram client (FOSS)

        # gaming
        sc-controller # use steam controller without steam
        steam
        protontricks # for problematic Steam Play games
        # wine # wine is not an emulator

        python-onlykey # OnlyKey CLI
      ];

      # for adwaita-qt
      sessionVariables.QT_STYLE_OVERRIDE = "Adwaita-Dark";
    };

    # see application profiles for file associations and defaults
    xdg.mimeApps.enable = true;

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

    services.network-manager-applet.enable = true;
  };
}
