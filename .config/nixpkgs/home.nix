{ config, pkgs, ... }:

{
  home.packages = with pkgs; [
    gnumake
    gnutls # for circe
    shellcheck
    zip unzip
    socat # detach processes
    wget # file download CLI
    youtube-dl # video download CLI
    pulsemixer # pulseaudio CLI
    mpc_cli # mpd CLI
    rclone # multiplatform cloud sync CLI

    htop # system monitoring TUI

    google-fonts
    nerdfonts # warning: downloads almost 2 GiB
    emojione # emoji

    sxhkd # wm agnostic keybindings for X
    xorg.xwininfo # query window information
    xorg.xprop # query window properties
    xorg.xdpyinfo # get info like DPI
    xdotool # manage windows in scripts
    xclip # manage clipboard in scripts
    arandr # monitor layout GUI
    blueman # bluetooth GUI
    pavucontrol # pulseaudio GUI
    wpgtk # gtk GUI
    networkmanager_dmenu
    rofi-systemd
    sxiv # simple x image viewer
    maim # lightweight screenshot utility
    keepassxc # password manager
    xournalpp # handwritten notes and PDF markup
    steam
  ];

  xsession = {
    enable = true;
    initExtra = ''
      # setbg &		# Set the background
      [ -f ~/.Xresources ] && xrdb -merge ~/.Xresources
      sxhkd &	# Bind keys
      xset r rate 300 50 &	# Speed xrate up
      mpdupdate &
    '';

    windowManager = {
      # command = "exec ${pkgs.i3}/bin/i3";
      i3 = {
        enable = true;
        package = pkgs.i3-gaps;
        config = null;
  extraConfig = builtins.readFile ~/.config/i3/config-nix;
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
    # Let Home Manager install and manage itself.
    home-manager.enable = true;

    neovim = {
      enable = true;
    };

    emacs = {
      enable = true;
      extraPackages = epkgs: [ epkgs.emacs-libvterm ];
    };

    git = {
      enable = true;
      userName = "hpfr";
      userEmail = "44043764+hpfr@users.noreply.github.com";
    };

    alacritty = {
      enable = true;
    };

    firefox = {
      enable = true;
      profiles.default = {
        name = "default";
	userChrome = builtins.readFile ~/.config/firefox/userChrome.css;
      };
    };

    rofi = {
      enable = true;
      theme = "Arc-Dark";
      extraConfig = ''
        rofi.dpi: 196
        rofi.modi: window,run,ssh,drun,combi
        rofi.combi-modi: window,drun
      '';
    };

    zathura = {
      enable = true;
      extraConfig = builtins.readFile ~/.config/zathura/zathurarc-nix;
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
      config = ~/.config/polybar/nix.conf;
      script = "";
    };
  };
}
