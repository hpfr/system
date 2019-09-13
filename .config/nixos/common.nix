{ config, pkgs, options, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    <home-manager/nixos>
  ];

  nix.nixPath = options.nix.nixPath.default ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];
  nixpkgs.overlays = [
    (self: super: {
      mwlwifi = super.callPackage ./pkgs/mwlwifi { };
    })
    (self: super: {
      ipts = super.callPackage ./pkgs/ipts { };
    })
    (self: super: {
      i915 = super.callPackage ./pkgs/i915 { };
    })
    (self: super: {
      mrvl = super.callPackage ./pkgs/mrvl { };
    })
    (self: super: {
      libwacom = super.libwacom.overrideAttrs (oldAttrs: {
        patches = oldAttrs.patches or [] ++ [ ./pkgs/libwacom/patches/00_mei-bus.patch ./pkgs/libwacom/patches/01_surface-tablet-data.patch ];
      });
    })
    (self: super: {
      keepassxc = super.keepassxc.override {
        withKeePassNetworking = true;
      };
    })
    (self: super: {
      linux_5_1 = super.linux_5_1.override {
        extraConfig = ''
          SERIAL_DEV_BUS y
          SERIAL_DEV_CTRL_TTYPORT y
          SURFACE_ACPI m
          SURFACE_ACPI_SSH y
          SURFACE_ACPI_SAN y
          SURFACE_ACPI_VHF y
          SURFACE_ACPI_DTX y
          SURFACE_ACPI_SID n
          INTEL_IPTS m
          MWLWIFI n
        '';
        # ignoreConfigErrors = true;
      };
    })
    (self: super: {
      linux_4_19 = super.linux_4_19.override {
        extraConfig = ''
          SERIAL_DEV_BUS y
          SERIAL_DEV_CTRL_TTYPORT y
          SURFACE_ACPI m
          SURFACE_ACPI_SSH y
          SURFACE_ACPI_SAN y
          SURFACE_ACPI_VHF y
          SURFACE_ACPI_DTX y
          SURFACE_ACPI_SID n
          INTEL_IPTS m
          MWLWIFI n
        '';
        # ignoreConfigErrors = true;
      };
    })
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.wireless.networks = {
  #   eduroam = {
  #     auth = ''
  #       key_mgmt=WPA-EAP
  #       eap=PEAP
  #       identity="user@example.com"
  #       password="secret"
  #     '';
  #   };
  # };
  networking.networkmanager.enable = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    # consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/Chicago";
  location.provider = "geoclue2";

  fonts.fontconfig.penultimate.enable = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   hplip # python27Packages.dbus-python dbus
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  # services.printing = {
  #   enable = true;
  #   drivers = [ pkgs.hplipWithPlugin ]; # FIXME hp-setup not working
  # };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull; # for bluetooth?
  };
  hardware.bluetooth.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";

  services.xserver.libinput.enable = true;

  services.xserver.displayManager.startx.enable = true;
  # services.xserver.windowManager.i3.enable = true;
  # services.xserver.windowManager.i3.package = pkgs.i3-gaps;
  services.xserver.desktopManager.xterm.enable = false;

  hardware.opengl.driSupport32Bit = true; # for 32-bit games

  nixpkgs.config.allowUnfree = true;

  services.dbus.packages = with pkgs; [ gnome3.dconf ];

  # for steam controller # FIXME
  # hardware.steam-hardware.enable = true;
  # services.udev.extraRules = ''
  # #   # This rule is needed for basic functionality of the controller in Steam and keyboard/mouse emulation
  # #   SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0666"

  # # This rule is necessary for gamepad emulation; make sure you replace 'pgriffais' with a group that the user that runs Steam belongs to
  # #   # KERNEL=="uinput", MODE="0660", GROUP="steam-input", OPTIONS+="static_node=uinput"
  # #   # KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"
  # KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput", GROUP="input", MODE="0660"

  # #   # Valve HID devices over USB hidraw
  # #   KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0666"

  # #   # Valve HID devices over bluetooth hidraw
  # #   KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0666"

  # #   # DualShock 4 over USB hidraw
  # #   KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0666"

  # #   # DualShock 4 wireless adapter over USB hidraw
  # #   KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0666"

  # #   # DualShock 4 Slim over USB hidraw
  # #   KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0666"

  # #   # DualShock 4 over bluetooth hidraw
  # #   KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0666"

  # #   # DualShock 4 Slim over bluetooth hidraw
  # #   KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0666"

  # #   # Nintendo Switch Pro Controller over USB hidraw
  # #   KERNEL=="hidraw*", ATTRS{idVendor}=="057e", ATTRS{idProduct}=="2009", MODE="0666"

  # #   # Nintendo Switch Pro Controller over bluetooth hidraw
  # #   KERNEL=="hidraw*", KERNELS=="*057E:2009*", MODE="0666"
  # '';
  services.udev.extraRules = ''
    # Valve USB devices
    SUBSYSTEM=="usb", ATTRS{idVendor}=="28de", MODE="0660", TAG+="uaccess"

    # Steam Controller udev write access
    KERNEL=="uinput", SUBSYSTEM=="misc", TAG+="uaccess", OPTIONS+="static_node=uinput"

    # Valve HID devices over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="28de", MODE="0660", TAG+="uaccess"

    # Valve HID devices over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*28DE:*", MODE="0660", TAG+="uaccess"

    # DualShock 4 over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="05c4", MODE="0660", TAG+="uaccess"

    # DualShock 4 wireless adapter over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="0ba0", MODE="0660", TAG+="uaccess"

    # DualShock 4 Slim over USB hidraw
    KERNEL=="hidraw*", ATTRS{idVendor}=="054c", ATTRS{idProduct}=="09cc", MODE="0660", TAG+="uaccess"

    # DualShock 4 over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:05C4*", MODE="0660", TAG+="uaccess"

    # DualShock 4 Slim over bluetooth hidraw
    KERNEL=="hidraw*", KERNELS=="*054C:09CC*", MODE="0660", TAG+="uaccess"

    ######################################################################

    # IPTS Touchscreen (SP2017)
    SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Touchscreen", ENV{ID_INPUT_TOUCHSCREEN}="1", SYMLINK+="input/touchscreen"

    # IPTS Pen (SP2017)
    SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Pen", SYMLINK+="input/pen"
  '';

  services.flatpak = { # for Spotify, Discord, and Telegram
    enable = true;
  };

  # services.emacs = {
  #   enable = true;
  #   package = with pkgs;
  #     ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
  #       epkgs.emacs-libvterm
  #     ]));
  # };

  xdg.portal = { # for Flatpak
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
  };

  # create group for steam controller
  # users.groups.steam-input = {};

  # users.mutableUsers = false;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.lh = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" "input" "video" "lp"];
  };

  home-manager.useUserPackages = true;
  home-manager.users.lh = { config, pkgs, ... }: {
    nixpkgs.config.allowUnfree = true;
    home.packages = with pkgs; [
      nix-diff
      gnumake
      gnutls # for circe
      shellcheck
      zip unzip
      socat # detach processes
      wget # file download CLI
      youtube-dl # video download CLI
      mpc_cli # mpd CLI
      rclone # multiplatform cloud sync CLI
      texlive.combined.scheme-medium
      pandoc # convert document formats
      python3

      htop # system monitoring TUI
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

      arandr # monitor layout GUI
      blueman # bluetooth GUI
      pavucontrol # pulseaudio GUI
      wpgtk # gtk GUI
      networkmanager_dmenu
      rofi-systemd
      sxiv # simple x image viewer
      maim # lightweight screenshot utility

      libreoffice # office suite
      keepassxc # password manager
      xournalpp # handwritten notes and PDF markup
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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
