{ config, pkgs, options, ... }:

{
  imports = [ # Include the results of the hardware scan.
    /etc/nixos/hardware-configuration.nix
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
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    hplip python27Packages.dbus-python dbus
  ];

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
  services.printing = {
    enable = true;
    drivers = [ pkgs.hplipWithPlugin ]; # FIXME hp-setup not working
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

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

  services.dbus.packages = [ pkgs.gnome3.dconf ];

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

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
