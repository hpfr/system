{ config, pkgs, options, ... }:

{
  imports = [
    /etc/nixos/hardware-configuration.nix
    <home-manager/nixos>
  ];

  nix.nixPath = options.nix.nixPath.default ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
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
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
  };

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

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  # environment.systemPackages = with pkgs; [
  #   hplip # python27Packages.dbus-python dbus
  # ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  services = {
    # Enable CUPS to print documents.
    # printing = {
    #   enable = true;
    #   drivers = [ pkgs.hplipWithPlugin ]; # FIXME hp-setup not working
    # };

    # emacs = {
    #   enable = true;
    #   package = with pkgs;
    #     ((emacsPackagesNgGen emacs).emacsWithPackages (epkgs: [
    #       epkgs.emacs-libvterm
    #     ]));
    # };
  };

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
      rclone # multiplatform cloud sync CLI
      texlive.combined.scheme-medium
      pandoc # convert document formats
      python3

      htop # system monitoring TUI
    ];

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
    };
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?
}
