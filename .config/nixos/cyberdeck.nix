{ config, pkgs, ... }:

{
  imports = [
    ./gui.nix
  ];

  hardware = {
    firmware = with pkgs; [ mwlwifi ipts i915 mrvl ];
    acpilight.enable = true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_4_19;
    kernelPatches = [
      {
        name = "surface-acpi";
        patch = ./pkgs/linux/patches/4.19/0001-surface-acpi.patch;
      }
      {
        name = "surface-suspend";
        patch = ./pkgs/linux/patches/4.19/0002-suspend.patch;
      }
      {
        name = "surface-buttons";
        patch = ./pkgs/linux/patches/4.19/0003-buttons.patch;
      }
      {
        name = "surface-cameras";
        patch = ./pkgs/linux/patches/4.19/0004-cameras.patch;
      }
      {
        name = "surface-ipts";
        patch = ./pkgs/linux/patches/4.19/0005-ipts.patch;
      }
      {
        name = "surface-hid";
        patch = ./pkgs/linux/patches/4.19/0006-hid.patch;
      }
      {
        name = "surface-sd";
        patch = ./pkgs/linux/patches/4.19/0007-sdcard-reader.patch;
      }
      {
        name = "surface-wifi";
        patch = ./pkgs/linux/patches/4.19/0008-wifi.patch;
      }
      {
        name = "surface-mwlwifi";
        patch = ./pkgs/linux/patches/4.19/0010-mwlwifi.patch;
      }
    ];
    # extraModulePackages = [ pkgs.mwlwifi ]; # not sure of diff between this and hw.fw
    kernelModules = [ "hid" "hid_sensor_hub" "hid_generic" "usbhid" "hid_multitouch" "intel_ipts" ]; # surface_acpi not in lsmod?
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32"; # large console font
  };

  fonts.fontconfig.dpi = 192;

  networking.hostName = "cyberdeck"; # Define your hostname.

  nix = {
    buildMachines = [{
      hostName = "builder";
      system = "x86_64-linux";
      maxJobs = 16;
      supportedFeatures = [ "kvm" ];
    }];
    # remote builds. test with nix-build '<nixpkgs/nixos>' -A system [-vvv]
    # override with:
    # nrs --option builders "" --option substituters "https://cache.nixos.org"
    #     --option trusted-public-keys "cache.nixos.org-1:<snip>"
    distributedBuilds = true;
    extraOptions = ''
      # use when remote builder has faster internet connection than local
      builders-use-substitutes = true
    '';
    # builder as remote substituter
    binaryCaches = [ "ssh-ng://builder" ];
    binaryCachePublicKeys =
      [ "builder:uAGekC7E+5VX90GFu3Tef0l5cD3dUAEgT6/s3F8v6Fo=" ];
  };
  services = {
    logind = {
      lidSwitch = "ignore";
      extraConfig = ''
        HandlePowerKey=suspend
      '';
    };

    xserver = {
      wacom.enable = true;
      libinput = {
        enable = true;
        naturalScrolling = true;
      };
      dpi = 192; # doesn't seem to work with startx
    };

    udev.extraRules = ''
      ######################################################################

      # IPTS Touchscreen (SP2017)
      SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Touchscreen", ENV{ID_INPUT_TOUCHSCREEN}="1", SYMLINK+="input/touchscreen"

      # IPTS Pen (SP2017)
      SUBSYSTEMS=="input", ATTRS{name}=="ipts 1B96:001F Pen", SYMLINK+="input/pen"

      ######################################################################
    '';
  };

  systemd.services = {
    surface-sleep = {
      wantedBy = [ "systemd-suspend.target" ];
      script = ''
        # unload the modules before going to sleep
        systemctl stop NetworkManager.service
        modprobe -r intel_ipts
        modprobe -r mei_me
        modprobe -r mei
        modprobe -r mwifiex_pcie;
        modprobe -r mwifiex;
        modprobe -r cfg80211;
      '';
    };
    surface-wake = {
      wantedBy = [ "post-resume.target" ];
      script = ''
        # need to cycle the modules on a resume and after the reset is called, so unload...
        modprobe -r intel_ipts
        modprobe -r mei_me
        modprobe -r mei
        modprobe -r mwifiex_pcie;
        modprobe -r mwifiex;
        modprobe -r cfg80211;
        # and reload
        modprobe -i intel_ipts
        modprobe -i mei_me
        modprobe -i mei
        modprobe -i cfg80211;
        modprobe -i mwifiex;
        modprobe -i mwifiex_pcie;
        echo 1 > /sys/bus/pci/rescan
        systemctl restart NetworkManager.service
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    libwacom
    acpilight
  ];

  environment.etc."systemd/sleep.conf".text = ''
    [Sleep]
    SuspendState=freeze
  '';

  home-manager.users.lh = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      onboard
    ];
    programs = {
      bash.sessionVariables = {
        GDK_SCALE = 2;
        GDK_DPI_SCALE = 0.5;
        QT_AUTO_SCREEN_SCALE_FACTOR = 1;
        MOZ_USE_XINPUT2 = 1;
      };
      rofi.extraConfig = ''
        rofi.dpi: 192
      '';
    };
    services = {
      polybar = {
        config = {
          "bar/main" = {
            height = 40;
            font-0 = "Hasklug Nerd Font:size=16;2";
            font-1 = "EmojiOne Color:size=16";
            font-2 = "unifont:fontformat=truetype:size=16:antialias=false;0";
            font-3 = "siji:pixelsize=16;1";
            modules-right = "filesystem backlight-acpi pulseaudio memory cpu wlan temperature battery date";
          };
          "module/wlan" = {
            interface = "wlp5s0";
          };
        };
      };
    };
  };
}
