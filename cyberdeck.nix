{ config, pkgs, lib, ... }:

{
  imports = [ ./gui.nix ];

  hardware = {
    firmware = with pkgs; [ ipts ];
    # firmware = with pkgs; [ mwlwifi mrvl ];
    acpilight.enable = true;
  };

  system.stateVersion = "19.03";

  nixpkgs.overlays = [
    (self: super: { ipts = super.callPackage ./pkgs/ipts { }; })
    # (self: super: { mwlwifi = super.callPackage ./pkgs/mwlwifi { }; })
    # (self: super: { mrvl = super.callPackage ./pkgs/mrvl { }; })
    # Limit patched libwacom to Xorg. Everything still works afaict
    (self: super: {
      # I believe this is for desktop environments that depend on
      # xf86inputlibinput, but otherwise the xorg overlay covers everything
      xf86inputlibinput =
        super.xf86inputlibinput.override { libinput = self.libinput-surface; };
      xorg = super.xorg // {
        xf86inputlibinput = super.xorg.xf86inputlibinput.override {
          libinput = self.libinput-surface;
        };
      };
      libinput-surface =
        super.libinput.override { libwacom = self.libwacom-surface; };
      libwacom-surface = super.libwacom.overrideAttrs (oldAttrs: {
        patches = oldAttrs.patches or [ ]
          ++ (map (name: ./pkgs/libwacom/patches + "/${name}")
            (builtins.attrNames (lib.filterAttrs (k: v: v == "regular")
              (builtins.readDir ./pkgs/libwacom/patches))));
      });
    })
    (self: super: {
      linux_4_19 = super.linux_4_19.override {
        argsOverride = {
          version = "4.19.95";
          modDirVersion = "4.19.95";
          src = pkgs.fetchurl {
            url = "mirror://kernel/linux/kernel/v4.x/linux-4.19.95.tar.xz";
            sha256 = "1c2g5wcf4zgy5q51qrf0s4hf3pr1j8gi8gn27w8cafn1xqrcmvaa";
          };
        };
        extraConfig = ''
          INTEL_IPTS m
          INTEL_IPTS_SURFACE m
          SERIAL_DEV_BUS y
          SERIAL_DEV_CTRL_TTYPORT y
          SURFACE_SAM m
          SURFACE_SAM_SSH m
          SURFACE_SAM_SSH_DEBUG_DEVICE y
          SURFACE_SAM_SAN m
          SURFACE_SAM_VHF m
          SURFACE_SAM_DTX m
          SURFACE_SAM_HPS m
          SURFACE_SAM_SID m
          SURFACE_SAM_SID_GPELID m
          SURFACE_SAM_SID_PERFMODE m
          SURFACE_SAM_SID_VHF m
          SURFACE_SAM_SID_POWER m
          INPUT_SOC_BUTTON_ARRAY m
          MWLWIFI m
        '';
        # ignoreConfigErrors = true;
      };
    })
    # (self: super: {
    #   linux_latest = super.linux_latest.override {
    #     extraConfig = ''
    #       SERIAL_DEV_BUS y
    #       SERIAL_DEV_CTRL_TTYPORT y
    #       SURFACE_SAM y
    #       SURFACE_SAM_SSH m
    #       SURFACE_SAM_SAN m
    #       SURFACE_SAM_VHF m
    #       SURFACE_SAM_DTX m
    #       SURFACE_SAM_SID m
    #       SURFACE_SAM_SID_GPELID m
    #       SURFACE_SAM_SID_VHF m
    #       INPUT_SOC_BUTTON_ARRAY m
    #       MWLWIFI n
    #     '';
    #     # ignoreConfigErrors = true;
    #   };
    # })
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_4_19;
    # kernelPackages = pkgs.linuxPackages_latest;
    # in case upgrade fails, comment out patches and kernel config, rebuild, and
    # then rebuild on the new gen and it should work
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
      {
        name = "surface-ioremap-uc";
        patch = ./pkgs/linux/patches/4.19/0012-ioremap_uc.patch;
      }
    ];
    # kernelPatches = [
    #   {
    #     name = "surface-ioremap-uc";
    #     patch = ./pkgs/linux/patches/5.4/0001-ioremap_uc.patch;
    #   }
    #   # {
    #   #   name = "surface-hid";
    #   #   patch = ./pkgs/linux/patches/5.4/0002-hid.patch;
    #   # }
    #   {
    #     name = "surface-acpi";
    #     patch = ./pkgs/linux/patches/5.4/0003-surface-acpi.patch;
    #   }
    #   {
    #     name = "surface-wifi";
    #     patch = ./pkgs/linux/patches/5.4/0006-wifi.patch;
    #   }
    # ];
    # extraModulePackages = [ pkgs.mwlwifi ]; # not sure of diff between this and hw.fw
    kernelModules = [
      # surface_san not in lsmod?
      "hid"
      "hid_sensor_hub"
      "hid_generic"
      "usbhid"
      "hid_multitouch"
      "intel_ipts"
      "ipts_surface"
    ];
  };

  console.font = "latarcyrheb-sun32"; # large console font

  fonts.fontconfig.dpi = 192;

  networking = {
    hostName = "cyberdeck"; # Define your hostname.
    networkmanager.wifi = {
      powersave = false;
      scanRandMacAddress = false;
    };
  };

  # I have very limited space on this machine, so I have to be very strict with
  # disk management, especially with how space hungry NixOS generations can be.
  #
  # sudo nix-env --profile /nix/var/nix/profiles/system --list-generations
  # to see system profiles, and then
  # ls -l /nix/var/nix/profiles/system*link
  # to see nixpkgs revision
  # sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations {a..b}
  # and then sudo nix-collect-garbage to delete all the new garbage
  # or sudo nix-collect-garbage -d to delete all profiles
  #
  # ncdu on /nix/store to find largest items, then
  # nix-store --query --roots /nix/store/large-item
  # to determine why it can't be GC'd, then delete roots and
  # nix-store --delete /nix/store/large-item
  # to target space hogs

  nix = {
    buildMachines = [
      {
        hostName = "monolith";
        system = "x86_64-linux";
        maxJobs = 16;
        speedFactor = 2;
        supportedFeatures = [ "kvm" ];
      }
      {
        hostName = "hal";
        system = "x86_64-linux";
        maxJobs = 12;
        speedFactor = 1;
      }
    ];
    # remote builds. override with: nrs --option builders ""
    # root's ~/.ssh/config must include the relevant config
    # add authorized public keys to remotes
    distributedBuilds = true;
    extraOptions = ''
      # use when remote builder has faster internet connection than local
      # otherwise local gets all dependencies and sends them to builder
      builders-use-substitutes = true
    '';
    # builder as remote substituter
    # enable with --option trusted-substituters "ssh-ng://host"
    trustedBinaryCaches = [ "ssh-ng://monolith" "ssh-ng://hal" ];
    binaryCachePublicKeys = [
      "monolith:qYcj/A6mRSPaaFn9sYYieWVY+0ZRPb2KavAJwYzTeJQ="
      "hal:qCUZMYJDjG0op5k8grKUSYojNoaqA+931VeFucyqH6U="
    ];
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
    libinput-surface
    libwacom-surface
    acpilight
  ];

  environment.etc."systemd/sleep.conf".text = ''
    [Sleep]
    SuspendState=freeze
  '';

  home-manager.users.lh = { config, pkgs, ... }: {
    home = {
      packages = with pkgs; [ onboard ];
      sessionVariables = {
        GDK_SCALE = 2;
        GDK_DPI_SCALE = 0.5;
        QT_AUTO_SCREEN_SCALE_FACTOR = 1;
        MOZ_USE_XINPUT2 = 1;
      };
    };

    xresources.properties."Xft.dpi" = 192;
    xsession.pointerCursor.size = 64;

    programs = {
      ssh.matchBlocks = {
        hal = {
          hostname = "10.10.10.8";
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
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
            font-1 = "JoyPixels:size=16";
            modules-right =
              "temperature cpu memory filesystem battery wlan backlight-acpi pulseaudio date";
          };
          "module/wlan".interface = "wlp1s0";
        };
      };
    };
  };
}
