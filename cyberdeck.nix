{ config, pkgs, lib, ... }:

{
  imports = [
    ./gui.nix
    ./linux-surface.nix
    # testing
    # ../nixos-hardware/microsoft/surface
  ];

  hardware = { acpilight.enable = true; };

  system.stateVersion = "19.03";

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
