{ config, pkgs, lib, ... }:

{
  imports = [
    ./hosts-base.nix
    ./linux-surface.nix
    # testing
    # ../nixos-hardware/microsoft/surface
  ];

  profiles.system = {
    xorg-base.enable = true;
    hidpi.enable = true;
  };

  hardware = {
    acpilight.enable = true;
    # surface wifi doesn't work alongside bluetooth
    # depends on model, maybe even varies device-to-device
    bluetooth.enable = false;
  };

  system.stateVersion = "19.03";

  boot = {
    kernelPackages = pkgs.linuxPackages_4_19;
    # kernelPackages = pkgs.linuxPackages_5_6;
  };

  console.font = "latarcyrheb-sun32"; # large console font

  networking = {
    hostName = "dave";
    networkmanager.wifi = {
      powersave = false;
      scanRandMacAddress = false;
    };
  };

  /* I have very limited space on this machine, so I have to be very strict with
     disk management, especially with how space hungry NixOS generations can be.

     sudo nix-env --profile /nix/var/nix/profiles/system --list-generations
     to see system profiles, and then
     ls -l /nix/var/nix/profiles/system*link
     to see nixpkgs revision
     sudo nix-env --profile /nix/var/nix/profiles/system --delete-generations {a..b}
     and then sudo nix-collect-garbage to delete all the new garbage
     or sudo nix-collect-garbage -d to delete all profiles

     ncdu on /nix/store to find largest items, then
     nix-store --query --roots /nix/store/large-item
     to determine why it can't be GC'd, then delete roots and
     nix-store --delete /nix/store/large-item
     to target space hogs
  */

  nix = {
    buildMachines = [
      {
        hostName = "monolith";
        system = "x86_64-linux";
        maxJobs = 16;
        speedFactor = 2;
        supportedFeatures = [ "kvm" "big-parallel" ];
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
    xserver = {
      wacom.enable = true;
      libinput = {
        enable = true;
        naturalScrolling = true;
      };
    };
  };

  environment.systemPackages = [ pkgs.acpilight ];

  home-manager.users.lh = {
    profiles.user.xorg-base.enable = true;

    home = {
      packages = with pkgs; [ onboard ];
      sessionVariables.MOZ_USE_XINPUT2 = 1;
    };

    xsession.pointerCursor.size = 64;

    services.polybar.config."module/wlan".interface = "wlp1s0";
  };
}
