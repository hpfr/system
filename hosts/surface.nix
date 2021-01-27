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

  boot.kernelPackages = pkgs.linuxPackages_5_9;

  console.font = "latarcyrheb-sun32"; # large console font

  networking = {
    hostName = "surface";
    networkmanager.wifi = {
      powersave = false;
      scanRandMacAddress = false;
    };
  };

  services = {
    fstrim.enable = true;
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
