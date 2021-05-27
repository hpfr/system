{ config, pkgs, lib, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.gnome.enable = true;

  hardware = {
    brillo.enable = true;
    bluetooth.enable = true;
    # orientation and ambient light sensors
    sensor.iio.enable = true;
  };

  system.stateVersion = "20.09";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "dave";

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

  environment.systemPackages = with pkgs; [ libwacom ];

  home-manager.users.lh = {
    profiles.user = {
      gnome.enable = true;
      email.enable = true;
    };

    # touch gestures in firefox
    home = {
      sessionVariables.MOZ_USE_XINPUT2 = 1;
      packages = [ pkgs.libinput ];
    };
  };
}
