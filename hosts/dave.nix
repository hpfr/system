{ config, secrets, pkgs, lib, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.gnome.enable = true;

  hardware = {
    brillo.enable = true;
    bluetooth.enable = true;
    # orientation and ambient light sensors
    sensor.iio.enable = true;
  };

  system.stateVersion = "21.05";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = with secrets.wireguard; {
    hostName = "dave";
    firewall.interfaces.wlo1.allowedUDPPorts = [ listenPort ];
    wg-quick.interfaces.wg0 = {
      address = [ dave.ip ];
      listenPort = listenPort;
      privateKeyFile = "/home/lh/.config/wireguard/private";
      dns = [ secrets.star-gate.ip ];
      peers = [{
        publicKey = star-child.publicKey;
        allowedIPs = [ wgSubnet localSubnet ];
        endpoint = secrets.star-child.ip + ":" + toString listenPort;
        # deal with NAT
        persistentKeepalive = 25;
      }];
    };
  };

  services = {
    fstrim.enable = true;
    xserver = {
      wacom.enable = true;
      libinput = {
        enable = true;
        touchpad.naturalScrolling = true;
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
