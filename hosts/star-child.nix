{ config, pkgs, secrets, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.base.enable = true;

  system.stateVersion = "20.09";

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/vda";
    };
  };

  networking = with secrets.wireguard; {
    hostName = "star-child";
    nat = {
      enable = true;
      externalInterface = "ens3";
      internalInterfaces = [ "wg0" ];
    };
    firewall.interfaces.ens3.allowedUDPPorts = [ listenPort ];
    wireguard = {
      interfaces.wg0 = {
        ips = [ star-child.ip ];
        listenPort = listenPort;
        privateKeyFile = "/home/lh/.config/wireguard/private";
        peers = [
          {
            publicKey = star-gate.publicKey;
            allowedIPs = [ star-gate.ip localSubnet ];
          }
          {
            publicKey = tibia.publicKey;
            allowedIPs = [ tibia.ip ];
          }
          {
            publicKey = dave.publicKey;
            allowedIPs = [ dave.ip ];
          }
        ];
      };
    };
  };

  services.openssh.ports = [ secrets.star-child.sshPort ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.base.enable = true;
  };
}
