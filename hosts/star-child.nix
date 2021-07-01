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

  networking.hostName = "star-child";

  services.openssh.ports = [ secrets.star-child.sshPort ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.base.enable = true;
  };
}
