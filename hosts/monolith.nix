{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.base.enable = true;

  system.stateVersion = "19.03";

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-amd" ]; # required for virtualisation
    kernel.sysctl = { "net.ipv4.ip_forward" = 1; };
  };

  networking = {
    hostName = "monolith";
    bridges = {
      br25 = { interfaces = [ "enp25s0" ]; };
      br26 = { interfaces = [ "enp26s0" ]; };
    };
    interfaces = {
      # br25.ipv4.addresses = [{
      #   address = "192.168.1.8";
      #   prefixLength = 24;
      # }];
      br26.ipv4.addresses = [{
        address = "192.168.1.9";
        prefixLength = 24;
      }];
    };
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  virtualisation.libvirtd.enable = true;

  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.base.enable = true;
  };
}
