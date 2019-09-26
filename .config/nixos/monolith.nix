{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  boot.kernelModules = [ "kvm-amd" ]; # required for virtualisation
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
  };

  networking.hostName = "monolith";

  services.openssh.enable = true;

  virtualisation.libvirtd.enable = true;
  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];
}
