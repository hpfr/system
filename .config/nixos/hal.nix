{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./common.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hal-nixos"; # Define your hostname.

  services.xserver.videoDrivers = [ "nvidia" ];
}
