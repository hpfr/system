{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
  ];

  services.openssh.enable = true;
}
