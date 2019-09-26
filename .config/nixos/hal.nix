{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./gui.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hal-nixos"; # Define your hostname.

  services.xserver.videoDrivers = [ "nvidia" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    programs.rofi.extraConfig = ''
      rofi.dpi: 96
    '';
    services = {
      polybar = {
        config = {
          "bar/main" = {
            height = 27;
            font-0 = "Hasklug Nerd Font:size=12;2";
            font-1 = "EmojiOne Color:size=12";
            font-2 = "unifont:fontformat=truetype:size=16:antialias=false;0";
            font-3 = "siji:pixelsize=16;1";
          };
        };
      };
    };
  };
}
