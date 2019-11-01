{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./gui.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hal-nixos"; # Define your hostname.

  services.xserver.videoDrivers = [ "nvidia" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    # boot into windows without keyboard
    home.packages = with pkgs; [ refind ];
    programs = {
      rofi.extraConfig = ''
        rofi.dpi: 96
      '';
    };
    services = {
      polybar = {
        config = {
          "bar/main" = {
            height = 27;
            font-0 = "Hasklug Nerd Font:size=12;2";
            font-1 = "JoyPixels:size=12";
            modules-right =
              "temperature cpu memory filesystem eth wlan pulseaudio date";
          };
          "module/wlan".interface = "wlp5s0";
          "module/eth".interface = "enp4s0";
        };
      };
    };
  };
}
