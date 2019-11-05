{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./gui.nix
  ];

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "hal";
    interfaces.enp4s0.ipv4.addresses = [{
      address = "192.168.1.8";
      prefixLength = 24;
    }];
    defaultGateway = {
      address = "192.168.1.1";
      interface = "enp4s0";
    };
    nameservers = [ "1.1.1.1" "8.8.8.8" ];
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  services = {
    openssh.enable = true;
    xserver.videoDrivers = [ "nvidia" ];
  };

  home-manager.users.lh = { config, pkgs, ... }: {
    home.packages = with pkgs;
      [
        refind # boot into windows without keyboard
        # onlykey # doesn't work yet :/
      ];
    xsession.windowManager.i3 = {
      config.assigns = { "6" = [{ class = "^Firefox$"; }]; };
      # the dual monitor config I prefer
      extraConfig = ''
        workspace 1 output DP-0
        workspace 2 output DP-0
        workspace 3 output DP-0
        workspace 4 output DP-0
        workspace 5 output DP-0
        workspace 6 output HDMI-0
        workspace 7 output HDMI-0
        workspace 8 output HDMI-0
        workspace 9 output HDMI-0
        workspace 10 output HDMI-0
      '';
    };
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
