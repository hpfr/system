{ config, pkgs, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./gui.nix
  ];

  system.stateVersion = "19.03";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking = {
    hostName = "hal";
    # interfaces.enp4s0.ipv4.addresses = [{
    #   address = "192.168.1.8";
    #   prefixLength = 24;
    # }];
    # defaultGateway = {
    #   address = "192.168.1.1";
    #   interface = "enp4s0";
    # };
    # nameservers = [ "1.1.1.1" "8.8.8.8" ];
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  services = {
    openssh.enable = true;
    xserver.videoDrivers = [ "nvidia" ];
  };

  home-manager.users.lh = { config, pkgs, ... }: {
    home.packages = with pkgs; [
      refind # boot into windows without keyboard
      # multibootusb # unmaintained
      # onlykey # doesn't work yet :/
      nvtop

      libreoffice
      # freecad
      blender
      kicad

      lutris
      minecraft
    ];
    xsession = {
      initExtra = ''
        # G-Sync is enabled by default, causes stuttering
        nvidia-settings -a AllowVRR=0
        # S2417DG defaults to 60 Hz unfortunately
        xrandr --output DP-0 --primary --mode 2560x1440 --rate 144 --pos 0x0 --rotate normal \
               --output HDMI-0 --mode 1920x1080 --pos 2560x360 --rotate normal

        # start dropdown terminal hidden
        tdrop --auto-detect-wm --monitor-aware --width '-6' \
              --x-offset 3 --y-offset 3 \
              --program-flags "--title 'Alacritty (Dropdown)'" alacritty

        sleep 0.1
        tdrop -am -w '-6' -x 3 -y 3 current
        sleep 0.1

        emacs &
        i3-msg 'workspace 5'
        firefox &
      '';
      windowManager.i3 = {
        # the dual monitor config I prefer
        extraConfig = ''
          workspace 0 output DP-0
          workspace 1 output DP-0
          workspace 2 output DP-0
          workspace 3 output DP-0
          workspace 4 output DP-0
          workspace 5 output HDMI-0
          workspace 6 output HDMI-0
          workspace 7 output HDMI-0
          workspace 8 output HDMI-0
          workspace 9 output HDMI-0
        '';
      };
    };
    programs = {
      rofi.extraConfig = ''
        rofi.dpi: 1
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
