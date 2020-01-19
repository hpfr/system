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
      minecraft
    ];
    xsession = {
      initExtra = ''
        emacs &
        i3-msg workspace 5
        firefox &
        i3-msg workspace 0
      '';
      windowManager.i3 = {
        config.assigns = { "5" = [{ class = "^Firefox$"; }]; };
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
      firefox.userChrome = ''
        /* hide the native tabs */
        #tabbrowser-tabs {
            visibility: hidden !important;
        }
        tab {
          display: none !important;
        }
        /* merge title bar and menu bar with nav bar */
        #main-window[sizemode="normal"] > #titlebar {
          -moz-appearance: initial !important; /* remove weird padding above tab bar in windowed mode */
        }
        /* #titlebar-buttonbox { */
        /* 	height: 32px !important; /1* make title bar 32px like others *1/ */
        /* 	background: var(--chrome-secondary-background-color) !important; /1* blend with nav bar *1/ */
        /* } */
        #toolbar-menubar{
          background: var(--toolbar-bgcolor) !important;
        }
        #TabsToolbar {
          height: 32px; /* make tab bar 32px like others (even though it is hidden, it controls the dynamic margin-bottom for the title bar) */
        }
        #nav-bar {
          /* padding-right: 168px; /1* leave space on right for window control buttons in title bar *1/ */
          margin-top: -32px; /* move the nav-bar up into the tab bar's spot */
          box-shadow: none !important; /* remove 1px line above*/
        }
        #main-window[privatebrowsingmode="temporary"] #nav-bar {
          padding-right: 32px !important; /* room for private browsing indicator */
        }

        #sidebar-splitter {
          width: 0px !important;
        }
        /* hide the sidebar header in Tree Style Tab */
        #sidebar-box[sidebarcommand="treestyletab_piro_sakura_ne_jp-sidebar-action"] #sidebar-header {
          display: none;
        }
      '';
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
