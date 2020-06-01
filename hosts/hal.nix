{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.xorg-base.enable = true;

  system.stateVersion = "19.03";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hal";

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  nixpkgs.overlays = [
    (self: super: {
      quartus-prime-lite = super.quartus-prime-lite.override {
        supportedDevices = [ "Cyclone IV" ];
      };
    })
    (self: super: {
      openocd = super.openocd.overrideAttrs (oldAttrs: {
        configureFlags = oldAttrs.configureFlags or [ ] ++ [
          "--enable-maintainer-mode"
          "--enable-ti-icdi"
          "--enable-stlink"
        ];
      });
    })
  ];

  services = {
    xserver = {
      dpi = 96;
      videoDrivers = [ "nvidia" ];
    };
    udev.extraRules = ''
      # flash Tiva Launchpad without root permissions
      SUBSYSTEM=="usb", ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666"
    '';
  };

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.xorg-base.enable = true;

    home.packages = with pkgs; [
      refind # boot into windows without keyboard
      nvtop

      # beefy software suites
      libreoffice
      freecad
      blender
      kicad

      lutris
      chromium # browser games

      # MCU dev
      gcc-arm-embedded
      openocd
      picocom

      # closed-source
      quartus-prime-lite
    ];
    xsession = {
      initExtra = ''
        # G-Sync is enabled by default, causes stuttering
        nvidia-settings -a AllowVRR=0
        # S2417DG defaults to 60 Hz unfortunately
        # I have all kinds of problems with X. for some reason, one of my monitors won't wake from sleep on input
        # so I have to run xrandr to wake it. with dual monitors, for some reason X sometimes sets up panning
        # on the 1920x1080 monitor to make it 1440 pixels tall. this is sometimes unfixable
        xrandr --output DP-0 --primary --mode 2560x1440 --rate 144 --pos 0x0 \
               --output HDMI-0 --mode 1920x1080 --pos 2560x360

        emacs &
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
    services.polybar.config = {
      "module/wlan".interface = "wlp5s0";
      "module/eth".interface = "enp4s0";
    };
  };
}
