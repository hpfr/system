{ config, lib, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  # profiles.system.xorg-base.enable = true;
  # profiles.system.sway.enable = true;
  profiles.system.gnome.enable = true;

  hardware = {
    brillo.enable = true;
    bluetooth.enable = false;
  };

  system.stateVersion = "21.05";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "moon-watcher";

  nix.extraOptions = ''
    secret-key-files = /home/lh/.ssh/cache-priv-key.pem
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
    cpupower-gui.enable = lib.mkForce false; # virtual machine
    clight.enable = lib.mkForce false;
    fstrim.enable = true;
    xserver.dpi = 96;
    udev.extraRules = ''
      # flash Tiva Launchpad without root permissions
      SUBSYSTEM=="usb", ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666"
    '';
    qemuGuest.enable = true;
  };

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user = {
      # xorg-base.enable = true;
      # sway.enable = true;
      gnome.enable = true;
      email.enable = true;
    };

    home = {
      packages = with pkgs; [
        # TODO: package tuxclocker?
        radeontop # monitor AMD GPU utilization
        openrgb # turn off RGB hardware

        # beefy software suites
        # libreoffice
        # freecad
        # blender
        kicad
        inkscape

        chromium # browser testing

        # MCU dev
        # TODO: make into shell.nix
        gcc-arm-embedded
        openocd
        picocom

        # closed-source
        # quartus-prime-lite
      ];
    };

    wayland.windowManager.sway.config.startup = [{
      command = ''
        ${pkgs.sway}/bin/swaymsg "output DP-3 pos 0 0 mode 2560x1440@144Hz"
      '';
    }];

    xsession.windowManager.i3 = {
      extraConfig = ''
        workspace 0 output DP-3
        workspace 1 output DP-3
        workspace 2 output DP-3
        workspace 3 output DP-3
        workspace 4 output DP-3
        workspace 5 output HDMI-1
        workspace 6 output HDMI-1
        workspace 7 output HDMI-1
        workspace 8 output HDMI-1
        workspace 9 output HDMI-1
      '';
    };

    services.polybar.config = {
      "module/wlan".interface = "wlp5s0";
      "module/eth".interface = "enp4s0";
    };
  };
}
