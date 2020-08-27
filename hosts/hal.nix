{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.xorg-base.enable = true;

  system.stateVersion = "19.03";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hal";

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
    xserver.dpi = 96;
    udev.extraRules = ''
      # flash Tiva Launchpad without root permissions
      SUBSYSTEM=="usb", ATTRS{idVendor}=="1cbe", ATTRS{idProduct}=="00fd", MODE="0666"
    '';
  };

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.xorg-base.enable = true;

    home.packages = with pkgs; [
      nvtop

      # beefy software suites
      # libreoffice
      # freecad
      blender
      kicad
      inkscape

      lutris
      chromium # browser games

      # MCU dev
      gcc-arm-embedded
      openocd
      picocom

      # closed-source
      quartus-prime-lite
    ];

    services.polybar.config = {
      "module/wlan".interface = "wlp5s0";
      "module/eth".interface = "enp4s0";
    };
  };
}
