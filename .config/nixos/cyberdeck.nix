{ config, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  hardware = {
    firmware = with pkgs; [ mwlwifi ipts i915 mrvl ];
    acpilight.enable = true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_5_1;
    kernelPatches = [
      {
        name = "surface-acpi";
        patch = ./pkgs/linux/patches/0001-surface-acpi.patch;
      }
      {
        name = "surface-suspend";
        patch = ./pkgs/linux/patches/0002-suspend.patch;
      }
      {
        name = "surface-buttons";
        patch = ./pkgs/linux/patches/0003-buttons.patch;
      }
      {
        name = "surface-cameras";
        patch = ./pkgs/linux/patches/0004-cameras.patch;
      }
      {
        name = "surface-ipts";
        patch = ./pkgs/linux/patches/0005-ipts.patch;
      }
      {
        name = "surface-hid";
        patch = ./pkgs/linux/patches/0006-hid.patch;
      }
      {
        name = "surface-sd";
        patch = ./pkgs/linux/patches/0007-sdcard-reader.patch;
      }
      {
        name = "surface-wifi";
        patch = ./pkgs/linux/patches/0008-wifi.patch;
      }
      {
        name = "surface-mwlwifi";
        patch = ./pkgs/linux/patches/0010-mwlwifi.patch;
      }
    ];
    # extraModulePackages = [ pkgs.mwlwifi ]; # not sure of diff between this and hw.fw
    kernelModules = [ "hid" "hid_sensor_hub" "hid_generic" "usbhid" "hid_multitouch" "intel_ipts" ]; # surface_acpi not in lsmod?
  };

  i18n = {
    consoleFont = "latarcyrheb-sun32"; # large console font
  };

  networking.hostName = "cyberdeck"; # Define your hostname.

  services = {
    xserver = {
      wacom.enable = true;
      multitouch.enable = true;
      # dpi = 192; # doesn't seem to work with startx
    };
    redshift = {
      enable = true;
      temperature.night = 3000;
    };
  };

  environment.systemPackages = with pkgs; [
    libwacom
    acpilight
  ];

  environment.etc."systemd/sleep.conf".text = ''
    [Sleep]
    SuspendState=freeze
  '';
}
