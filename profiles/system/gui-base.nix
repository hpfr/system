{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gui-base;
in {
  options.profiles.system.gui-base.enable =
    mkEnableOption "my system-level GUI base configuration";

  config = mkIf cfg.enable {
    profiles.system = {
      local-base.enable = true;
      # pipewire.enable = true;
      fonts.enable = true;
      remote-builds.enable = true;
    };

    nixpkgs.overlays = [
      # TODO: refactor and relocate scripts based on dependencies
      (self: super: {
        gui-scripts = (super.runCommand "gui-scripts" {
          preferLocalBuild = true;
          allowSubstitutes = false;
        } ''
          shopt -s globstar
          for tool in ${./../../bin/gui}"/"**; do
            [ -f $tool ] && install -D -m755 $tool $out/bin/$(basename $tool)
          done
          patchShebangs $out/bin
        '');
      })
    ];

    boot.extraModulePackages = with config.boot.kernelPackages;
      [ ddcci-driver ];
    boot.kernelModules = [ "i2c_dev" "ddcci" "ddcci_backlight" ];

    location.provider = "geoclue2"; # for redshift

    sound.enable = true;
    hardware = {
      pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull; # for bluetooth?
      };
      bluetooth.enable = lib.mkDefault true;
      logitech.wireless = {
        enable = true;
        enableGraphical = true;
      };
    };

    # passthrough USB devices to VM's over SPICE
    virtualisation.spiceUSBRedirection.enable = true;

    services = {
      logind.extraConfig = ''
        HandlePowerKey=suspend
      '';

      xserver = {
        enable = true;
        layout = "us";
        libinput.enable = true;
        displayManager.gdm.enable = true;
      };

      # support for color profiles
      colord.enable = true;
      # brightness control via ambient lighting
      clight.enable = false;

      cpupower-gui.enable = true; # manage CPU performance

      blueman.enable = true;

      dbus.packages = with pkgs; [ gnome.dconf ];

      udev.extraRules = ''
        # ddcutil without root
        # Assigns the i2c devices to group i2c, and gives that group RW access:
        KERNEL=="i2c-[0-9]*", GROUP="i2c", MODE="0660"
        #, PROGRAM="${pkgs.ddcutil}/bin/ddcutil --bus=%n getvcp 0x10

        # UDEV Rules for OnlyKey, https://docs.crp.to/linux.html
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{ID_MM_DEVICE_IGNORE}="1"
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{MTP_NO_PROBE}="1"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
        KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="${pkgs.onlykey-cli}/bin/onlykey-cli settime"
        KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="${pkgs.onlykey-cli}/bin/onlykey-cli settime"
        #
      '';

      # Xbox One Wireless adapter
      hardware.xow.enable = true;

      # libratbag for mouse configuration
      ratbagd.enable = true;

      # usbmuxd for libimobiledevice
      usbmuxd.enable = true;

      # for proprietary apps like Spotify, Discord, and Slack
      flatpak.enable = true;
    };

    # for Flatpak
    xdg.portal = {
      enable = true;
      extraPortals = optional (!config.profiles.system.gnome.enable)
        pkgs.xdg-desktop-portal-gtk;
      gtkUsePortal = false;
    };

    programs = {
      ssh.askPassword = "${pkgs.gnome.seahorse}/libexec/seahorse/ssh-askpass";
      corectrl.enable = true;
      steam = {
        # TODO: allow opening firewall without enabling steam
        enable = true;
        remotePlay.openFirewall = true;
      };
      gamemode.enable = true;
    };

    users = {
      groups.onlykey = { };
      users.lh.extraGroups = [ "onlykey" "corectrl" ];
    };
  };
}
