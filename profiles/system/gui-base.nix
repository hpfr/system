{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gui-base;
in {
  options.profiles.system.gui-base.enable =
    mkEnableOption "my system-level GUI base configuration";

  config = mkIf cfg.enable {
    profiles.system = {
      base.enable = true;
      fonts.enable = true;
    };

    nixpkgs.overlays = [
      # not sure why this isn't the default, KPXC has it as their default
      (self: super: {
        keepassxc = super.keepassxc.override { withKeePassNetworking = true; };
      })
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

    location.provider = "geoclue2"; # for redshift

    # Enable sound.
    sound.enable = true;
    hardware = {
      pulseaudio = {
        enable = true;
        package = pkgs.pulseaudioFull; # for bluetooth?
      };
      bluetooth.enable = lib.mkDefault true;
      opengl.driSupport32Bit = true; # for 32-bit games
      steam-hardware.enable = true;
    };

    # fix for virt-manager USB redirection
    security.wrappers.spice-client-glib-usb-acl-helper.source =
      "${pkgs.spice-gtk}/bin/spice-client-glib-usb-acl-helper";
    # packages that use polkit must be at system level
    environment.systemPackages = with pkgs; [ spice-gtk ];

    networking.firewall = {
      allowedTCPPorts = [
        # steam in-home streaming
        27036
        27037
        # barrier
        24800
      ];
      allowedTCPPortRanges = [{
        # steam login and download
        from = 27015;
        to = 27030;
      }];
      allowedUDPPorts = [
        # steam in-home streaming
        27031
        27036
        # steam client?
        4380
        # barrier?
        24800
      ];
      allowedUDPPortRanges = [
        # steam login and download
        {
          from = 27015;
          to = 27030;
        }
        # steam game traffic
        {
          from = 27000;
          to = 27100;
        }
      ];
    };

    services = {
      logind.extraConfig = ''
        HandlePowerKey=suspend
      '';

      dbus.packages = with pkgs; [ gnome3.dconf ];

      udev.extraRules = ''
        # UDEV Rules for OnlyKey, https://docs.crp.to/linux.html
        #
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{ID_MM_DEVICE_IGNORE}="1"
        ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", ENV{MTP_NO_PROBE}="1"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
        KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0666"
        #
        # If you share your linux system with other users, or just don't like the
        # idea of write permission for everybody, you can replace MODE:="0666" with
        # OWNER:="yourusername" to create the device owned by you, or with
        # GROUP:="somegroupname" and mange access using standard unix groups.
        #
        # One requirement of TOTP (Time-based One-time Password) is having the correct
        # time. If OnlyKey is used on a system where the OnlyKey app is not running it
        # will display “NOTSET” instead of the OTP code. Because OnlyKey has no battery
        # it requires an app to send it the correct time to be able to generate TOTP
        # codes. If you have OnlyKey command-line utility installed, adding the
        # following will automatically set the current time on OnlyKey every time you
        # plug it: RUN+="/usr/local/bin/onlykey-cli settime"
        #
        # SUBSYSTEMS=="usb", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="/usr/local/bin/onlykey-cli settime"
        # KERNEL=="ttyACM*", ATTRS{idVendor}=="1d50", ATTRS{idProduct}=="60fc", MODE:="0660", GROUP:="onlykey", RUN+="/usr/local/bin/onlykey-cli settime"
        #
        ##
      '';

      # for proprietary apps like Spotify, Discord, and Slack
      flatpak.enable = true;
    };

    # for Flatpak
    xdg.portal = {
      enable = true;
      extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    };
  };
}
