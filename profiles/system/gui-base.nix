{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.gui-base;
in {
  options.profiles.system.gui-base.enable =
    mkEnableOption "my system-level GUI base configuration";

  config = mkIf cfg.enable {
    profiles.system = {
      local-base.enable = true;
      pipewire.enable = true;
      fonts.enable = true;
      remote-builds.enable = true;
    };

    nixpkgs = {

      config.allowUnfreePredicate = pkg:
        builtins.elem (lib.getName pkg) [
          "xow_dongle-firmware"
          # for steam hardware udev rules
          "steam-original"
        ];

      overlays = [
        # TODO: refactor and relocate scripts based on dependencies
        (self: super:
          let
            cyrus-sasl-xoauth2-src = super.fetchFromGitHub {
              owner = "moriyoshi";
              repo = "cyrus-sasl-xoauth2";
              rev = "36aabca54fd65c8fa7a707cb4936751599967904";
              sha256 = "02bjzydw7drskkn9v1wwc7f3i17r324lycv3gnsd129xq6w8fn9s";
            };
            cyrus_sasl_with_xoauth2 = super.cyrus_sasl.overrideAttrs
              (oldAttrs: {
                postInstall = ''
                  echo INSTALLING XOUATH2
                  mkdir -p cyrus-sasl-xoauth2
                  cp -t cyrus-sasl-xoauth2 ${cyrus-sasl-xoauth2-src}/*
                  cd cyrus-sasl-xoauth2
                  export NIX_CFLAGS_COMPILE="$NIX_CFLAGS_COMPILE -isystem $dev/include"
                  ./autogen.sh
                  ./configure --with-cyrus-sasl=$out
                  make
                  make install
                  cd ..
                '';
              });
          in {
            inherit cyrus_sasl_with_xoauth2;
            isync =
              super.isync.override { cyrus_sasl = cyrus_sasl_with_xoauth2; };
          })
        (self: super: {
          gui-scripts = super.runCommand "gui-scripts" {
            preferLocalBuild = true;
            allowSubstitutes = false;
          } ''
            shopt -s globstar
            for tool in ${./../../bin/gui}"/"**; do
              [ -f $tool ] && install -D -m755 $tool $out/bin/$(basename $tool)
            done
            patchShebangs $out/bin
          '';
        })
      ];
    };

    boot.extraModulePackages = with config.boot.kernelPackages;
      [ ddcci-driver ];
    boot.kernelModules = [ "i2c_dev" "ddcci" "ddcci_backlight" ];

    location.provider = "geoclue2"; # for redshift

    sound.enable = true;
    hardware = {
      bluetooth.enable = lib.mkDefault true;
      logitech.wireless = {
        enable = true;
        enableGraphical = true;
      };
      # steam controller
      steam-hardware.enable = true;
      # Xbox One wireless adapter
      xone.enable = true;
    };

    # passthrough USB devices to VM's over SPICE
    virtualisation.spiceUSBRedirection.enable = true;

    i18n.inputMethod.enabled = "ibus";
    i18n.inputMethod.ibus.engines = [ pkgs.ibus-engines.typing-booster ];

    environment.systemPackages = [ pkgs.bindfs ];
    fileSystems."/home/lh/bindfs/media" = {
      device = "/home/lh/media";
      fsType = "fuse.bindfs";
      options = [
        "ro"
        "owner"
        "uid=${toString config.users.users.lh.uid}"
        "gid=${toString config.users.groups.users.gid}"
        # TODO: set up autofs or afuse
        "noauto"
        "default_permissions"
        # only accessible to my user
        "no-allow-other"
        # resolve symlinks
        "resolve-symlinks"
      ];
    };

    # steam remote play
    networking.firewall = {
      allowedTCPPorts = [ 27036 ];
      allowedUDPPortRanges = [{
        from = 27031;
        to = 27036;
      }];
    };

    services = {
      logind.extraConfig = ''
        HandlePowerKey=suspend
      '';

      earlyoom.enable = true;

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

      dbus.packages = [ pkgs.dconf ];

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

      # libratbag for mouse configuration
      ratbagd.enable = true;

      # usbmuxd for libimobiledevice
      usbmuxd.enable = true;

      # for proprietary apps and some apps that don't play very well with nix
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
      seahorse.enable = true;
      corectrl.enable = true;
      gamemode.enable = true;
    };

    users = {
      groups.onlykey = { };
      users.lh.extraGroups = [ "onlykey" "corectrl" ];
    };
  };
}
