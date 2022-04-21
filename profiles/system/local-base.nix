{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.local-base;
in {
  options.profiles.system.local-base.enable =
    mkEnableOption "my system-level base configuration for local machines";

  config = mkIf cfg.enable {
    profiles.system = {
      base.enable = true;
      syncthing.enable = true;
    };

    boot.loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };

    networking.networkmanager.enable = true;

    # ddcutil i2c group
    users.groups.i2c = { };
    users.users.lh.extraGroups = [
      "networkmanager" # networking
      "video"
      "dialout" # serial ports for MCU programming
      "lp" # printing?
      "i2c" # ddcutil
    ];

    # persistent ssh from iOS
    programs.mosh.enable = true;

    # fast locate implementation with daily updatedb
    services.locate = {
      enable = true;
      locate = pkgs.plocate;
      # not supported by plocate
      localuser = null;
    };
  };
}
