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

    nix.settings = {
      substituters = [ "https://nix-community.cachix.org" ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };

    nixpkgs = {
      config = {
        # steam, etc
        allowUnfree = true;
      };
      overlays = let
        rev = "13bd8f5d68519898e403d3cab231281b1fbd0d71";
        sha256 = "0d0zqgx0hschydz534cxx166k7vadb636glbgk6w48m9zvmzlxz1";
      in [
        (import (builtins.fetchTarball {
          url =
            "https://github.com/nix-community/emacs-overlay/archive/${rev}.tar.gz";
          inherit sha256;
        }))
      ];
    };

    boot = {
      # Use the systemd-boot EFI boot loader.
      loader = {
        systemd-boot.enable = true;
        efi.canTouchEfiVariables = true;
      };
      # ntfs write support
      supportedFilesystems = [ "ntfs-3g" ];
    };

    networking.networkmanager = {
      enable = true;
      # wifi.backend = "iwd";
    };

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
