{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.local-base;
in {
  options.profiles.user.local-base.enable =
    mkEnableOption "my user-level base configuration for local machines";

  config = mkIf cfg.enable {
    profiles.user.base.enable = true;

    # user-level nixpkgs config for nix-shell, home-manager, non-NixOS
    # installations, etc
    xdg.configFile."nixpkgs/config.nix".text = ''
      {
        allowUnfree = true;
        joypixels.acceptLicense = true;
      }
    '';

    home = {
      packages = with pkgs; [
        # system-related
        # TODO: package as utils only? for relabeling
        # exfat # use exFAT-formatted drives
        ntfs3g # write to NTFS-formatted drives

        # CLI's
        # k2pdfopt # optimize pdf's for mobile devices NOTE: broken
        quickserve # host files (like reveal.js presentations)
      ];
    };

    services.syncthing.enable = true;
  };
}
