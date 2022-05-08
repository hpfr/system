{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.fonts;
in {
  options.profiles.system.fonts.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    nixpkgs = {
      overlays = [
        (import ./iosevka.nix { inherit lib; })
        # ignore redundant ttf's, as well as variable ttf's which break latex usage
        (self: super: {
          stix-two = super.stix-two.overrideAttrs (oldAttrs: {
            postFetch = ''
              mkdir -p $out/share/fonts
              unzip -j $downloadedFile \*.otf -d $out/share/fonts/opentype
            '';
            outputHash = "sha256-9AZUQReu9bqhUJV33JIsCYrpfnBcGb9PqkpJeNVFSjc=";
          });
        })
      ];
    };
    fonts.enableDefaultFonts = false;
  };
}
