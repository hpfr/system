{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.fonts;
in {
  options.profiles.system.fonts.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    nixpkgs = {
      overlays = [
        (import ./iosevka.nix { inherit lib; })
        # TODO: remove nerd font icon usage in favor of emoji
        (self: super: {
          iosevka-nerd-font =
            super.nerdfonts.override { fonts = [ "Iosevka" ]; };
        })
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
    fonts = {
      enableDefaultFonts = false;
      fonts = with pkgs; [
        noto-fonts # no tofu
        noto-fonts-extra # all weights
        last-resort # final fallback
        source-serif
        source-sans
        source-code-pro
        stix-two
        libertinus
        liberation_ttf # kaobook
        iosevka-term # monospace
        iosevka-term-curly-slab # monospace serif for emacs
        julia-mono # monospaced unicode
        noto-fonts-emoji # emoji
        undefined-medium # pseudo-bitmap
        # https://github.com/adobe-fonts/source-han-mono/issues/1
        source-han-sans # CJK support
        source-han-serif
        source-han-mono
      ];
    };
  };
}
