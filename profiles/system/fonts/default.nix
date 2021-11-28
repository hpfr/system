{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.fonts;
in {
  options.profiles.system.fonts.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    nixpkgs = {
      config.joypixels.acceptLicense = true;
      overlays = [
        (import ./iosevka.nix { inherit lib; })
        # TODO: remove nerd font icon usage in favor of emoji
        (self: super: {
          iosevka-nerd-font =
            super.nerdfonts.override { fonts = [ "Iosevka" ]; };
        })
        (self: super: {
          lato = super.lato.overrideAttrs (oldAttrs: {
            postFetch = ''
              mkdir -p $out/share/fonts
              unzip -j $downloadedFile \*.ttf -x \*Hairline\*.ttf -d $out/share/fonts/lato
            '';
            outputHash = "109pywbskq0f830ahrpgh4l56a0g9anzz0f12db2zhqlfi5gcbbw";
          });
        })
      ];
    };
    fonts = {
      enableDefaultFonts = false;
      fonts = with pkgs; [
        noto-fonts # no tofu
        noto-fonts-extra # all weights
        lato # sans-serif
        source-serif-pro
        source-sans-pro
        source-code-pro
        iosevka-term # monospace
        iosevka-term-curly-slab # monospace serif for emacs
        julia-mono # monospaced unicode
        joypixels # emoji
        undefined-medium # pseudo-bitmap
        # https://github.com/adobe-fonts/source-han-mono/issues/1
        source-han-sans # CJK support
        source-han-serif
        source-han-mono
        # computer modern but unicode
        cm_unicode
      ];

      # TODO: copy this option's functionality into home-manager module
      # TODO: figure out Noto fallbacks
      # https://docs.fedoraproject.org/en-US/packaging-guidelines/FontsPolicy/
      # https://src.fedoraproject.org/rpms/google-noto-fonts/c/6ec5a0916978c1565534038d084a88a9913f1837?branch=rawhide
      fontconfig.defaultFonts = {
        serif = [ "Source Serif 4 Variable" "Noto Serif" "Source Han Serif" ];
        sansSerif = [ "Lato" "Noto Sans" "Source Han Sans" ];
        monospace = [ "Iosevka Term" "Source Han Mono" "JuliaMono" ];
        emoji = [ "JoyPixels" ];
      };
    };
  };
}
