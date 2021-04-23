{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.fonts;
in {
  options.profiles.system.fonts.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    nixpkgs = {
      config.joypixels.acceptLicense = true;
      overlays = [
        (self: super:
          let
            characterDefaults = {
              # applies to all three slopes
              design = {
                zero = "dotted";
                asterisk = "hexlow";
                paragraph-sign = "low";
                at = "short";
              };

              upright = { g = "double-storey"; };
              italic = { };
              oblique = { };
            };
            mkIosevka = { family, spacing, serifs, variants ? { } }:
              super.iosevka.override {
                set =
                  builtins.replaceStrings [ " " ] [ "-" ] (lib.toLower family);
                privateBuildPlan = {
                  family = family;
                  spacing = spacing;
                  serifs = serifs;
                  variants = mkMerge [ variants characterDefaults ];
                };
              };
          in {
            iosevka = mkIosevka { family = "Iosevka"; };
            iosevka-term = mkIosevka {
              family = "Iosevka Term";
              spacing = "term";
            };
            iosevka-slab = mkIosevka {
              family = "Iosevka Slab";
              serifs = "slab";
            };
            iosevka-term-slab = mkIosevka {
              family = "Iosevka Term Slab";
              spacing = "term";
              serifs = "slab";
            };
            iosevka-curly = mkIosevka {
              family = "Iosevka Curly";
              variants.inherits = "ss20";
            };
            iosevka-term-curly = mkIosevka {
              family = "Iosevka Term Curly";
              spacing = "term";
              variants.inherits = "ss20";
            };
            iosevka-curly-slab = mkIosevka {
              family = "Iosevka Curly Slab";
              serifs = "slab";
              variants.inherits = "ss20";
            };
            iosevka-term-curly-slab = mkIosevka {
              family = "Iosevka Term Curly Slab";
              spacing = "term";
              serifs = "slab";
              variants.inherits = "ss20";
            };
            iosevka-etoile = mkIosevka {
              family = "Iosevka Etoile";
              spacing = "quasi-proportional";
              serifs = "slab";
              no-cv-ss = true;
              no-ligation = true;
              variants = {
                design = {
                  at = "fourfold";
                  j = "serifed";
                };
                upright = {
                  i = "serifed";
                  l = "serifed";
                };
                italic = {
                  i = "italic";
                  l = "italic";
                };
                oblique = {
                  i = "serifed";
                  l = "serifed";
                };
              };
            };
            iosevka-sparkle = mkIosevka {
              family = "Iosevka Sparkle";
              spacing = "quasi-proportional";
              no-cv-ss = true;
              no-ligation = true;
              variants = {
                design = {
                  at = "fourfold";
                  j = "narrow-serifed";
                };
                upright = {
                  i = "serifed";
                  l = "serifed";
                  f = "serifed";
                  r = "serifed";
                };
                italic = {
                  i = "italic";
                  l = "italic";
                  f = "tailed";
                  r = "top-serifed";
                };
                oblique = {
                  i = "serifed";
                  l = "serifed";
                  f = "serifed";
                  r = "serifed";
                };
              };
            };
          })
        # TODO: remove nerd font icon usage in favor of emoji
        (self: super: {
          iosevka-nerd-font =
            super.nerdfonts.override { fonts = [ "Iosevka" ]; };
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
        iosevka-term-curly-slab # monospace
        joypixels # emoji
        undefined-medium # pseudo-bitmap
        # https://github.com/adobe-fonts/source-han-mono/issues/1
        source-han-sans # CJK support
        source-han-serif
        source-han-mono
        # TODO: https://github.com/ryanoasis/nerd-fonts/issues/479
        # https://github.com/ryanoasis/nerd-fonts/pull/461
        iosevka-nerd-font # icons
      ];

      # TODO: copy this option's functionality into home-manager module
      fontconfig.defaultFonts = {
        serif = [ "Source Serif Variable" ];
        sansSerif = [ "Lato" ];
        monospace = [ "Iosevka Term Curly Slab" ];
        emoji = [ "JoyPixels" ];
      };
    };
  };
}
