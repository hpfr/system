{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.fonts;
in {
  options.profiles.system.fonts.enable = mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    nixpkgs.overlays = [
      (self: super:
        let
          characterOverrides = {
            # applies to all three slopes
            design = [
              "v-zero-dotted"
              "v-asterisk-hexlow"
              "v-paragraph-low"
              "v-at-short"
            ];

            upright = [ "v-g-doublestorey" ];
            italic = [ ];
            oblique = [ ];
          };
          mkIosevka = { family, design ? [ ], upright ? [ ], italic ? [ ]
            , oblique ? [ ] }:
            super.iosevka.override {
              set =
                builtins.replaceStrings [ " " ] [ "-" ] (lib.toLower family);
              privateBuildPlan = {
                family = family;
                design = design ++ characterOverrides.design;
                upright = upright ++ characterOverrides.upright;
                italic = italic ++ characterOverrides.italic;
                oblique = oblique ++ characterOverrides.oblique;
              };
            };
        in {
          iosevka = mkIosevka { family = "Iosevka"; };
          iosevka-term = mkIosevka {
            family = "Iosevka Term";
            design = [ "sp-term" ];
          };
          iosevka-slab = mkIosevka {
            family = "Iosevka Slab";
            design = [ "slab" ];
          };
          iosevka-term-slab = mkIosevka {
            family = "Iosevka Term Slab";
            design = [ "sp-term" "slab" ];
          };
          iosevka-curly = mkIosevka {
            family = "Iosevka Curly";
            design = "ss20";
          };
          iosevka-term-curly = mkIosevka {
            family = "Iosevka Term Curly";
            design = [ "sp-term" "ss20" ];
          };
          iosevka-curly-slab = mkIosevka {
            family = "Iosevka Curly Slab";
            design = [ "slab" "ss20" ];
          };
          iosevka-term-curly-slab = mkIosevka {
            family = "Iosevka Term Curly Slab";
            design = [ "sp-term" "slab" "ss20" ];
          };
          iosevka-etoile = mkIosevka {
            family = "Iosevka Etoile";
            design = [
              "type"
              "slab"
              "v-at-fourfold"
              "v-j-serifed"
              "no-cv-ss"
              "no-ligation"
            ];
            upright = [ "v-i-serifed" "v-l-serifed" ];
            italic = [ "v-i-italic" "v-l-italic" ];
            oblique = [ "v-i-serifed" "v-l-serifed" ];
          };
          iosevka-sparkle = mkIosevka {
            family = "Iosevka Sparkle";
            design = [
              "type"
              "v-at-fourfold"
              "v-j-narrow-serifed"
              "no-cv-ss"
              "no-ligation"
            ];
            upright =
              [ "v-i-serifed" "v-l-serifed" "v-f-serifed" "v-r-serifed" ];
            italic =
              [ "v-i-italic" "v-l-italic" "v-f-tailed" "v-r-top-serifed" ];
            oblique =
              [ "v-i-serifed" "v-l-serifed" "v-f-serifed" "v-r-serifed" ];
          };
        })
      (self: super: {
        iosevka-nerd-font = super.nerdfonts.override { fonts = [ "Iosevka" ]; };
      })
    ];
    fonts = {
      enableDefaultFonts = false;
      fonts = with pkgs; [
        noto-fonts # noto serif
        noto-fonts-extra # all weights
        lato # sans-serif
        iosevka-term-curly-slab # monospace
        joypixels # emoji
        undefined-medium # pseudo-bitmap
        noto-fonts-cjk # CJK support

        # TODO: https://github.com/ryanoasis/nerd-fonts/issues/479
        # https://github.com/ryanoasis/nerd-fonts/pull/461
        iosevka-nerd-font # icons

      ];

      # TODO: copy this option's functionality into home-manager module
      fontconfig.defaultFonts = {
        serif = [ "Noto Serif" ];
        sansSerif = [ "Lato" ];
        monospace = [ "Iosevka Term Curly Slab" ];
        emoji = [ "JoyPixels" ];
      };
    };
  };
}
