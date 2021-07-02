{ lib }:
self: super:
let
  variantDefaults = {
    # applies to all three slopes
    design = {
      # personal preference
      at = "short";
      # source code pro settings. source:
      # https://github.com/be5invis/Iosevka/blob/71db324837d7ddbdd61f913f625eea87f7bbe0ee/params/variants.toml
      capital-d = "more-rounded-serifless";
      capital-g = "toothless-corner-serifless-hooked";
      capital-k = "straight-serifless";
      a = "double-storey-serifless";
      d = "toothed-serifless";
      e = "flat-crossbar";
      f = "serifless";
      g = "double-storey";
      i = "hooky";
      j = "serifed";
      k = "straight-serifless";
      l = "tailed-serifed";
      u = "toothed";
      y = "straight-turn";
      eszet = "longs-s-lig";
      long-s = "flat-hook";
      # reduce ambiguity between a and α
      lower-alpha = "crossing";
      lower-lambda = "straight-turn";
      cyrl-capital-u = "straight-turn";
      zero = "dotted-oval";
      one = "base-flat-top-serif";
      two = "straight-neck";
      four = "closed";
      five = "oblique-upper-left-bar";
      six = "closed-contour";
      seven = "bend-serifless";
      eight = "crossing-asymmetric";
      nine = "closed-contour";
      asterisk = "penta-low";
      paragraph-sign = "low";
      number-sign = "slanted";
      dollar = "open";
    };
    italic = {
      a = "single-storey-serifless";
      g = "single-storey-serifless";
      i = "tailed-serifed";
      long-s = "flat-hook-tailed";
    };
  };
  variantSlabOverrides = {
    design = {
      capital-d = "more-rounded-bilateral-serifed";
      capital-g = "toothless-corner-serifed-hooked";
      capital-k = "straight-serifed";
      a = "double-storey-serifed";
      d = "toothed-serifed";
      f = "serifed";
      k = "straight-serifed";
    };
    italic = {
      a = "single-storey-serifed";
      g = "single-storey-serifed";
    };
  };
  # many from the linked variants.toml are omitted due to
  # variants.inherits = "ss20" handling them. the included ones are to
  # override the above ss09 overrides
  variantCurlyOverrides = {
    design = {
      capital-k = "curly-serifless";
      k = "curly-serifless";
      y = "curly-turn";
      lower-lambda = "curly-turn";
      cyrl-capital-u = "curly-turn";
      six = "open-contour";
      nine = "open-contour";
    };
    italic = {
      a = "single-storey-tailed";
      d = "tailed-serifless";
      e = "rounded";
      f = "flat-hook-tailed";
      k = "cursive-serifless";
      y = "cursive";
    };
  };
  variantCurlySlabOverrides = {
    design = {
      capital-k = "curly-serifed";
      k = "curly-serifed";
    };
    italic = { k = "cursive-serifed"; };
  };
  mkIosevka =
    { family, spacing ? "pseudo-mono", serifs ? "sans", variants ? { } }:
    super.iosevka.override {
      set = builtins.replaceStrings [ "iosevka " " " ] [ "" "-" ]
        (lib.toLower family);
      privateBuildPlan = {
        inherit family spacing serifs;
        variants = let
          slab = serifs == "slab";
          curly = variants != null && variants != { } && variants.inherits
            == "ss20";
          slabOverridden = if slab then
            lib.recursiveUpdate variantDefaults variantSlabOverrides
          else
            variantDefaults;
          curlyOverridden = if curly then
            lib.recursiveUpdate slabOverridden variantCurlyOverrides
          else
            slabOverridden;
          curlySlabOverridden = if slab && curly then
            lib.recursiveUpdate curlyOverridden variantCurlySlabOverrides
          else
            curlyOverridden;
        in lib.recursiveUpdate curlySlabOverridden variants;
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
}
