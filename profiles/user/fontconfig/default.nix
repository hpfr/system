{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.fontconfig;
in {
  options.profiles.user.fontconfig.enable =
    mkEnableOption "my font configuration";

  config = mkIf cfg.enable {
    fonts.fontconfig.enable = true;

    xdg.configFile."fontconfig" = {
      source = ./files;
      recursive = true;
    };
    # also sets dconf
    gtk.font = {
      name = "Sans";
      size = 11;
    };
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        monospace-font-name = "Monospace 11";
        document-font-name = "Sans 11";
      };
      "org/gnome/desktop/wm/preferences".titlebar-uses-system-font = true;
    };

    home.packages = with pkgs; [
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
      noto-fonts # no tofu
      noto-fonts-extra # all weights
      last-resort # final fallback
    ];
  };
}
