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
  };
}
