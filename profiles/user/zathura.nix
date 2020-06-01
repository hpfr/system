{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.zathura;
in {
  options.profiles.user.zathura.enable = mkEnableOption ''
    Include my Zathura config.
  '';

  config = mkIf cfg.enable {
    home.sessionVariables.READER = "zathura";

    xdg.mimeApps.defaultApplications."application/pdf" =
      "org.pwmt.zathura.desktop";

    programs.zathura = {
      enable = true;
      options = {
        page-padding = 1;
        statusbar-h-padding = 0;
        statusbar-v-padding = 0;
        selection-clipboard = "clipboard";
      };
      extraConfig = ''
        map u scroll half-up
        map d scroll half-down
        map D toggle_page_mode
        map r reload
        map R rotate
        map K zoom in
        map J zoom out
        map i recolor
        map p print
      '';
    };
  };
}
