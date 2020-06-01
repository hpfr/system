{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.rofi;
in {
  options.profiles.user.rofi.enable = mkEnableOption "my rofi configuration";

  config = mkIf cfg.enable {
    programs.rofi = {
      enable = true;
      theme = "Arc-Dark";
      extraConfig = ''
        rofi.modi: window,run,ssh,drun,combi
        rofi.combi-modi: window,drun
        rofi.dpi: 1
      '';
    };

    home = {
      # connect to wifi
      packages = with pkgs; [ networkmanager_dmenu ];

      sessionVariables = {
        # use this variable in scripts to generalize dmenu, rofi, etc
        MENU = "rofi -dmenu";
        # use in scripts called from dmenu or sxhkd
        SUDO_ASKPASS = "${pkgs.gui-scripts}/bin/menupass";
      };
    };

    programs.fish.loginShellInit = ''
      set MENU rofi -dmenu
    '';

    # https://github.com/firecat53/networkmanager-dmenu
    xdg.configFile."networkmanager-dmenu/config.ini".text = ''
      [dmenu]
      dmenu_command = rofi
      l = 40
      rofi_highlight = True

      [dmenu_passphrase]
      rofi_obscure = True

      [editor]
      gui_if_available = True
    '';
  };
}
