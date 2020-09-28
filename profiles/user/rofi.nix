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

      # use this variable in scripts to generalize dmenu, rofi, etc
      sessionVariables.MENU = "rofi -dmenu";
    };

    services.sxhkd.keybindings = {
      "super + d" = "rofi -show combi";
      "super + r" = "rofi -show run";
      "super + shift + w" = "networkmanager_dmenu";
      "super + shift + d" = "displayselect";
      "super + grave" = "rofi-emoji";
      "super + x" = "prompt 'Shutdown computer?' 'shutdown -h now'";
      "super + shift + x" = "prompt 'Reboot computer?' 'reboot'";
      "super + shift + r" = "winresize";
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
