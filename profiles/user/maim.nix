{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.maim;
in {
  options.profiles.user.maim.enable = mkEnableOption "my maim configuration";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.maim ];
    # https://github.com/naelstrof/maim/issues/172
    services.picom.blurExclude = [ "class_g = 'slop'" ];
    services.sxhkd.keybindings = {
      "Print" =
        ''maim ~/documents/pictures/"$(date '+%F-%Hh%Mm%S')"-screenshot.png'';
      "shift + Print" =
        if config.profiles.user.rofi.enable then "maimpick" else null;
      # F2 is restart in i3
      # Change display
      "super + F3" = "displayselect";
      # Hibernate
      "super + F4" = "prompt 'Hibernate computer?' 'sudo systemctl suspend'";
      # Restart/rescan wifi/eth networks
      "super + F5" = "sudo -A systemctl restart NetworkManager";
      # Mount a USB drive
      "super + F9" = "menumount";
      # Unmount a USB drive
      "super + F10" = "menuumount";
      # Network Manager interface
      "super + F12" = "$TERMINAL -e nmtui";
    };
  };
}
