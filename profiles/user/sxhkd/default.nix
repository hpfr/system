{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.sxhkd;
in {
  options.profiles.user.sxhkd.enable = mkEnableOption "my sxhkd configuration";

  config = mkIf cfg.enable {
    services.sxhkd = {
      enable = true;
      keybindings = {
        "super + Return" = "$TERMINAL";
        "super + d" = "rofi -show combi";
        "super + r" = "rofi -show run";
        "super + e" = "$EDITOR";
        "super + w" = "$BROWSER";
        "super + p" = "$READER";
        "super + n" = "xournalpp";
        # "super + m" = "com.spotify.Client";
        "super + i" = "$TERMINAL -e htop";
        "super + s" = "$TERMINAL -e pulsemixer";
        "super + shift + w" = "networkmanager_dmenu";
        "super + shift + d" = "displayselect";
        "super + shift + g" = "gimp";

        "super + grave" = "rofi-emoji";
        "super + Insert" = "showclip";
        "super + x" = "prompt 'Shutdown computer?' 'shutdown -h now'";
        "super + shift + x" = "prompt 'Reboot computer?' 'reboot'";
        "super + z" = "mpc pause; i3lock-fancy -pt ''; xset dpms force off";
        "super + b" = "polybar-msg cmd toggle";

        "super + shift + r" = "winresize";
      };
    };
  };
}
