{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.sxhkd;
in {
  # TODO: https://github.com/nix-community/home-manager/pull/847#issuecomment-573397045
  options.profiles.user.sxhkd.enable = mkEnableOption "my sxhkd configuration";

  config = mkIf cfg.enable {
    services.sxhkd = {
      enable = true;
      # many keybindings are assigned in modules associated with the command.
      # use nixos-option, search the repo, or simply view the output sxhkdrc for
      # full list
      # also check window manager config for bindings
      keybindings = {
        "super + Return" = "$TERMINAL";
        "super + e" = "$EDITOR";
        "super + w" = "$BROWSER";
        "super + n" = "xournalpp";
        "super + p" = "keepassxc";
        "super + m" = "com.spotify.Client";
        "super + i" = "$TERMINAL -e htop";
        "super + s" = "pavucontrol";
        # toggle current dropdown
        "super + a" = "tdrop -am -w '-6' -x 3 -y 3 current";
        # fresh terminal dropdown
        "super + shift + a" = ''
          tdrop --auto-detect-wm --monitor-aware --width '-6' \
            --x-offset 3 --y-offset 3 \
            --program-flags "--title 'Alacritty (Dropdown)'" alacritty
        '';

        "super + Insert" = "showclip";
        "super + z" = "mpc pause; i3lock-fancy -pt ''; xset dpms force off";
        # logout with super + shift + z
        "super + b" = "polybar-msg cmd toggle";

        # Pause audio
        "super + {_,shift +} p" = "mpc {toggle,pause}";
        # Changes to next/previous tracks
        "super + {comma,period}" = "mpc {prev,next}";
        # Restart track
        "super + shift + less" = "mpc seek 0%";
        # Seek foward in song
        "super + {_,shift +} bracketright" = "mpc seek +{10,120}";
        # Seek backward in song
        "super + {_,shift +} bracketleft" = "mpc seek -{10,120}";

        "XF86Launch1" = "xset dpms force off";
        "XF86AudioMute" = "ponymix toggle";
        "{_, control, shift} + XF86AudioLowerVolume" =
          "ponymix decrease {5, 1, 10}";
        "{_, control, shift} + XF86AudioRaiseVolume" =
          "ponymix increase {5, 1, 10}";
        "{_, control, shift} + XF86MonBrightnessDown" =
          "xbacklight -dec {5, 1, 10}";
        "{_, control, shift} + XF86MonBrightnessUp" =
          "xbacklight -inc {5, 1, 10}";
      };
    };
  };
}
