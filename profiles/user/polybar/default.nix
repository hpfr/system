{ config, lib, nixos-config, pkgs, ... }:

with lib;

let cfg = config.profiles.user.polybar;
in {
  options.profiles.user.polybar.enable =
    mkEnableOption "my polybar configuration";

  config = mkIf cfg.enable {
    services.polybar = {
      enable = true;

      package = pkgs.polybar.override {
        mpdSupport = true;
        pulseSupport = true;
        i3GapsSupport = true;
      };

      config = {
        "bar/main" = {
          dpi = 0; # auto dpi
          height = if nixos-config.services.xserver.dpi == 192 then 34 else 24;
          font-0 = "undefined medium:size=10;2";
          font-1 = "Iosevka Nerd Font:size=10;3";
          # https://github.com/polybar/polybar/wiki/Known-Issues#huge-emojis
          font-2 = "Noto Color Emoji:scale=12;2";
          modules-right =
            "temperature cpu memory filesystem battery eth wlan backlight-acpi pulseaudio date";
        };
      };

      extraConfig = builtins.readFile ./default.conf;

      script = with pkgs; ''
        #!/usr/bin/env sh
        PATH=$PATH:${coreutils}/bin:${xorg.xrandr}/bin:${gnused}/bin
        pkill polybar >/dev/null
        while pgrep -u $(id -u) -x polybar > /dev/null; do sleep 1; done
        for m in $(xrandr --listactivemonitors | tail -n +2 | \
          cut -d ' ' -f 3 | sed 's/[*+]//g'); do
            MONITOR=$m polybar main &
        done
      '';
    };
  };
}
