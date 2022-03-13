{ config, lib, pkgs, ... }:

with lib;
let cfg = config.profiles.system.pipewire;
in {
  options.profiles.system.pipewire.enable =
    mkEnableOption "my pipewire configuration";

  config = mkIf cfg.enable {
    security.rtkit.enable = true;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      jack.enable = true;
      pulse.enable = true;
      config = {
        pipewire."context.properties"."default.clock.rate" = "192000";
        pipewire-pulse."stream.properties"."resample.quality" = 15;
        client."stream.properties"."resample.quality" = 15;
        client-rt."stream.properties"."resample.quality" = 15;
      };
      media-session.config.bluez-monitor.properties = {
        "bluez5.headset-roles" = [ "hsp_hs" "hsp_ag" ];
        "bluez5.codecs" = [ "aac" "ldac" "aptx_hd" ];
      };
    };

    # aplay
    sound.enable = true;

    # gui patchbay
    environment.systemPackages = [ pkgs.helvum ];
  };
}
