{ config, lib, pkgs, ... }:

with lib;
let cfg = config.profiles.system.syncthing;
in {
  options.profiles.system.syncthing.enable =
    mkEnableOption "my system Syncthing configuration";

  config = mkIf cfg.enable {
    services.syncthing = {
      enable = true;
      # user service in home-manager
      systemService = false;
      openDefaultPorts = true;
    };
  };
}
