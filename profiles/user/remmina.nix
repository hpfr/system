{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.remmina;
in {
  options.profiles.user.remmina.enable =
    mkEnableOption "my Remmina configuration";

  config = mkIf cfg.enable {
    home.packages = [ pkgs.remmina ];

    xdg.mimeApps.defaultApplications = let
      applyToAll = list:
        builtins.listToAttrs (map (key: {
          name = key;
          value = "org.remmina.Remmina.desktop";
        }) list);
    in applyToAll [
      "x-scheme-handler/rdp"
      "x-scheme-handler/spice"
      "x-scheme-handler/vnc"
      "x-scheme-handler/remmina"
      "application/x-remmina"
    ];
  };
}
