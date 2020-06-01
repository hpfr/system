{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.nomacs;
in {
  options.profiles.user.nomacs.enable =
    mkEnableOption "my nomacs configuration";

  config = {
    home.packages = [ pkgs.nomacs ];

    xdg.mimeApps.defaultApplications = let
      applyToAll = list:
        builtins.listToAttrs (map (key: {
          name = key;
          value = "nomacs.desktop";
        }) list);
    in applyToAll [
      "image/gif"
      "image/jpeg"
      "image/png"
      "image/bmp"
      "image/tiff"
      "image/x-eps"
      "image/x-ico"
      "image/x-portable-bitmap"
      "image/x-portable-graymap"
      "image/x-portable-pixmap"
      "image/x-xbitmap"
      "image/x-xpixmap"
      "image/heic"
      "image/heif"
    ];
  };
}
