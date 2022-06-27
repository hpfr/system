{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.gthumb;
in {
  options.profiles.user.gthumb.enable =
    mkEnableOption "my gthumb configuration";

  config = mkIf cfg.enable {
    # # nix package currently lacks support for avif and jxl
    # # https://github.com/NixOS/nixpkgs/pull/153275
    # # https://github.com/NixOS/nixpkgs/pull/102189
    # home.packages = [ pkgs.gthumb ];

    xdg.mimeApps.defaultApplications = let
      applyToAll = list:
        builtins.listToAttrs (map (key: {
          name = key;
          value = "org.gnome.gThumb.desktop";
        }) list);
    in applyToAll [
      "image/gif"
      "image/jpeg"
      "image/png"
      "image/bmp"
      "image/tiff"
      "image/heic"
      "image/heif"
      "image/avif"
      "image/xpm"
      "image/svg+xml"
      "image/webp"
      "image/jxl"
      "image/x-eps"
      "image/x-bmp"
      "image/x-ico"
      "image/x-png"
      "image/x-pcx"
      "image/x-tga"
      "image/x-portable-bitmap"
      "image/x-portable-graymap"
      "image/x-portable-pixmap"
      "image/x-xbitmap"
      "image/x-xpixmap"
    ];
  };
}
