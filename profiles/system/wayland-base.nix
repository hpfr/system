{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.wayland-base;
in {
  options.profiles.system.wayland-base.enable =
    mkEnableOption "my system-level Wayland base configuration";

  config = mkIf cfg.enable { profiles.system.gui-base.enable = true; };
}
