{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.autorandr;
in {
  options.profiles.user.autorandr.enable =
    mkEnableOption "my autorandr configuration";

  config = mkIf cfg.enable {
    programs.autorandr = {
      enable = true;
      profiles = let
        dell-1440p =
          "00ffffffffffff0010ace7a055504b30201c0104a5351e7806ee91a3544c99260f505421080001010101010101010101010101010101565e00a0a0a02950302035000f282100001a000000ff002341534e7a627a3668484c3764000000fd001e9022de3b010a202020202020000000fc0044656c6c20533234313744470a014f020312412309070183010000654b040001015a8700a0a0a03b50302035000f282100001a5aa000a0a0a04650302035000f282100001a6fc200a0a0a05550302035000f282100001a22e50050a0a0675008203a000f282100001e1c2500a0a0a01150302035000f282100001a0000000000000000000000000000000000000044";
        acer-1080p =
          "00ffffffffffff000472a601e02110053314010380331d782e77c5a557529c25115054bfef8081c08140714f81808100d1c001010101023a801871382d40582c4500fe1f1100001e000000fd00324b1e5010000a202020202020000000ff004c4e5a3038303032343231300a000000fc00416365722053323331484c0a20014d020314b24905040301121314101f65030c0010008c0ad08a20e02d10103e9600fe1f11000018011d007251d01e206e285500fe1f1100001e011d00bc52d01e20b8285540fe1f1100001e8c0ad090204031200c405500fe1f110000180000000000000000000000000000000000000000000000000000000000000000000000ed";
      in {
        amd-dual = {
          fingerprint = {
            DP-3 = dell-1440p;
            HDMI-1 = acer-1080p;
          };
          config = {
            DP-3 = {
              enable = true;
              primary = true;
              mode = "2560x1440";
              rate = "144";
              position = "0x0";
            };
            HDMI-1 = {
              enable = true;
              mode = "1920x1080";
              position = "2560x360";
            };
          };
        };
        amd-single = {
          fingerprint.DP-3 = dell-1440p;
          config.DP-3 = {
            enable = true;
            mode = "2560x1440";
            rate = "144";
          };
        };
        amd-vfio = {
          fingerprint.HDMI-1 = acer-1080p;
          config.HDMI-1 = {
            enable = true;
            mode = "1920x1080";
          };
        };
      };
    };
  };
}
