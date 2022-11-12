{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.espanso;
in {
  options.profiles.user.espanso.enable =
    mkEnableOption "my espanso configuration";

  config = mkIf cfg.enable {
    services.espanso = {
      enable = true;
      settings = {
        matches = [
          {
            trigger = ":date";
            replace = "{{mydate}}";
            vars = [{
              name = "mydate";
              type = "date";
              params = { format = "%m/%d/%Y"; };
            }];
          }
          {
            trigger = ";lq";
            replace = "“";
          }
          {
            trigger = ";rq";
            replace = "”";
          }
          {
            trigger = ";la";
            replace = "‘";
          }
          {
            trigger = ";ra";
            replace = "’";
          }
          {
            trigger = ";'p";
            replace = "′";
          }
          {
            trigger = '';"p'';
            replace = "″";
          }
          {
            trigger = ";''";
            replace = "́";
          }
          {
            trigger = ";``";
            replace = "́";
          }
          {
            trigger = ";,,";
            replace = "̧";
          }
          {
            trigger = ";^^";
            replace = "̂";
          }
          {
            trigger = '';""'';
            replace = "̈";
          }
        ];
      };
    };
  };
}
