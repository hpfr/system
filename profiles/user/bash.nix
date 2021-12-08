{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.bash;
in {
  options.profiles.user.bash.enable = mkEnableOption "my bash configuration";

  config = mkIf cfg.enable {
    programs.bash = {
      enable = true;
      shellOptions = [
        # Append to history file rather than replacing
        "histappend"
        # extended globbing
        "extglob"
        "globstar"
        # warn if closing shell with running jobs
        "checkjobs"
      ];
    };
  };
}
