{ config, secrets, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.user.ssh;
in {
  options.profiles.user.ssh.enable = mkEnableOption "my ssh configuration";

  config = mkIf cfg.enable {
    programs.ssh = {
      enable = true;
      matchBlocks = {
        cs = {
          hostname = "best-linux.cs.wisc.edu";
          user = "hupfer";
          extraOptions.SetEnv = "TERM=xterm-256color";
          # CSL doesn't support key auth
        };
        engr = {
          hostname = "best-tux.cae.wisc.edu";
          user = "liam";
          extraOptions.SetEnv = "TERM=xterm-256color";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
        star-child = with secrets.star-child; {
          hostname = ip;
          port = sshPort;
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
        "*.lan" = {
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
      };
    };
  };
}
