{ config, lib, pkgs, ... }:

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
          # # home-manager hasn't implemented yet
          # setEnv  = { "TERM" = "xterm-256color" };
          # # CSL doesn't support key auth :(
          # identityFile = "~/.ssh/kpxc-id.pub";
          # identitiesOnly = true;
        };
        engr = {
          hostname = "best-tux.cae.wisc.edu";
          user = "liam";
          # setEnv  = { "TERM" = "xterm-256color" };
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
        monolith = {
          hostname = "10.10.10.9";
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
        hal = {
          hostname = "10.10.10.8";
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
        poole = {
          hostname = "10.10.10.11";
          user = "lh";
          identityFile = "~/.ssh/kpxc-id.pub";
          identitiesOnly = true;
        };
      };
    };
  };
}
