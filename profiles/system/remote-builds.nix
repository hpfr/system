{ config, lib, pkgs, ... }:

with lib;

let cfg = config.profiles.system.remote-builds;
in {
  options.profiles.system.remote-builds.enable =
    mkEnableOption "my remote builds configuration";

  config = mkIf cfg.enable {
    nix = {
      # don't include a remote builder in its own configuration
      buildMachines =
        builtins.filter (attrs: attrs.hostName != config.networking.hostName) [{
          hostName = "monolith";
          system = "x86_64-linux";
          maxJobs = 16;
          speedFactor = 2;
          supportedFeatures = [ "kvm" "big-parallel" ];
        }];
      # remote builds. override with: nrs --option builders ""
      # root's ~/.ssh/config must include the relevant config
      # add authorized public keys to remotes
      distributedBuilds = true;
      extraOptions = ''
        # use when remote builder has faster internet connection than local
        # otherwise local gets all dependencies and sends them to builder
        builders-use-substitutes = true
      '';
      settings = {
        # builder as remote substituter
        # enable with --option trusted-substituters "ssh-ng://host"
        trusted-substituters = builtins.filter
          (str: (builtins.match ".*${config.networking.hostName}$" str) == null)
          [ "ssh-ng://monolith" ];
        trusted-public-keys = builtins.filter
          (str: (builtins.match "^${config.networking.hostName}.*" str) == null)
          [ "monolith:qYcj/A6mRSPaaFn9sYYieWVY+0ZRPb2KavAJwYzTeJQ=" ];
      };
    };
  };
}
