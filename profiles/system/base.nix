{ config, pkgs, lib, options, ... }:

with lib;

let cfg = config.profiles.system.base;
in {
  options.profiles.system.base.enable =
    mkEnableOption "my system base configuration";

  config = mkIf cfg.enable {
    environment.etc."nixos/overlays-compat/overlays.nix".text =
      builtins.readFile ./../../pkgs/overlays.nix;

    nix = {
      # required for remote builders
      trustedUsers = [ "root" "@wheel" ];
      nixPath = options.nix.nixPath.default
        ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];
      # conserve disk space by hardlinking identical store files
      autoOptimiseStore = true;
    };

    nixpkgs.overlays = [
      (self: super: {
        base-scripts = (super.runCommand "base-scripts" {
          preferLocalBuild = true;
          allowSubstitutes = false;
        } ''
          for tool in ${./../../bin/tools}"/"*; do
            install -D -m755 $tool $out/bin/$(basename $tool)
          done

          patchShebangs $out/bin
        '');
      })
    ];

    time.timeZone = "America/Chicago";

    programs = {
      # sets up fish as an available login shell
      fish.enable = true;
      ssh.startAgent = true;
    };

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
      permitRootLogin = "no";
      kexAlgorithms = [
        "curve25519-sha256"
        "curve25519-sha256@libssh.org"
        "diffie-hellman-group14-sha256"
        "diffie-hellman-group16-sha512"
        "diffie-hellman-group18-sha512"
        "diffie-hellman-group-exchange-sha256"
      ];
      macs = [
        "hmac-sha2-512-etm@openssh.com"
        "hmac-sha2-256-etm@openssh.com"
        "umac-128-etm@openssh.com"
      ];
      extraConfig = ''
        HostKeyAlgorithms ssh-ed25519,ssh-ed25519-cert-v01@openssh.com,sk-ssh-ed25519@openssh.com,sk-ssh-ed25519-cert-v01@openssh.com,rsa-sha2-256,rsa-sha2-512,rsa-sha2-256-cert-v01@openssh.com,rsa-sha2-512-cert-v01@openssh.com
      '';
    };

    # users.mutableUsers = false;
    # Don't forget to set a password with ‘passwd’.
    users.users.lh = {
      isNormalUser = true;
      shell = pkgs.fish;
      extraGroups = [
        "wheel" # sudo
        "input" # uinput? steam controller?
      ];
    };

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
    };
  };
}
