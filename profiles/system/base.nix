{ config, pkgs, lib, options, ... }:

with lib;

let cfg = config.profiles.system.base;
in {
  options.profiles.system.base.enable =
    mkEnableOption "my system base configuration";

  config = mkIf cfg.enable {
    environment = {
      extraInit = concatMapStringsSep "\n" (user: ''
        if [ "$(id -un)" = "${user}" ]; then
          . /etc/profiles/per-user/${user}/etc/profile.d/hm-session-vars.sh
        fi
      '') [ "lh" ];
      etc."nixos/overlays-compat/overlays.nix".text =
        builtins.readFile ./../../pkgs/overlays.nix;

      defaultPackages = [ ];
      shellAliases = lib.mkForce { };
    };

    nix = {
      nixPath = options.nix.nixPath.default
        ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];
      settings = {
        # required for remote builders
        trusted-users = [ "root" "@wheel" ];
        # conserve disk space by hardlinking identical store files
        auto-optimise-store = true;
      };
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

    # simple sudo alternative
    security.doas = {
      enable = true;
      extraRules = [{
        groups = [ "wheel" ];
        persist = true;
        keepEnv = true;
      }];
    };

    services.openssh = {
      enable = true;
      passwordAuthentication = false;
      kbdInteractiveAuthentication = false;
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
        "wheel" # root privileges
        "input" # uinput? steam controller?
      ];
    };

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;
    };
  };
}
