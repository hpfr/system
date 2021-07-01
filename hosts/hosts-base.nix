{ config, lib, pkgs, ... }:

let secrets = import ../lib/secrets.nix;
in {
  _module.args.secrets = secrets;
  imports = [ /etc/nixos/hardware-configuration.nix <home-manager/nixos> ]
    ++ (map (name: (../profiles/system + "/${name}"))
      (builtins.attrNames (builtins.readDir ./../profiles/system)));

  home-manager.users.lh = let nixos-config = config;
  in {
    _module.args = {
      inherit secrets;
      nixos-config = nixos-config;
    };

    imports = (map (name: (../profiles/user + "/${name}"))
      (builtins.attrNames (builtins.readDir ../profiles/user)));
  };
}
