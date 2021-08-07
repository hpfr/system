{ config, pkgs, lib, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.gnome.enable = true;

  # nixpkgs.config.kodi.enableSteamLauncher = true;

  system.stateVersion = "21.05";

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "poole";

  services = {
    fstrim.enable = true;
    xserver.displayManager.autoLogin = {
      enable = true;
      user = "lh";
    };
  };

  home-manager.users.lh = { config, pkgs, lib, ... }: {
    profiles.user.gnome.enable = true;
    home.packages = [ pkgs.kodi-wayland ];

    # never lock screen
    dconf.settings."org/gnome/desktop/screensaver".lock-enabled = false;
  };
}
