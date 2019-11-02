{ config, pkgs, ... }:

{
  imports = [ ./base.nix ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [ "kvm-amd" ]; # required for virtualisation
    kernel.sysctl = { "net.ipv4.ip_forward" = 1; };
  };

  networking = {
    hostName = "monolith";
    bridges = {
      br25 = { interfaces = [ "enp25s0" ]; };
      br26 = { interfaces = [ "enp26s0" ]; };
    };
    interfaces = {
      # br25.ipv4.addresses = [{
      #   address = "192.168.1.8";
      #   prefixLength = 24;
      # }];
      br26.ipv4.addresses = [{
        address = "192.168.1.9";
        prefixLength = 24;
      }];
    };
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  services.openssh.enable = true;

  virtualisation = {
    libvirtd.enable = true;
    # docker.enable = true; # docker screws with networking, use a VM
  };

  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    home.packages = with pkgs;
      [
        # disable tty output so I can leave server hooked up to second monitor
        # without monitor switching to server VGA when desktop HDMI sleeps
        vbetool
      ];
  };
}
