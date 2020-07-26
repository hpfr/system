{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.base.enable = true;

  system.stateVersion = "19.03";

  boot = {
    kernelParams = [ "amd_iommu=on" ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [
      # required for virtualisation
      "kvm-amd"
      # vfio modules
      "vfio_virqfd"
      "vfio_pci"
      "vfio_iommu_type1"
      "vfio"
      # fan control (k10temp for CPU automatically loaded)
      "nct6775"
    ];

    extraModprobeConfig = ''
      softdep amdgpu pre: vfio-pci
      options vfio-pci ids=1002:67df,1002:aaf0
    '';
  };

  networking = {
    hostName = "monolith";
    bridges.br25.interfaces = [ "enp25s0" ];
    interfaces = {
      enp25s0.useDHCP = false;
      br25.useDHCP = true;
    };
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/cache-priv-key.pem
  '';

  services.openssh.enable = true;

  virtualisation.libvirtd = {
    enable = true;
    qemuOvmf = true;
    onBoot = "ignore";
    onShutdown = "shutdown";
  };

  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.base.enable = true;
  };
}
