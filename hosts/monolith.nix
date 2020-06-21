{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.base.enable = true;

  system.stateVersion = "19.03";

  boot = {
    kernelParams = [
      "amd_iommu=on"
      # "video=efifb:off"
      # "video=vesafb:off,efifb:off"
      # "iommu=1"
      # below is alternative to modprobe
      # "rd.driver.pre=vfio-pci"
      # "vfio-pci.ids=1002:67df,1002:aaf0"
    ];
    kernelPackages = pkgs.linuxPackages_latest;
    # I shouldn't have to use this?
    # blacklistedKernelModules = [ "amdgpu" ];
    kernelModules = [
      # required for virtualisation
      "kvm-amd"
      # vfio modules
      "vfio_virqfd"
      "vfio_pci"
      "vfio_iommu_type1"
      "vfio"
    ];

    extraModprobeConfig = ''
      softdep amdgpu pre: vfio-pci
      options vfio-pci ids=1002:67df,1002:aaf0
    '';

    # # initrd version of modprobe above
    # initrd.availableKernelModules = [
    #   # shouldn't have to comment this out
    #   # "amdgpu"
    #   "vfio-pci"
    # ];
    # initrd.kernelModules = [
    #   # vfio modules
    #   "vfio_virqfd"
    #   "vfio_pci"
    #   "vfio_iommu_type1"
    #   "vfio"
    # ];
    # initrd.preDeviceCommands = ''
    #   DEVS="0000:21:00.0 0000:21:00.1"
    #   for DEV in $DEVS; do
    #     echo "vfio-pci" > /sys/bus/pci/devices/$DEV/driver_override
    #   done
    #   modprobe -i vfio-pci
    # '';
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
