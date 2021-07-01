{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.local-base.enable = true;

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
      softdep nouveau pre: vfio-pci
      softdep i2c_nvidia_gpu pre: vfio-pci

      # 21:00.0, .1, .2, .3 in IOMMU group 13
      options vfio-pci ids=10de:1f02,10de:10f9,10de:1ada,10de:1adb
    '';
  };

  # the driver that binds to the USB controller in my passthrough GPU is built
  # into the kernel, so we have to unbind it and bind the vfio driver manually
  systemd.services.unbind-rtx-usb = {
    enable = true;
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";

    script = let device-id = "0000:21:00.2";
    in ''
      if test -d '/sys/bus/pci/drivers/xhci_hcd/${device-id}'; then
        echo -n "${device-id}" > /sys/bus/pci/drivers/xhci_hcd/unbind
        echo -n "${device-id}" > /sys/bus/pci/drivers/vfio-pci/bind
      fi
    '';
  };

  networking = {
    hostName = "monolith";
    bridges.br25.interfaces = [ "enp25s0" ];
    interfaces = {
      enp25s0.useDHCP = false;
      br25.useDHCP = true;
    };
    firewall.allowedTCPPorts = [
      5900 # spice
    ];
  };

  nix.extraOptions = ''
    secret-key-files = /home/lh/.ssh/cache-priv-key.pem
  '';

  services = {
    fstrim.enable = true;
    openssh.enable = true;
  };

  virtualisation.libvirtd = {
    enable = true;
    qemuOvmf = true;
    onBoot = "ignore";
    onShutdown = "shutdown";
  };

  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.local-base.enable = true;
  };
}
