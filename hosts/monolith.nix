{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.local-base.enable = true;

  system.stateVersion = "19.03";

  boot = {
    kernelParams = [
      "amd_iommu=on"
      # maximum breakup of IOMMU groups with ACS patch
      "pcie_acs_override=downstream,multifunction"
    ];
    kernelPackages = pkgs.linuxPackages_latest;
    kernelPatches = [{
      name = "acs";
      patch = pkgs.fetchpatch {
        url =
          "https://gitlab.com/Queuecumber/linux-acs-override/raw/master/workspaces/5.10.4/acso.patch";
        sha256 = "0qjb66ydbqqypyvhhlq8zwry8zcd8609y8d4a0nidhq1g6cp9vcw";
      };
    }];
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
      # rtx gpu
      alias pci:v000010DEd00001F02sv00001458sd000037C8bc03sc00i00 vfio-pci
      # bpx pro
      alias pci:v00001987d00005012sv00001987sd00005012bc01sc08i02 vfio-pci
      # front usb
      # alias pci:v00001022d000043D0sv00001849sd000043D0bc0Csc03i30 vfio-pci
      # rear usb
      alias pci:v00001022d0000145Fsv00001849sd0000FFFFbc0Csc03i30 vfio-pci

      # softdep nouveau pre: vfio-pci
      # softdep i2c_nvidia_gpu pre: vfio-pci

      # 21:00.0, .1, .2, .3 in IOMMU group 13
      # 22:00.0, .1 for second slot (580)
      # 20:00.0 for second M.2 slot (BPX Pro)
      # 03:00.0 for front USB controller: 1022:43d0
      # 23:00.3 for rear USB controller
      options vfio-pci ids=10de:1f02,10de:10f9,10de:1ada,10de:1adb,1002:67df,1002:aaf0,1987:5012,1022:145f
    '';
  };

  systemd = {
    services = let
      # unbind a PCI device from a particular driver and bind it to vfio-pci
      # this is useful when drivers are built into the kernel so they load
      # before a module like vfio-pci
      mkUnbindService = { enable, device-id, driver }: {
        inherit enable;
        wantedBy = [ "default.target" ];
        serviceConfig.Type = "oneshot";

        script = ''
          if test -d '/sys/bus/pci/drivers/${driver}/${device-id}'; then
            echo -n "${device-id}" > /sys/bus/pci/drivers/${driver}/unbind
            echo -n "${device-id}" > /sys/bus/pci/drivers/vfio-pci/bind
          fi
        '';
      };
    in {
      unbind-rtx-usb = mkUnbindService {
        enable = true;
        device-id = "0000:21:00.2";
        driver = "xhci_hcd";
      };
      unbind-bpx-ssd = mkUnbindService {
        enable = true;
        device-id = "0000:20:00.0";
        driver = "nvme";
      };
      unbind-front-usb = mkUnbindService {
        enable = false;
        device-id = "0000:03:00.0";
        driver = "xhci_hcd";
      };
      unbind-rear-usb = mkUnbindService {
        enable = true;
        device-id = "0000:23:00.3";
        driver = "xhci_hcd";
      };
    };
    # https://askubuntu.com/questions/676007/how-do-i-make-my-systemd-service-run-via-specific-user-and-start-on-boot
    # https://serverfault.com/questions/846441/loginctl-enable-linger-disable-linger-but-reading-linger-status
    tmpfiles.rules = [ "f /var/lib/systemd/linger/lh 0644 root root" ];
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
