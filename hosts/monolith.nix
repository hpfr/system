{ config, pkgs, ... }:

{
  imports = [ ./hosts-base.nix ];

  profiles.system.local-base.enable = true;

  system.stateVersion = "19.03";

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelModules = [
      # required for virtualisation
      "kvm-amd"
      # fan control (k10temp for CPU automatically loaded)
      "nct6775"
    ];
  };

  # https://askubuntu.com/questions/676007/how-do-i-make-my-systemd-service-run-via-specific-user-and-start-on-boot
  # https://serverfault.com/questions/846441/loginctl-enable-linger-disable-linger-but-reading-linger-status
  systemd.tmpfiles.rules = [ "f /var/lib/systemd/linger/lh 0644 root root" ];

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

  virtualisation = {
    libvirtd = {
      enable = true;
      qemuOvmf = true;
      onBoot = "ignore";
      onShutdown = "shutdown";
    };
    vfio = {
      enable = true;
      IOMMUType = "amd";
      applyACSPatch = true;
      # https://wiki.archlinux.org/title/PCI_passthrough_via_OVMF#%22BAR_3:_cannot_reserve_[mem]%22_error_in_dmesg_after_starting_VM
      disableEFIFB = true;
      # reallocPCI = true;
      # can help with Windows BSOD issues
      ignoreMSRs = true;
      # rx 580 reset on shutdown
      enableVendorReset = true;
    };
  };

  users.extraUsers.lh.extraGroups = [ "libvirtd" "kvm" ];

  home-manager.users.lh = { config, pkgs, ... }: {
    profiles.user.local-base.enable = true;
  };
}
