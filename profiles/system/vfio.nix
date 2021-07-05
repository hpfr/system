# source: https://gist.github.com/CRTified/43b7ce84cd238673f7f24652c85980b3
{ lib, pkgs, config, ... }:
with lib;
let cfg = config.virtualisation.vfio;
in {
  options.virtualisation.vfio = {
    enable = mkEnableOption "VFIO Configuration";
    IOMMUType = mkOption {
      type = types.enum [ "intel" "amd" ];
      example = "intel";
      description = "Type of the IOMMU used";
    };
    devices = mkOption {
      type = types.listOf (types.strMatching "[0-9a-f]{4}:[0-9a-f]{4}");
      default = [ ];
      example = [ "10de:1b80" "10de:10f0" ];
      description = "PCI IDs of devices to bind to vfio-pci";
    };
    disableEFIFB =
      mkEnableOption "blocking the usage of the EFI framebuffer on boot.";
    reallocPCI = mkEnableOption
      "the pci=realloc kernel parameter to alleviate hotplugging issues";
    ignoreMSRs = mkEnableOption "KVM guest access to model-specific registers";
    applyACSPatch = mkEnableOption
      "the ACS override kernel patch and kernel parameters for IOMMU group splitting";
    enableVendorReset = mkEnableOption
      "the vendor-reset kernel module and required kernel config";
  };

  config = mkIf cfg.enable {
    boot = {
      kernelParams = (if cfg.IOMMUType == "intel" then [
        "intel_iommu=on"
        "intel_iommu=igfx_off"
      ] else
        [ "amd_iommu=on" ]) ++ (optional (builtins.length cfg.devices > 0)
          ("vfio-pci.ids=" + builtins.concatStringsSep "," cfg.devices))
        ++ (optional cfg.applyACSPatch
          "pcie_acs_override=downstream,multifunction")
        ++ (optional cfg.disableEFIFB "video=efifb:off")
        ++ (optional cfg.reallocPCI "pci=realloc")
        ++ (optional cfg.ignoreMSRs "kvm.ignore_msrs");

      extraModulePackages = with config.boot.kernelPackages;
        optional cfg.enableVendorReset vendor-reset;

      kernelModules = [ "vfio_virqfd" "vfio_pci" "vfio_iommu_type1" "vfio" ]
        ++ (optional cfg.enableVendorReset "vendor-reset");

      kernelPatches = [
        (optionalAttrs cfg.applyACSPatch {
          name = "acs";
          patch = pkgs.fetchpatch {
            url =
              "https://gitlab.com/Queuecumber/linux-acs-override/raw/master/workspaces/5.10.4/acso.patch";
            sha256 = "0qjb66ydbqqypyvhhlq8zwry8zcd8609y8d4a0nidhq1g6cp9vcw";
          };
        })
        (optionalAttrs cfg.enableVendorReset {
          name = "config";
          patch = null;
          extraConfig = ''
            KALLSYMS_ALL y
          '';
        })
      ];
    };
  };
}
