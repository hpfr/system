{ config, lib, pkgs, ... }:

{
  hardware = {
    # firmware = with pkgs; [ ipts ];
  };

  nixpkgs.overlays = [
    # only use for kernels <= 5.3
    (self: super: { ipts = super.callPackage ./pkgs/ipts { }; })
    # Limit patched libwacom to Xorg. Everything still works afaict
    (self: super: {
      # I believe this is for desktop environments that depend on
      # xf86inputlibinput, but otherwise the xorg overlay covers everything
      xf86inputlibinput =
        super.xf86inputlibinput.override { libinput = self.libinput-surface; };
      xorg = super.xorg // {
        xf86inputlibinput = super.xorg.xf86inputlibinput.override {
          libinput = self.libinput-surface;
        };
      };
      libinput-surface =
        super.libinput.override { libwacom = self.libwacom-surface; };
      libwacom-surface = super.libwacom.overrideAttrs (oldAttrs: {
        patches = oldAttrs.patches or [ ]
          ++ (map (name: ./pkgs/libwacom/patches + "/${name}")
            (builtins.attrNames (lib.filterAttrs (k: v: v == "regular")
              (builtins.readDir ./pkgs/libwacom/patches))));
      });
    })
    (self: super: {
      linux_4_19 = super.linux_4_19.override {
        # argsOverride = {
        #   version = "4.19.95";
        #   modDirVersion = "4.19.95";
        #   src = pkgs.fetchurl {
        #     url = "mirror://kernel/linux/kernel/v4.x/linux-4.19.95.tar.xz";
        #     sha256 = "1c2g5wcf4zgy5q51qrf0s4hf3pr1j8gi8gn27w8cafn1xqrcmvaa";
        #   };
        # };
        structuredExtraConfig = with lib.kernel; {
          INTEL_IPTS = module;
          INTEL_IPTS_SURFACE = module;
          SERIAL_DEV_BUS = yes;
          SERIAL_DEV_CTRL_TTYPORT = yes;
          SURFACE_SAM = module;
          SURFACE_SAM_SSH = module;
          SURFACE_SAM_SSH_DEBUG_DEVICE = yes;
          SURFACE_SAM_SAN = module;
          SURFACE_SAM_VHF = module;
          SURFACE_SAM_DTX = module;
          SURFACE_SAM_HPS = module;
          SURFACE_SAM_SID = module;
          SURFACE_SAM_SID_GPELID = module;
          SURFACE_SAM_SID_PERFMODE = module;
          SURFACE_SAM_SID_VHF = module;
          SURFACE_SAM_SID_POWER = module;
          INPUT_SOC_BUTTON_ARRAY = module;

          PINCTRL_INTEL = yes;
          PINCTRL_SUNRISEPOINT = yes;
        };
        # ignoreConfigErrors = true;
      };
    })
    (self: super: {
      linux_latest = super.linux_latest.override {
        structuredExtraConfig = with lib.kernel; {
          # Intel IPTS touchscreen
          TOUCHSCREEN_IPTS = module;

          # Surface Aggregator Module
          GPIO_SYSFS = yes; # req for SURFACE_SAM_HPS
          SURFACE_SAM = module;
          SURFACE_SAM_SSH = module;
          SURFACE_SAM_SSH_DEBUG_DEVICE = yes;
          SURFACE_SAM_SAN = module;
          SURFACE_SAM_VHF = module;
          SURFACE_SAM_DTX = module;
          SURFACE_SAM_HPS = module;
          SURFACE_SAM_SID = module;
          SURFACE_SAM_SID_GPELID = module;
          SURFACE_SAM_SID_PERFMODE = module;
          SURFACE_SAM_SID_VHF = module;
          SURFACE_SAM_SID_POWER = module;

          # other drivers
          INPUT_SOC_BUTTON_ARRAY = module;
        };
        # ignoreConfigErrors = true;
      };
    })
  ];

  boot = {
    # kernelPackages = pkgs.linuxPackages_4_19;
    kernelPackages = pkgs.linuxPackages_latest;
    # in case upgrade fails, comment out patches and kernel config, rebuild, and
    # then rebuild on the new gen and it should work
    # kernelPatches = [
    #   {
    #     name = "surface-buttons";
    #     patch = ./pkgs/linux/patches/4.19/0004-surface-buttons.patch;
    #   }
    #   {
    #     name = "surface-sam";
    #     patch = ./pkgs/linux/patches/4.19/0005-surface-sam.patch;
    #   }
    #   {
    #     name = "surface-suspend";
    #     patch = ./pkgs/linux/patches/4.19/0006-suspend.patch;
    #   }
    #   {
    #     name = "surface-ipts";
    #     patch = ./pkgs/linux/patches/4.19/0007-ipts.patch;
    #   }
    #   {
    #     name = "surface-ioremap-uc";
    #     patch = ./pkgs/linux/patches/4.19/0009-ioremap_uc.patch;
    #   }
    #   {
    #     name = "surface-wifi";
    #     patch = ./pkgs/linux/patches/4.19/0010-wifi.patch;
    #   }
    # ];
    kernelPatches = [
      {
        name = "surface-sam";
        patch = ./pkgs/linux/patches/5.6/0004-surface-sam.patch;
      }
      {
        name = "surface-wifi";
        patch = ./pkgs/linux/patches/5.6/0006-wifi.patch;
      }
      {
        name = "surface-ipts";
        patch = ./pkgs/linux/patches/5.6/0007-ipts.patch;
      }
    ];
    # not sure of diff between this and hw.fw
    # extraModulePackages = [ pkgs.mwlwifi ];
    kernelModules = [
      # surface_san not in lsmod?
      "hid"
      "hid_sensor_hub"
      "hid_generic"
      "usbhid"
      "hid_multitouch"
      # "intel_ipts"
      # "ipts_surface"
    ];
  };

}
