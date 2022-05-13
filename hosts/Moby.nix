{ config, lib, pkgs, ... }:

{

  networking.hostName = "Moby";
  time.timeZone = "Europe/Lisbon";

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  hardware.nvidia.modesetting.enable = true;
  services.xserver.videoDrivers = [ "nvidia" ];
  hardware.nvidia.prime = {
    sync.enable = true;
    nvidiaBusId = "PCI:1:0:0";
    intelBusId  = "PCI:0:2:0";
  };

  hardware = {
    bluetooth.enable = true;
    opengl = {
      enable = true;
      setLdLibraryPath = true;
      extraPackages = [
        pkgs.libGL
        pkgs.vaapiIntel
        pkgs.vaapiVdpau
        pkgs.libvdpau-va-gl
        pkgs.intel-media-driver
      ];
    };
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/20c4aff6-7290-4691-b380-329d04f78a74";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0818-EB3C";
      fsType = "vfat";
    };

  fileSystems."/home" =
    { device = "/dev/disk/by-uuid/c22f65e5-623b-4af0-b2fa-51b0a36090ea";
      fsType = "ext4";
    };

  swapDevices = [ ];

  powerManagement.cpuFreqGovernor = "powersave";
  hardware.enableRedistributableFirmware = true;
}
