{ config, lib, pkgs, ... }:

{
  networking.hostName = "Ishmael";
  time.timeZone = "Europe/Lisbon";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "i915.force_probe=46a6" ];
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    nvidia.open = true;
    nvidia.modesetting.enable = true;
    nvidia.prime = {
      offload.enable = true;
      intelBusId  = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
  services.xserver = {
    videoDrivers = [ "nvidia" ];
    displayManager = {
      defaultSession = "plasmawayland";
    };
  };


  fileSystems."/" =
    { device = "/dev/disk/by-uuid/e1172ddb-975f-464c-bc94-cb84da90bf98";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."nixos".device = "/dev/disk/by-uuid/2fe2808b-7e2d-44ef-8f92-9340c040d1fc";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0E22-5D33";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;
  powerManagement.cpuFreqGovernor = "powersave";
}
