{ config, lib, pkgs, ... }:

{

  networking.hostName = "Ishmael";
  time.timeZone = "Europe/Lisbon";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
    initrd.kernelModules = [ ];
    kernelParams = [ "i915.force_probe=46a6" ];
    kernelModules = [ "kvm-intel" ];
    kernelPackages = pkgs.linuxPackages_6_0;
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    nvidia.modesetting.enable = true;
    # nvidia.package = config.boot.kernelPackages.nvidiaPackages.production;
    nvidia.prime = {
      sync.enable = true;
      intelBusId  = "PCI:0:2:0";
      nvidiaBusId = "PCI:1:0:0";
    };
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  };
  services.xserver.videoDrivers = [ "intel" "nvidia" ];
  # services.xserver.config = "";
  # services.xserver.displayManager.xserverArgs = lib.mkAfter ["-config" "/home/matlad/empty"];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/04a8bc94-33a1-4fd7-b550-67e72ae1d5bc";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/0E22-5D33";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;
  powerManagement.cpuFreqGovernor = "powersave";
}
