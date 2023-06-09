{ config, lib, pkgs, ... }:

{
  networking.hostName = "Ishmael";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "i915.force_probe=46a6" ];
    extraModulePackages = config.boot.kernelPackages.acpi_call;
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };

  services.xserver.videoDrivers = [ "intel" ];
  # hardware.nvidia = {
  #   open = true;
  #   modesetting.enable = true;
  #   prime = {
  #     offload.enable = true;
  #     intelBusId  = "PCI:0:2:0";
  #     nvidiaBusId = "PCI:1:0:0";
  #   };
  # };

  boot.initrd.luks.devices."nixos".device = "/dev/disk/by-uuid/2fe2808b-7e2d-44ef-8f92-9340c040d1fc";
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/e1172ddb-975f-464c-bc94-cb84da90bf98";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0E22-5D33";
    fsType = "vfat";
  };

  swapDevices = [ ];
  system.stateVersion = "23.05";
}
