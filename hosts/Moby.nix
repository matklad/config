{ config, lib, pkgs, ... }:

{
  networking.hostName = "Moby";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };

  services.xserver.videoDrivers = [ "intel" ];

  boot.initrd.luks.devices."nixos".device = "/dev/disk/by-uuid/9cfc1da8-0dd0-48a5-ad10-a139f67a6658";
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/d00fb9b8-a74e-4bd3-8e1e-c7496f51f69d";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/23D6-A6D8";
    fsType = "vfat";
  };

  swapDevices = [ ];
}
