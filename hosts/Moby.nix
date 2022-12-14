{ config, lib, pkgs, ... }:

{
  networking.hostName = "Moby";
  networking.networkmanager.wifi.powersave = false;
  time.timeZone = "Europe/Lisbon";

  boot = {
    initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "sd_mod" ];
    initrd.kernelModules = [ ];
    kernelModules = [ "kvm-intel" ];
    kernelParams = [ "i915.force_probe=46a6" ];
    kernelPackages = pkgs.linuxPackages_6_1;
    # kernelPackages = pkgs.linuxPackages_latest;
  };

  hardware = {
    bluetooth.enable = true;
    enableRedistributableFirmware = true;
    cpu.intel.updateMicrocode = true;
  };
  services.xserver.videoDrivers = [ "intel" ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/d00fb9b8-a74e-4bd3-8e1e-c7496f51f69d";
      fsType = "ext4";
    };

  boot.initrd.luks.devices."nixos".device = "/dev/disk/by-uuid/9cfc1da8-0dd0-48a5-ad10-a139f67a6658";

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/23D6-A6D8";
      fsType = "vfat";
    };

  swapDevices = [ ];

  networking.useDHCP = lib.mkDefault true;
  powerManagement.cpuFreqGovernor = "powersave";
}
