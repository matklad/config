{ config, pkgs, ... }:

{
  imports =
    [ ./hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "nixos";
  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "pt_PT.UTF-8";
    LC_IDENTIFICATION = "pt_PT.UTF-8";
    LC_MEASUREMENT = "pt_PT.UTF-8";
    LC_MONETARY = "pt_PT.UTF-8";
    LC_NAME = "pt_PT.UTF-8";
    LC_NUMERIC = "pt_PT.UTF-8";
    LC_PAPER = "pt_PT.UTF-8";
    LC_TELEPHONE = "pt_PT.UTF-8";
    LC_TIME = "pt_PT.UTF-8";
  };

  services.xserver.enable = true;
  services.displayManager.sddm.enable = true;
  services.desktopManager.plasma6.enable = true;

  services.xserver.xkb = {
    layout = "us";
    variant = "workman";
  };

  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };

  users.users.matklad = {
    isNormalUser = true;
    description = "matklad";
    extraGroups = [ "networkmanager" "wheel" ];
    shell = pkgs.fish;
  };

  programs = {
      fish.enable = true;
      firefox.enable = true;
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
      kdePackages.kate
      vim
      git
      gh
      vscode
      curl
      rustup
      llvmPackages_21.libcxxStdenv
      lldb
      htop
      curl
      gdb
      atool
      xz
      cmake
      ninja
      python3
      (pkgs.writeShellScriptBin "nixos-pull" ''
         set -ex
         curl -H 'Cache-Control: no-cache' \
            https://raw.githubusercontent.com/matklad/config/refs/heads/master/configuration.nix \
            | sudo tee /etc/nixos/configuration.nix > /dev/null
         sudo nixos-rebuild switch -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/nixos-25.05.tar.gz
      '')
  ];

  services = {
      openssh = {
          enable = true;
          settings = {
              PasswordAuthentication = false;
          };
      };
  };

  system.stateVersion = "25.05";
}
