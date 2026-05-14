{ config, pkgs, ... }:

{
  imports =
    [ /etc/nixos/hardware-configuration.nix ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # boot.kernelPackages = pkgs.linuxPackages_latest;

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
      git.enable = true;
  };
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
      kdePackages.kate
      vim
      gh
      vscode
      curl
      rustup
      htop
      gdb
      atool
      xz
      cmake
      ninja
      python3
      (pkgs.writeShellScriptBin "nixos-pull" ''
          set -ex
          NIXPKGS_REV=$(git ls-remote https://github.com/NixOS/nixpkgs/ nixos-25.11 | awk '{print $1}')
          NIXPKGS=$(nix-instantiate --eval -E "builtins.fetchTarball { url = \"https://github.com/NixOS/nixpkgs/archive/$NIXPKGS_REV.tar.gz\"; }" | tr -d \")

          CONFIG_REV=$(git ls-remote https://github.com/matklad/config HEAD | awk '{print $1}')
          CONFIG=$(nix-instantiate --eval -E "(builtins.fetchGit { url = \"https://github.com/matklad/config.git\"; rev=\"$CONFIG_REV\"; }).outPath" | tr -d \")

          sudo nixos-rebuild switch -I nixpkgs=$NIXPKGS -I nixos-config=$CONFIG
      '')
  ] ++ (with pkgs.llvmPackages_21; [
      bintools
      clang
      lldb
      llvm
  ]);

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
