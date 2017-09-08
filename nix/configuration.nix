# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.blacklistedKernelModules = [ "nouveau" ];

  networking.hostName = "nixos"; 
  networking.networkmanager.enable = true; 

  time.timeZone = "Europe/Moscow";

  nixpkgs.config.allowUnfree = true;
  
  nixpkgs.config.packageOverrides = pkgs: {
    bluez = pkgs.bluez5;
  };

  environment.systemPackages = with pkgs; [
    git
    emacs
    neovim
    chromium
    qbittorrent
    gimp
    sublime3
    tree

    konsole
    yakuake

    nox
    htop
    atool
    unrar
    unzip
    file
    python3
    #oraclejdk8
    clang
    cmake
    gnumake
    linuxPackages.perf
    patchelf
    aspell aspellDicts.en aspellDicts.ru

    xorg.xkbcomp
    xbindkeys
    pasystray
    wmctrl
    obconf
    compton
    xclip

    zlib
  ];
  
  hardware.opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
  
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.package = pkgs.pulseaudioFull;
  hardware.bluetooth.enable = true;

  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];

    displayManager.lightdm.enable = true;
    desktopManager.xfce = { 
      enable = true; 
      enableXfwm = false; 
    };
    windowManager.openbox.enable = true;
    synaptics = {
      enable = true;
      horizTwoFingerScroll = false;
      twoFingerScroll = true;
    };
  };

  fonts = {
    enableFontDir = true;
    enableDefaultFonts = true;
    fonts = with pkgs; [
      hack-font
      ubuntu_font_family
      inconsolata
    ];
  };

  programs.zsh = { enable = true; shellInit = "alias vim=nvim"; };
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  users.extraUsers.matklad = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
  };

  users.extraUsers.man = { isNormalUser = false; };

  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "65536";
  }];
  
  system.stateVersion = "17.09";

   environment.extraInit = let loader = "ld-linux-x86-64.so.2"; in ''
     export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/current-system/sw/lib:${pkgs.stdenv.cc.cc.lib}/lib:${pkgs.mesa}/lib:${pkgs.xorg.libX11}/lib:${pkgs.xorg.libXcursor}/lib:${pkgs.xorg.libXxf86vm}/lib:${pkgs.xorg.libXi}/lib:${pkgs.ncurses5}/lib"
     ln -fs ${pkgs.stdenv.cc.libc.out}/lib/${loader} /lib64/${loader}
   '';

}
