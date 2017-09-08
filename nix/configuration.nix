
{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];
  nix.nixPath = [ "/home/matklad/" "nixos-config=/etc/nixos/configuration.nix" ];

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    blacklistedKernelModules = [ "nouveau" ];
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: { bluez = pkgs.bluez5; };
  };

  environment.systemPackages = with pkgs; [
    git
    emacs
    neovim
    chromium
    qbittorrent
    gimp
    sublime3
    deadbeef-with-plugins
    filelight

    tree
    nox
    htop
    atool
    unrar
    unzip
    file
    python3
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
    xclip

    zlib
  ];

  hardware = {
    opengl.extraPackages = with pkgs; [ vaapiIntel libvdpau-va-gl vaapiVdpau ];
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    bluetooth.enable = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];

    displayManager.sddm.enable = true;
    desktopManager.lxqt.enable = true;

    synaptics = {
      enable = true;
      horizTwoFingerScroll = false;
      twoFingerScroll = true;
    };
  };
  environment.lxqt.excludePackages = with pkgs.lxqt; [ qlipper ];


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

  environment.extraInit = with pkgs; let loader = "ld-linux-x86-64.so.2"; in ''
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/current-system/sw/lib:${stdenv.cc.cc.lib}/lib:${mesa}/lib:${xorg.libX11}/lib:${xorg.libXcursor}/lib:${xorg.libXxf86vm}/lib:${xorg.libXi}/lib:${ncurses5}/lib"
    ln -fs ${stdenv.cc.libc.out}/lib/${loader} /lib64/${loader}
  '';
}
