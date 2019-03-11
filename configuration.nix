# Install from master:
#
#  nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -iA hello
# Install from local folder:
#  nix-env -f /home/matklad/projects/nixpkgs -iA jetbrains.idea-community

{ config, pkgs, ... }:
# sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
let
  unstable = import <nixos-unstable> {
    config = config.nixpkgs.config;
  };
in
{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot = {
    tmpOnTmpfs = true;
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    blacklistedKernelModules = [ "nouveau" ];
  };

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    extraHosts = import ./hosts.nix;
  };

  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    # GUI
    gwenview
    qbittorrent
    gimp
    deadbeef-with-plugins
    filelight
    simplescreenrecorder
    spectacle
    smplayer mpv
    firefox-bin
    chromium
    okular
    tdesktop
    zoom-us
    kitty
    obs-studio
    vscode

    # Langs
    python3
    cmake
    gnumake
    ninja
    gdb
    ant
    maven
    nodejs-10_x
    jekyll
    gcc
    unstable.rustup

    # Utils
    direnv
    git
    tree
    nox
    htop
    atool unrar zip unzip ark
    linuxPackages.perf
    patchelf
    aspell aspellDicts.en aspellDicts.ru
    pkgconfig
    graphviz
    flameGraph
    binutils
    vagrant
    exfat
    microcodeIntel
    asciidoctor

    # Rust stuff
    ripgrep
    exa
    fd
    tokei

    xorg.xkbcomp
    xbindkeys

    xdotool
    wmctrl
    wget
    curl
    xclip
    zlib
    ntfs3g
  ];

  programs = {
    fish.enable = true;		
    java.enable = true;
  };

  hardware = {
    pulseaudio.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;
    bluetooth.enable = true;
  };

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "intel" ];

      displayManager.sddm = {
        enable = true;
        autoLogin = {
          enable = true;
          user = "matklad";
        };
      };
      desktopManager.plasma5.enable = true;

      libinput = {
        enable = true;
        disableWhileTyping = true;
      };
    };
    unclutter.enable = true;
    printing.enable = true;
    emacs.enable = true;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

  fonts = {
    enableFontDir = true;
    enableDefaultFonts = true;
    fonts = with pkgs; [
      hack-font
      fira-code
      ubuntu_font_family
      inconsolata
    ];
  };

  users = {
    defaultUserShell = "/run/current-system/sw/bin/fish";
    extraUsers.matklad = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" ];
      uid = 1000;
    };
    extraUsers.man = { isNormalUser = false; };
  };

  system.stateVersion = "18.09";
}
