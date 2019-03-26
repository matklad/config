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
  jumpapp = let
    runtimePath = pkgs.lib.makeSearchPath "bin" (with pkgs; [
      xdotool
      wmctrl
      xorg.xprop
      nettools
      perl
    ]);
    in
    pkgs.stdenv.mkDerivation rec {
      version = "1.0";
      name = "jumpapp-${version}";
      src = pkgs.fetchFromGitHub {
        owner = "matklad";
        repo = "jumpapp";
        rev = "d04e55af8e66087f68b9cf7817649236bf8be49b";
        sha256 = "118gbi8k31y11rkgjabj7ihb9z1lfkckhvr9ww2vybk411irghj3";
      };
      makeFlags = [ "PREFIX=$(out)" ];
      buildInputs = [ pkgs.perl pkgs.pandoc];
      postFixup = ''
        sed -i "2 i export PATH=${runtimePath}:\$PATH" $out/bin/jumpapp
        sed -i "2 i export PATH=${runtimePath}:\$PATH" $out/bin/jumpappify-desktop-entry
      '';
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
    jetbrains.idea-community

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
    jumpapp

    # Rust stuff
    ripgrep
    exa
    fd
    tokei

    xorg.xkbcomp
    xbindkeys

    xorg.xwininfo
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
        horizontalScrolling = false;
        naturalScrolling = true;
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
