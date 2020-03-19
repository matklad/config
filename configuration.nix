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
    buildInputs = [ pkgs.perl pkgs.pandoc ];
    postFixup = ''
        sed -i "2 i export PATH=${runtimePath}:\$PATH" $out/bin/jumpapp
        sed -i "2 i export PATH=${runtimePath}:\$PATH" $out/bin/jumpappify-desktop-entry
      '';
  };
  vscodeInsiders = (unstable.vscode.override { isInsiders = true; }).overrideAttrs(oldAttrs: rec {
    name = "vscode-insiders-${version}";
    version = "1553667544";

    src = pkgs.fetchurl {
      name = "VSCode_latest_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/latest/linux-x64/insider";
      sha256 = "0xclcz6x0290wsid99hrfgqym2wsw5nbxv81wg1db1c27m4f4pv2";
    };
  });
in
{
  imports = [ /etc/nixos/hardware-configuration.nix ];

  boot = {
    tmpOnTmpfs = true;
    loader = {
      timeout = 1;
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "nouveau" ];
  };

  # virtualisation.virtualbox.host.enable = true;
  # virtualisation.virtualbox.host.enableExtensionPack = true;

  networking = {
    hostName = "nixos";
    networkmanager = {
      enable = true;
      enableStrongSwan = true;
    };
    extraHosts = import ./hosts.nix;
    # firewall = { allowedTCPPorts = [ 4000 ]; };
  };

  time.timeZone = "Europe/Berlin";

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
    peek
    spectacle
    smplayer
    mpv
    firefox-bin
    chromium
    okular
    tdesktop
    zoom-us
    kitty
    obs-studio
    unstable.vscode
    vscodeInsiders
    jetbrains.idea-community
    libreoffice

    # Langs
    (python3.withPackages (py: [ py.requests ]))
    ruby
    cmake
    gnumake
    ninja
    gdb
    ant
    maven
    nodejs-10_x
    jekyll
    clang
    clang-tools
    rustup
    llvm
    lld
    lldb

    # Utils
    gnupg
    gopass
    direnv
    git
    git-hub
    tree
    nox
    htop
    atool
    unrar
    zip
    unzip
    ark
    linuxPackages.perf
    patchelf
    aspell
    aspellDicts.en
    aspellDicts.ru
    pkgconfig
    graphviz
    flameGraph
    binutils
    exfat
    asciidoctor
    jumpapp
    yubioath-desktop
    tightvnc
    file

    # Rust stuff
    ripgrep
    exa
    fd
    tokei
    bat
    hyperfine

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
    java = {
      enable = true;
      # package = pkgs.jetbrains.jdk;
    };
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

      displayManager.sddm.enable = true;
      desktopManager.plasma5.enable = true;

      libinput = {
        enable = true;
        disableWhileTyping = true;
        horizontalScrolling = false;
        naturalScrolling = true;
      };
      xkbOptions = "compose:ralt";
    };
    unclutter-xfixes.enable = true;
    printing.enable = true;
    emacs.enable = true;
    earlyoom.enable = true;
    pcscd.enable = true;
    strongswan = {
      enable = true;
      secrets = [
        "ipsec.d/ipsec.nm-l2tp.secrets"
      ];
    };
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
      noto-fonts
      noto-fonts-emoji
      iosevka
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
  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/config/bin";
  };

  system.stateVersion = "18.09";
}
