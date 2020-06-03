# Install from master:
#  nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -iA hello
#
# Install from local folder:
#  nix-env -f /home/matklad/projects/nixpkgs -iA jetbrains.idea-community

{ config, pkgs, ... }:
let
  # sudo nix-channel --add https://nixos.org/channels/nixos-unstable nixos-unstable
  # unstable = import <nixos-unstable> { config = config.nixpkgs.config; };

  vscodeInsiders = (pkgs.vscode.override { isInsiders = true; }).overrideAttrs(oldAttrs: rec {
    name = "vscode-insiders";
    src = pkgs.fetchurl {
      name = "VSCode_latest_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/latest/linux-x64/insider";
      hash = "sha256:0f5cy62c38yqhrhpngayvaa0qpnqlxzm7wnss3i68yv40lc8gj36";
    };
  });

  vscodeStable = pkgs.vscode.overrideAttrs(oldAttrs: rec {
    name = "vscode";
    version = "1.45.0";
    src = pkgs.fetchurl {
      name = "VSCode_latest_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
      hash = "sha256:16zchjp72m6n6za4ak5kn2ax1s5pjfn7l082d6gfbb2y62isvs7q";
    };
  });

  neovimNightly = (pkgs.neovim-unwrapped.overrideAttrs(oldAttrs: rec {
    src = pkgs.fetchFromGitHub {
      owner = "neovim";
      repo = "neovim";
      rev = "68546805790b5fd50e5e520a42dcf2e68c8fa4de";
      hash = "sha256:13l4k1r8f5wx8iaqf8yclcqwj2kd7d196x1p0ck45rfr3xnbgnr9";
    };
  }));
in
{
  imports = [
    /etc/nixos/hardware-configuration.nix
    ./host.nix
  ];

  programs = {
    dconf.enable = true;
    fish.enable = true;
    java.enable = true;
    # java.package = pkgs.jetbrains.jdk;
  };

  environment.systemPackages = with pkgs; [
    # GUI
    akregator
    chromium
    deadbeef-with-plugins
    filelight
    firefox-bin
    gimp
    gwenview
    kitty
    mpv
    okular
    peek
    qbittorrent
    smplayer
    spectacle
    tdesktop
    zoom-us
    signal-desktop
    obs-studio
    bluejeans

    jetbrains.idea-community
    vscodeStable
    neovimNightly

    # Langs
    (python3.withPackages (py: [ py.requests ]))
    ant
    gcc9Stdenv
    gcc9
    clang
    clang-tools
    cmake
    gdb
    gnumake
    jekyll
    lld
    lldb
    llvm
    maven
    ninja
    nodejs-10_x
    ruby_2_7
    bundler
    rustup

    # Utils
    ark
    asciinema
    asciidoctor
    aspell
    aspellDicts.en
    aspellDicts.ru
    atool
    binutils
    curl
    direnv
    exfat
    file
    flameGraph
    git
    git-hub
    gnupg
    gopass
    graphviz
    htop
    jumpapp
    linuxPackages.perf
    nox
    patchelf
    pkgconfig
    tightvnc
    tree
    unrar
    unzip
    wget
    xclip
    yubioath-desktop
    zip
    zlib

    # Rust stuff
    bat
    exa
    fd
    hyperfine
    ripgrep
    tokei

    ntfs3g
    xbindkeys
    xorg.xkbcomp
    xorg.xwininfo
  ];

  services = {
    xserver = {
      enable = true;
      videoDrivers = [ "modesetting" ];

      displayManager.sddm = {
        enable = true;
        autoLogin = { enable = true; user = "matklad"; };
      };
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
      secrets = [ "ipsec.d/ipsec.nm-l2tp.secrets" ];
    };
  };

  boot = {
    tmpOnTmpfs = true;
    loader = {
      timeout = 1;
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "nouveau" ];
    supportedFilesystems = [ "ntfs" ];
  };

  hardware = {
    pulseaudio = { enable = true; package = pkgs.pulseaudioFull; };
    bluetooth.enable = true;
  };

  # virtualisation.virtualbox.host = { enable = true; enableExtensionPack = true; };

  networking = {
    hostName = "nixos";
    networkmanager = {
      enable = true;
      enableStrongSwan = true;
    };
  };

  time.timeZone = "Europe/Berlin";

  nixpkgs.config = {
    allowUnfree = true;
  };

  systemd.extraConfig = "DefaultTimeoutStopSec=10s";

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
      jetbrains-mono
      nerdfonts
    ];
  };

  users = {
    defaultUserShell = "/run/current-system/sw/bin/fish";
    extraUsers.matklad = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" ];
      uid = 1000;
    };
    extraUsers.man.isNormalUser = false;
  };

  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/config/bin";
  };

  system.stateVersion = "18.09";
}
