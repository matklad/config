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
    version = "1.48.0";
    src = pkgs.fetchurl {
      name = "VSCode_latest_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
      hash = "sha256:0rrs2v97xhlxyjvipss5dmk88j7b03kyszwyhy46755954nzm85j";
    };
  });
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
    maven
    ninja
    nodejs-10_x
    ruby_2_7
    rustup

    # Utils
    ark
    asciidoctor
    asciinema
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
    gitAndTools.gh
    gnupg
    gopass
    gperftools
    graphviz
    htop
    jumpapp
    julia
    linuxPackages.perf
    nox
    patchelf
    pkgconfig
    s-tui
    tightvnc
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
    du-dust

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
    opengl = {
      enable = true;
      setLdLibraryPath = true;
      extraPackages = [
        pkgs.libGL
        pkgs.vaapiIntel
        pkgs.vaapiVdpau
        pkgs.libvdpau-va-gl
        pkgs.intel-media-driver
      ];
    };
  };

  virtualisation = {
    # virtualbox.host = { enable = true; enableExtensionPack = true; };  
    docker.enable = true;
  };

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
      # nerdfonts
    ];
  };

  users = {
    defaultUserShell = "/run/current-system/sw/bin/fish";
    extraUsers.matklad = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "docker" ];
      uid = 1000;
    };
    extraUsers.man.isNormalUser = false;
  };

  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/config/bin";
  };

  system.stateVersion = "18.09";
  system.autoUpgrade.enable = true;
}
