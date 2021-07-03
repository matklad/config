# Install witout channels:
#  nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -iA hello
#  nix-env -f /home/matklad/projects/nixpkgs -iA jetbrains.idea-community
#  nixos-rebuild -I nixpkgs=/home/matklad/projects/nixpkgs switch

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
    version = "1.50.0";
    src = pkgs.fetchurl {
      name = "VSCode_latest_linux-x64.tar.gz";
      url = "https://vscode-update.azurewebsites.net/${version}/linux-x64/stable";
      hash = "sha256:12nrv037an4f6h8hrbmysc0lk5wm492hywa7lp64n4d308zg5567";
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
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = true;})
    akregator
    chromium
    deadbeef-with-plugins
    filelight
    gimp
    jetbrains.idea-community
    kitty
    mpv
    obs-studio
    okular
    peek
    qbittorrent
    qview
    smplayer
    spectacle
    tdesktop
    vscode
    zoom-us

    # Langs
    (python3.withPackages (py: [ py.requests ]))
    ant
    clang_12
    clang-tools
    cmake
    gcc10
    gcc10Stdenv
    gdb
    gnumake
    jekyll
    julia-stable-bin
    lld_11
    lldb
    maven
    ninja
    nodejs-16_x
    ruby_2_7
    rustup

    # Utils
    ark
    asciidoctor
    asciinema
    atool
    binutils
    curl
    direnv
    exfat
    file
    git
    gitAndTools.gh
    gperftools
    graphviz
    htop
    jumpapp
    linuxPackages.perf
    micro
    nox
    ntfs3g
    patchelf
    pkgconfig
    s-tui
    unrar
    unzip
    wget
    xbindkeys
    xclip
    xorg.xkbcomp
    xorg.xwininfo
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
  ];

  services = {
    xserver = {
      enable = true;
      displayManager = {
        sddm.enable = true;
        autoLogin = { enable = false; user = "matklad"; };
      };
      desktopManager.plasma5.enable = true;

      libinput = {
        enable = true;
        touchpad = {
          disableWhileTyping = true;
          horizontalScrolling = false;
          naturalScrolling = true;
        };
      };

      layout = "us,ru";
      xkbVariant = "workman,";
      xkbOptions = "grp:win_space_toggle";
    };
    unclutter-xfixes.enable = true;
    printing.enable = true;
    emacs.enable = true;
    earlyoom.enable = true;
    pcscd.enable = true;
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };
  };
  console.useXkbConfig = true;

  boot = {
    tmpOnTmpfs = true;
    loader = {
      timeout = 1;
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "nouveau" ];
    supportedFilesystems = [ "ntfs" ];
    # kernelPackages = pkgs.linuxPackages_latest;
  };

  hardware = {
    keyboard.zsa.enable = true;
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
    docker = {
      enable = true;
      enableOnBoot = false;
    };
  };

  networking = {
    firewall.enable = true;
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
  };

  systemd.extraConfig = "DefaultTimeoutStopSec=10s";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "en_IE.UTF-8";
    };
  };

  fonts = {
    fontDir.enable = true;
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
      extraGroups = [ "wheel" "networkmanager" "docker" "audio" "plugdev"];
      uid = 1000;
    };
  # extraUsers.man.isNormalUser = false;
  };

  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/config/bin";
    RUSTC_FORCE_INCREMENTAL = "1";
    VISUAL = "micro";
    EDITOR = "micro";
  };

  security = {
    sudo.enable = false;
    doas = {
      enable = true;
      extraRules = [{ users = [ "matklad" ]; keepEnv = true; persist = true; }];
    };
    pam.loginLimits = [
      { domain = "*"; type = "soft"; item = "memlock"; value = "524288"; }
      { domain = "*"; type = "hard"; item = "memlock"; value = "524288"; }
    ];
  };

  system.stateVersion = "18.09";
  system.autoUpgrade.enable = false;
}
