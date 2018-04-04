# Install from master:
#
#  nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -iA hello
# Install from local folder:
#  nix-env -f /home/matklad/projects/nixpkgs -iA jetbrains.idea-community


{ config, pkgs, ... }:

{
  imports = [ /etc/nixos/hardware-configuration.nix ];

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
    # virtualbox.enableExtensionPack = true;
  };

  environment.systemPackages = with pkgs; [
    # GUI
    gwenview
    emacs
    neovim
    sublime3
    vscode
    qbittorrent
    gimp
    deadbeef-with-plugins
    filelight
    simplescreenrecorder
    spectacle
    smplayer mpv
    breeze-gtk breeze-qt5 breeze-icons breeze-grub
    gnome3.gnome_themes_standard
    firefox-bin
    yakuake
    okular
    networkmanagerapplet
    # jetbrains.idea-community

    # Langs
    python3
    cmake
    gnumake
    ninja
    gcc7
    gdb
    ant
    maven
    nodejs-8_x
    ghc

    # Utils
    git
    tree
    nox
    htop
    atool unrar zip unzip ark
    linuxPackages.perf
    patchelf
    aspell aspellDicts.en aspellDicts.ru
    pkgconfig
    fzf
    graphviz
    flameGraph

    # Rust stuff
    ripgrep
    exa
    fd

    xorg.xkbcomp
    xbindkeys

    wmctrl
    wget
    curl
    xclip
    zlib
    ntfs3g
  ];

  programs = {
    zsh = {
      enable = true;
      enableAutosuggestions = true;
      syntaxHighlighting.enable = true;
      shellInit = "alias vim=nvim";
    };
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

      synaptics = {
        enable = true;
        horizTwoFingerScroll = false;
        twoFingerScroll = true;
      };
    };
    unclutter.enable = true;
    printing.enable = true;
  };

  virtualisation = {
    virtualbox.host.enable = true;
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

  users = {
    defaultUserShell = "/run/current-system/sw/bin/zsh";
    extraUsers.matklad = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" ];
      uid = 1000;
    };
    extraUsers.man = { isNormalUser = false; };
  };

  security.pam.loginLimits = [{
    domain = "*";
    type = "soft";
    item = "nofile";
    value = "65536";
  }];

  system.stateVersion = "18.03";

  environment.extraInit = with pkgs; let loader = "ld-linux-x86-64.so.2"; in ''
    export PATH="$PATH:/home/matklad/.cargo/bin"
    ln -fs ${stdenv.cc.libc.out}/lib/${loader} /lib64/${loader}
  '';
}
