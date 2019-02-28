# Install from master:
#
#  nix-env -f https://github.com/NixOS/nixpkgs/archive/master.tar.gz -iA hello
# Install from local folder:
#  nix-env -f /home/matklad/projects/nixpkgs -iA jetbrains.idea-community


{ config, pkgs, ... }:

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
    extraHosts = ''
        127.0.0.1 news.ycombinator.com
        127.0.0.1 meduza.io
        127.0.0.1 users.rust-lang.org
        # 127.0.0.1 internals.rust-lang.org
	127.0.0.1 avva.livejournal.com
	127.0.0.1 reddit.com
	127.0.0.1 www.reddit.com
	127.0.0.1 d3.ru
    '';
  };

  time.timeZone = "Europe/Moscow";

  nixpkgs.config = {
    allowUnfree = true;
    # packageOverrides = pkgs: { bluez = pkgs.bluez5; };
    # virtualbox.enableExtensionPack = true;
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
    # breeze-gtk breeze-qt5 breeze-icons breeze-grub
    # gnome3.gnome_themes_standard
    firefox-bin
    chromium
    okular
    networkmanagerapplet
    tdesktop
    # zoom-us
    kitty
    vscode
    obs-studio

    # Langs
    rustup
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
    binutils-unwrapped
    vagrant
    exfat
    microcodeIntel

    # Rust stuff
    ripgrep
    exa
    fd

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

      synaptics = {
        palmDetect = true;
        enable = true;
        horizTwoFingerScroll = false;
        twoFingerScroll = true;
      };
    };
    unclutter.enable = true;
    printing.enable = true;
    emacs.enable = true;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStopSec=10s
  '';

#  virtualisation = {
#    virtualbox.host.enable = true;
#  };

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

  # environment.extraInit = with pkgs; let loader = "ld-linux-x86-64.so.2"; in ''
  #   export PATH="$PATH:/home/matklad/.cargo/bin"
  #   export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/run/current-system/sw/lib"
  #   ln -fs ${stdenv.cc.libc.out}/lib/${loader} /lib64/${loader}
  # '';
}
