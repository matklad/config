{ config, pkgs, ... }: {

  nix = {
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = ["nixpkgs=${pkgs.path}"];
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs = {
    fish.enable = true;
    java.enable = true;
    dconf.enable = true;
  };

  environment.systemPackages = with pkgs; [
    (pkgs.buildFHSUserEnv {
      name = "fhs";
      targetPkgs = pkgs: with pkgs; [
        alsaLib atk cairo cups dbus expat file fontconfig freetype glib
        libnotify libxml2 libxslt
        libGL vulkan-loader
        netcat nspr nss openjdk8 strace udev watch wget which xorg.libX11
        xorg.libXScrnSaver xorg.libXcomposite xorg.libXcursor xorg.libXdamage
        xorg.libXext xorg.libXfixes xorg.libXi xorg.libXrandr xorg.libXrender
        xorg.libXtst xorg.libxcb xorg.xcbutilkeysyms xorg.libxshmfence
        zlib fish
      ];
      runScript = "fish";
    })

    # GUI
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; })
    audacious
    chromium
    filelight
    gimp
    jetbrains.idea-community
    kdialog
    kitty
    obs-studio
    peek
    qbittorrent
    spectacle
    vscode
    kate

    # Comms
    slack
    tdesktop
    zoom-us

    # Viewers
    audacious
    mpv
    qpdfview
    qview
    smplayer
    vlc

    # Langs
    (python3.withPackages (py: [py.requests]))
    ant
    cmake
    deno
    gdb
    gnumake
    jekyll
    lldb_14
    llvmPackages_14.bintools
    llvmPackages_14.clang
    llvmPackages_14.stdenv
    maven
    ninja
    nodejs
    protobuf
    rustup
    valgrind

    # Archives
    ark
    unrar
    unzip
    atool
    zip
    p7zip

    # Utils
    asciidoctor
    asciinema
    binutils
    curl
    direnv
    entr
    exfat
    ffmpeg
    file
    git
    gitAndTools.gh
    gperftools
    graphviz
    google-cloud-sdk
    htop
    httpie
    hunspell
    hunspellDicts.en_US
    jq
    linuxPackages.perf
    micro
    neovim
    nox
    ntfs3g
    patchelf
    s-tui
    v4l-utils
    wally-cli
    wget
    xclip
    xorg.xkbcomp
    pciutils
    libimobiledevice

    # Rust stuff
    bat
    du-dust
    exa
    fd
    hyperfine
    ripgrep
    tokei
  ];

  services = {
    xserver = {
      enable = true;
      displayManager = {
        sddm = {
          enable = true;
        };
        defaultSession = "plasmawayland";
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
    earlyoom.enable = true;
    pcscd.enable = true;
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };
    usbmuxd.enable = true;
  };
  console.useXkbConfig = true;
  xdg = {
    portal = {
      enable = true;
      gtkUsePortal = true;
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
    keyboard.zsa.enable = true;
    pulseaudio = { enable = true; package = pkgs.pulseaudioFull; };
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
    networkmanager = {
      wifi.backend = "iwd";
      enable = true;
    };
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
      jetbrains-mono
      julia-mono
    ];
  };

  users = {
    defaultUserShell = "/run/current-system/sw/bin/fish";
    extraUsers.matklad = {
      isNormalUser = true;
      extraGroups = [ "wheel" "networkmanager" "docker" "audio" "plugdev"];
      uid = 1000;
    };
  };

  environment = {
    homeBinInPath = true;
    variables = {
      PATH = "$HOME/.cargo/bin";
      VISUAL = "micro";
      EDITOR = "micro";
      LIBCLANG_PATH = "${pkgs.llvmPackages_14.clang.cc.lib}/lib";
      RUST_BACKTRACE = "short";
    };
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
