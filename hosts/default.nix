{ config, pkgs, ... }: {

  nix.extraOptions = "experimental-features = nix-command flakes";

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs = {
    fish.enable = true;
    java.enable = true;
  };

  environment.systemPackages = with pkgs; [
    # GUI
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = true; })
    chromium
    filelight
    gimp
    jetbrains.idea-community
    kitty
    obs-studio
    peek
    qbittorrent
    spectacle
    vscode
    kdialog

    # Comms
    slack
    tdesktop
    zoom-us

    # Viewers
    deadbeef-with-plugins
    mpv
    okular
    qview
    smplayer

    # Langs
    (python3.withPackages (py: [ py.requests ]))
    ant
    clang_12
    clang-tools
    cmake
    gdb
    gnumake
    jekyll
    lld_11
    lldb
    maven
    ninja
    nodejs-14_x
    rustup
    valgrind

    # Archives
    ark
    unrar
    unzip
    atool
    zip

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
    htop
    httpie
    jq
    linuxPackages.perf
    micro
    nox
    ntfs3g
    patchelf
    s-tui
    v4l-utils
    wally-cli
    wget
    xclip

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
        autoLogin = { enable = true; user = "matklad"; };
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
  };

  environment.variables = {
    PATH = "$HOME/.cargo/bin:$HOME/config/bin";
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
