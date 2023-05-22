{ config, pkgs, ... }: {

  nix = {
    extraOptions = "experimental-features = nix-command flakes";
    nixPath = ["nixpkgs=${pkgs.path}"];
    gc = {
      automatic = true;
      options = "--max-freed 1G --delete-older-than 7d";
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
  };

  programs = {
    fish.enable = true;
    java.enable = true;
    dconf.enable = true;
    ssh.askPassword = "ksshaskpass";
  };

  environment.systemPackages = with pkgs; [
    (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
     pkgs.buildFHSUserEnv (base // {
       name = "fhs";
       targetPkgs = pkgs: (base.targetPkgs pkgs) ++ [pkgs.pkg-config];
       profile = ''export FHS=1'';
       runScript = "fish";
       extraOutputsToInstall = ["dev"];
     }))

    # GUI
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; })
    filelight
    gimp
    kdialog
    kitty
    obs-studio
    peek
    kooha
    qbittorrent
    spectacle
    vscode

    # Comms
    tdesktop
    zoom-us
    slack

    # Viewers
    audacious
    qpdfview
    qview
    vlc
    ibus-engines.table-others

    # Langs
    (python3.withPackages (py: [py.requests]))
    ant
    cmake
    deno
    elan
    gdb
    gnumake
    lldb_14
    llvmPackages_latest.bintools
    llvmPackages_latest.clang
    llvmPackages_latest.stdenv
    lua5_4
    maven
    nil
    ninja
    nodejs
    protobuf
    rustup
    valgrind
    zig

    # Archives
    ark
    unrar
    unzip
    atool
    zip
    p7zip

    # Utils
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
    hunspell
    hunspellDicts.en_US
    jq
    libimobiledevice
    linuxPackages.perf
    nox
    ntfs3g
    patchelf
    pciutils
    rr
    s-tui
    shellcheck
    v4l-utils
    wally-cli
    wget
    wl-clipboard

    # Rust stuff
    bat
    du-dust
    exa
    fd
    gitui
    helix
    hyperfine
    ripgrep
    tokei
  ];

  services = {
    emacs.enable = true;

    logind = {
      extraConfig = ''
        IdleAction=poweroff
        IdleActionSec=90min
      '';
    };

    xserver = {
      enable = true;
      displayManager = {
        sddm.enable = true;
        autoLogin = {enable = true; user = "matklad";};
        defaultSession = "plasmawayland";
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

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
    };

    unclutter-xfixes.enable = true;
    printing.enable = true;
    earlyoom.enable = true;
    pcscd.enable = true;
    openssh = {
      enable = true;
      settings.PasswordAuthentication = false;
    };
    usbmuxd = {
      enable = true;
      package = pkgs.usbmuxd2;
    };
    kanata = {
      enable = true;
      keyboards.laptop = {
        devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
        config = builtins.readFile ./kanata.kbd;
      };
    };
  };
  console.useXkbConfig = true;
  xdg = {
    portal = {
      enable = true;
    };
  };

  boot = {
    tmp.useTmpfs = true;
    loader = {
      timeout = 1;
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
    blacklistedKernelModules = [ "nouveau" ];
    supportedFilesystems = [ "ntfs" ];
    kernelPackages = pkgs.linuxPackages_6_2;
    kernel.sysctl."kernel.perf_event_paranoid" = 1;
  };

  hardware = {
    keyboard.zsa.enable = true;
    ledger.enable = true;
  };

  virtualisation = {
    # virtualbox.host = { enable = true; enableExtensionPack = true; };
    docker = {
      enable = true;
      enableOnBoot = false;
    };
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 10100 ];
      allowedUDPPorts = [ 10100 ];
    };
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
    inputMethod = {
      enabled = "ibus";
      ibus.engines = [pkgs.ibus-engines.uniemoji pkgs.ibus-engines.table pkgs.ibus-engines.table-others];
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
      extraGroups = [ "wheel" "networkmanager" "docker" "audio" "plugdev" ];
      uid = 1000;
    };
  };

  environment = {
    enableDebugInfo = true;
    homeBinInPath = true;
    sessionVariables = {
      # NIXOS_OZONE_WL = "1";
      PATH = "$HOME/.cargo/bin";
      VISUAL = "hx";
      EDITOR = "hx";
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
    rtkit.enable = true;
  };

  system.stateVersion = "23.05";
  system.autoUpgrade.enable = false;
}
