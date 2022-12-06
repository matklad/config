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
    ssh.askPassword = "ksshaskpass";
  };

  environment.systemPackages = with pkgs; [
    (pkgs.buildFHSUserEnv {
      name = "fhs";
      targetPkgs = pkgs: with pkgs; [
        alsa-lib atk cairo cups curl dbus expat file fish fontconfig freetype
        fuse glib gtk3 libGL libnotify libxml2 libxslt netcat nspr nss openjdk8
        openssl.dev pango pkg-config strace udev vulkan-loader watch wget which
        xorg.libX11 xorg.libxcb xorg.libXcomposite xorg.libXcursor
        xorg.libXdamage xorg.libXext xorg.libXfixes xorg.libXi xorg.libXrandr
        xorg.libXrender xorg.libXScrnSaver xorg.libxshmfence xorg.libXtst
        xorg.xcbutilkeysyms zlib
      ];
      profile = ''export FHS=1'';
      runScript = "fish";
    })

    # GUI
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; })
    filelight
helix
    gimp
    kdialog
    kitty
    obs-studio
    peek
    qbittorrent
    spectacle
    vscode

    # Comms
    tdesktop
    zoom-us
    slack

    # Viewers
    audacious
    mpv
    qpdfview
    qview
    smplayer

    # Langs
    (python3.withPackages (py: [py.requests]))
    ant
    cmake
    deno
    gdb
    gnumake
    lldb_14
    llvmPackages_14.bintools
    llvmPackages_14.clang
    llvmPackages_14.stdenv
    lua5_4
    maven
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

    # Rust stuff
    bat
    du-dust
    exa
    fd
    hyperfine
    ripgrep
    tokei
    # helix
  ];

  services = {
    xserver = {
      enable = true;
      displayManager = {
        sddm = {
          enable = true;
        };
        defaultSession = "plasma";
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
    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      media-session.config.bluez-monitor.rules = [
        {
          matches = [ { "device.name" = "~bluez_card.*"; } ];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              "bluez5.msbc-support" = true;
              "bluez5.sbc-xq-support" = true;
            };
          };
        }
        { matches = [ { "node.name" = "~bluez_input.*"; } { "node.name" = "~bluez_output.*"; } ]; }
      ];
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
    kanata = {
      enable = true;
      keyboards.laptop = {
        devices = ["/dev/input/by-path/platform-i8042-serio-0-event-kbd"];
        config = ''
          (defsrc
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o     p    [    ]    \
            caps a    s    d    f    g    h    j    k    l     ;    '    ret
            lsft z    x    c    v    b    n    m    ,    .     /    rsft
            lctl lmet lalt           spc            ralt rmet  rctl)

          (deflayer qwerty
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o     p    [    ]    \
            caps a    s    d    f    g    h    j    k    l     ;    '    ret
            lsft @lc  @la  @lm  v    b    n    m    @rm  @ra   @rc  rsft
            lctl lmet lalt           @sp            @alt rmet  rctl)

         (deflayer motion
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    pgup up   pgdn  p    [    ]    \
            caps a    s    d    f    g    bks  lft  down rght del   '    ret
            lsft @lc  @la  @lm  v    b    ret  m    home end   /    rsft
            lctl lmet lalt           @sp            ralt rmet  rctl)

          (defalias
            sp (tap-hold 200 200 spc (layer-toggle motion))
            lm (tap-hold 200 200 c lmet)
            rm (tap-hold 200 200 , rmet)
            lc (tap-hold 200 200 z lctl)
            rc (tap-hold 200 200 / rctl)
            la (tap-hold 200 200 x lalt)
            ra (tap-hold 200 200 . lalt)
            alt A-k
          )
        '';
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
    rtkit.enable = true;
  };

  system.stateVersion = "18.09";
  system.autoUpgrade.enable = false;
}
