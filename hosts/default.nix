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
    (pkgs.buildFHSUserEnv (pkgs.appimageTools.defaultFhsEnvArgs // {
      name = "fhs";
      profile = ''export FHS=1'';
      runScript = "fish";
    }))

    # GUI
    (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; })
    filelight
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
    elan
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
    xserver = {
      enable = true;
      displayManager = {
        sddm = {
          enable = true;
        };
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
#     media-session.config.bluez-monitor.rules = [
#       {
#         matches = [ { "device.name" = "~bluez_card.*"; } ];
#         actions = {
#           "update-props" = {
#             "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
#             "bluez5.msbc-support" = true;
#             "bluez5.sbc-xq-support" = false;
#           };
#         };
#       }
#       { matches = [ { "node.name" = "~bluez_input.*"; } { "node.name" = "~bluez_output.*"; } ]; }
#     ];
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
        config = ''
          (defsrc
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o     p    [    ]    \
            caps a    s    d    f    g    h    j    k    l     ;    '    ret
       lsft 102d z    x    c    v    b    n    m    ,    .     /    rsft
            lctl lmet lalt           spc            ralt rmet  rctl)

          (deflayer qwerty
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    u    i    o     p    [    ]    \
            esc  a    s    d    f    g    h    j    k    l     ;    '    ret
       lsft @w0  @lc  @la  @lm  v    b    n    m    @rm  @ra   @rc  rsft
            lctl lmet lalt           @sp            ralt rmet  rctl)

         (deflayer motion
            grv  1    2    3    4    5    6    7    8    9     0    -    =    bspc
            tab  q    w    e    r    t    y    pgup up   pgdn  p    [    ]    \
            esc  a    s    d    f    g    bspc lft  down rght del   '    ret
       lsft @w0  @lc  @la  @lm  v    b    ret  m    home end   /    rsft
            lctl lmet lalt           @sp            ralt rmet  rctl)

          (defalias
            sp (tap-hold 200 200 spc (layer-toggle motion))
            lm (tap-hold 200 200 c lmet)
            rm (tap-hold 200 200 , rmet)
            lc (tap-hold 200 200 z lctl)
            rc (tap-hold 200 200 / rctl)
            la (tap-hold 200 200 x lalt)
            ra (tap-hold 200 200 . lalt)
            w0 M-0
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
    homeBinInPath = true;
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
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

  system.stateVersion = "18.09";
  system.autoUpgrade.enable = false;
}
