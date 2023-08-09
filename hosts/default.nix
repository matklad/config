{ config, pkgs, lib, ... }: {

  time.timeZone = "Europe/Lisbon";

  nix = {
    extraOptions = "experimental-features = nix-command flakes ca-derivations";
    nixPath = ["nixpkgs=${pkgs.path}"];
    gc = {
      automatic = true;
      options = "--max-freed 1G --delete-older-than 7d";
    };
  };

  nixpkgs.config = {
    # contentAddressedByDefault = true;
    allowUnfree = true;
  };

  environment.systemPackages = (import ./packages.nix) pkgs;

  programs = {
    java.enable = true;
    dconf.enable = true;

    ssh = {
      startAgent = true;
      askPassword = "${pkgs.ksshaskpass.out}/bin/ksshaskpass";
    };

    git = {
      enable = true;
      config = {
        core = {
          autocrlf = "input";
          excludesfile = pkgs.writeText "gitignore_global" ''
            shell.nix
            .nix-shell-inputs
            .envrc
            .direnv/
            make.ts
          '';
        };
        user = { name = "Alex Kladov"; email = "aleksey.kladov@gmail.com"; };
        push.default = "current";
        github.user = "matklad";
        fetch.prune = true;
        diff = {
          colormoved = "default";
          colormovedws = "allow-indentation-change";
        };
      };
    };

    fish = {
      enable = true;
      shellAbbrs = {
        "r+" = "gh pr comment --body 'bors r+'";
        c = "code";
        ca = "cargo";
        cat = "bat";
        ct = "cargo t -- -q";
        ctrlc = "wl-copy";
        ctrlv = "wl-paste";
        g = "git";
        gb = "git branch";
        gbd = "git branch -D";
        gc = "git commit";
        gch = "git rev-parse HEAD | wl-copy";
        gl = "git log";
        gof = "git spinoff";
        gp = "git push";
        gpf = "git push --force-with-lease";
        gs = "git status --short --branch";
        gup = "git pull --rebase";
        ls = "exa -l";
        perfr = "perf record -F 9999 --call-graph dwarf";
        sw = "git switch";
        swc = "git switch -c";
        swd = "git switch -d";
      };
    };
  };

  services = {
    emacs.enable = true;

    logind = {
      extraConfig = ''
        IdleAction=poweroff
        IdleActionSec=60min
      '';
    };

    xserver = {
      enable = true;
      displayManager = {
        sddm.enable = true;
        autoLogin = { enable = false; user = "matklad"; };
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
      enable = false;
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
      # enable = true;
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
    # kernelPackages = pkgs.linuxPackages_6_2;
    kernel.sysctl."kernel.perf_event_paranoid" = 1;
  };

  hardware = {
    keyboard.zsa.enable = true;
    ledger.enable = true;
  };

  virtualisation = {
    # virtualbox.host = { enable = true; enableExtensionPack = true; };
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 10100 ];
      allowedUDPPorts = [ 10100 ];
    };
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
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
    enableDefaultPackages = true;
    packages = with pkgs; [
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
    localBinInPath = true;
    sessionVariables = {
      # NIXOS_OZONE_WL = "1";
      DENO_NO_UPDATE_CHECK = "1";
      PATH = "$HOME/.cargo/bin";
      VISUAL = "hx";
      EDITOR = "hx";
      RUST_BACKTRACE = "short";
      SSH_ASKPASS_REQUIRE="prefer";
    };

    etc."xdg/user-dirs.defaults".text = ''
      DOWNLOAD=downloads
      TEMPLATES=tmp
      PUBLICSHARE=/var/empty
      DOCUMENTS=tmp
      MUSIC=music
      PICTURES=tmp
      VIDEOS=video
      DESKTOP=/var/empty
    '';

    etc."bat/config".text = ''
      --plain
      --theme GitHub
    '';
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
}
