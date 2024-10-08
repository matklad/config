{ inputs, config, pkgs, lib, ... }: {

  time.timeZone = "Europe/Lisbon";

  nix = {
    package = pkgs.lix;
    extraOptions = "experimental-features = nix-command flakes ca-derivations";
    nixPath = ["nixpkgs=${pkgs.path}"];
    gc = {
      automatic = true;
      options = "--max-freed 1G --delete-older-than 7d";
    };
    channel.enable = false;
  };

  nixpkgs.config = {
    # contentAddressedByDefault = true;
    allowUnfree = true;
  };

  environment.systemPackages = (import ./packages.nix) { inherit inputs pkgs; };

  programs = {
    nix-ld.enable = true;
    java.enable = true;
    dconf.enable = true;
    virt-manager.enable = true;
    command-not-found.enable = false;
    # darling.enable = true;

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
        push = {
          default = "current";
          autoSetupRemote = "true";
        };
        github.user = "matklad";
        fetch.prune = true;
        diff = {
          colormoved = "default";
          colormovedws = "allow-indentation-change";
        };
        branch.sort = "committerdate";
        merge.conflictstyle = "diff3";
        advice.detachedHead = "false";
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
        gga = "gg amend";
        ggc = "gg commit";
        g = "git";
        gb = "git branch";
        gbd = "git branch -D";
        gc = "git commit";
        gch = "git rev-parse HEAD | wl-copy";
        gl = "git log";
        glm = "git log --format=%B -n 1";
        gof = "git spinoff";
        gp = "git push";
        gpf = "git push --force-with-lease";
        gs = "git status --short --branch";
        gup = "git pull --rebase";
        ls = "eza -l";
        perfr = "perf record -F 9999 --call-graph dwarf";
        sw = "gg switch";
        swc = "git switch -c";
        swd = "git switch -d";
        zz = "./zig/zig";
      };
      shellAliases = {
        q = "llm -s \"Answer in as few words as possible. Use a brief style with short replies.\" -m claude-3.5-sonnet";
      };
    };
  };

  services = {
    emacs.enable = true;
    mullvad-vpn.enable = true;
    mullvad-vpn.package = pkgs.mullvad-vpn;

    logind = {
      extraConfig = ''
        IdleAction=poweroff
        IdleActionSec=60min
      '';
    };

    displayManager = {
      sddm = {
        enable = true;
        wayland.enable = true;
      };
      autoLogin = { enable = false; user = "matklad"; };
      defaultSession = "plasma";
    };
    desktopManager.plasma6.enable = true;
    # desktopManager.cosmic.enable = true;
    xserver = {
      enable = true;

      xkb = {
        layout = "us,ru";
        variant = "workman,";
        options = "grp:win_space_toggle";
      };
    };

    libinput = {
      enable = true;
      touchpad = {
        disableWhileTyping = true;
        horizontalScrolling = false;
        naturalScrolling = true;
        additionalOptions = ''
          Option "PalmDetection" "on"
        '';
      };
      mouse = {
        disableWhileTyping = true;
      };
    };

    pipewire.enable = true;

    unclutter-xfixes.enable = true;
    printing.enable = true;
    earlyoom.enable = true;
    pcscd.enable = true;
    openssh = {
      enable = true;
      settings.PermitRootLogin = "no";
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
    kernel.sysctl."kernel.perf_event_paranoid" = 1;
    extraModprobeConfig = ''
      options snd_hda_intel power_save=0
    '';
    binfmt.emulatedSystems = ["aarch64-linux" "x86_64-windows"];
  };

  system.switch = {
    enable = false;
    enableNg = true;
  };

  hardware = {
    keyboard.zsa.enable = true;
    ledger.enable = true;
  };

  virtualisation = {
    libvirtd = {
      enable = true;
      qemu.swtpm.enable = true;
    };
    podman = {
      enable = true;
      dockerCompat = true;
    };
  };

  networking = {
    firewall = {
      enable = true;
      allowedTCPPorts = [ 22 8080 ];
    };
    networkmanager = {
      enable = true;
      wifi.backend = "iwd";
    };
    extraHosts =  if builtins.pathExists ./hosts.txt
      then builtins.readFile ./hosts.txt
      else "";
  };

  systemd.extraConfig = "DefaultTimeoutStopSec=10s";

  i18n = {
    inputMethod.enabled = null;
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME = "en_IE.UTF-8";
    };
  };

  fonts = {
    fontDir.enable = true;
    enableDefaultPackages = false;
    packages = with pkgs; [
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
      extraGroups = [ "wheel" "networkmanager" "docker" "audio" "plugdev" "podman" "libvirtd" ];
      uid = 1000;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHQSOWMtVbK4euVg8Dx/uWkLIpOZsDZzv4fg8zxAVEEW aleksey.kladov@gmail.com"
      ];
    };
  };

  environment = {
    enableDebugInfo = true;
    localBinInPath = true;
    sessionVariables = {
      NIXOS_OZONE_WL = "1";
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
      extraRules = [{ users = [ "matklad" ]; keepEnv = true; noPass = true; }];
    };
    pam.loginLimits = [
      { domain = "*"; type = "soft"; item = "memlock"; value = "524288"; }
      { domain = "*"; type = "hard"; item = "memlock"; value = "524288"; }
    ];
    rtkit.enable = true;
  };
}
