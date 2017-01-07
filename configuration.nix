{ config, pkgs, ... }:

let
  user = "matklad";
  custom = (pkgs.callPackage ./user/custom.nix {});
in {
  imports = [ /etc/nixos/hardware-configuration.nix
             ./user/packages.nix
             ];

  nixpkgs.config = {
    allowUnfree = true;
  };

  nix.extraOptions = ''
    build-cores = 4
  '';

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    extraEntries = ''
      menuentry "Windows 7" {
        chainloader (hd0,1)+1
      }
   '';
    };

  boot.kernelParams = [ "acpi_backlight=video" ];

  system.autoUpgrade.enable = true;

  networking = {
    hostName = "nixos";
    networkmanager.enable = true;
    enableIPv6 = false;
    firewall.enable = false;
  };
  virtualisation.virtualbox.host.enable = true;
  time.timeZone = "Europe/Moscow";

  programs.zsh.enable = true;

  hardware = {
    opengl.enable = true;
    opengl.driSupport = true;
    pulseaudio.enable = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = [ "intel" ];
    displayManager.lightdm.enable = true;
    desktopManager.xfce.enable = true;
    synaptics = {
      enable = true;
      horizTwoFingerScroll = false;
      twoFingerScroll = true;
      accelFactor = "0.002";
      minSpeed = "0.8";
      maxSpeed = "3.0";
      palmDetect = true;
      additionalOptions = ''
          Option "SoftButtonAreas" "50% 0 82% 0 0 0 0 0"
      '';
    };
  };

  fonts = {
     enableFontDir = true;
     enableGhostscriptFonts = true;
     fonts = with pkgs; [
       corefonts  # Micrsoft free fonts
       inconsolata  # monospaced
       ubuntu_font_family  # Ubuntu fonts
       unifont # some international languages
       hack-font
     ];
   };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.${user} = {
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
    uid = 1000;
  };
  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
  system.activationScripts = {
      dotfiles =
      ''
        cd /home/${user}
        ln -fs ${./dots/user-dirs.dirs} .config/user-dirs.dirs
        rm -rf .config/nvim
        ln -fs ${./dots/config/nvim} .config/nvim
      '';

      kdeAutostart =
      ''
        cd /home/${user}
        ln -fs ${pkgs.kde4.yakuake}/share/applications/kde4/yakuake.desktop ./.config/autostart/yakuake.desktop
      '';
  };

}
