{ config, pkgs, ... }:

let
  custom = (pkgs.callPackage ./custom.nix {});
in {
  environment.systemPackages =  with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.ru
    atool
    chromium
    clang
    clementine
    cloc
    cmake
    curl
    custom.cargo
    custom.idea
    custom.rustc
    emacs
    fbreader
    file
    gcc
    ghc
    gimp
    git
    gnumake
    htop
    imagemagick
    kde4.krusader
    kde4.ksnapshot
    kde4.konsole
    kde4.yakuake
    kde5.kgpg
    kde5.krunner
    kde5.okular
    kde5.plasma-nm
    kde5.plasma-pa
    mplayer
    neovim
    networkmanager
    nox
    # openjdk8
    python3
    qbittorrent
    smplayer
    stack
    # texLiveFull
    unclutter
    unrar
    unrar
    unzip
    wget
    wmctrl
    xbindkeys
    xorg.libX11
    zip
  ];
}
