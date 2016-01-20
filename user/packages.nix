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
    custom.yakuake
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
    kde5.kgpg
    kde5.krunner
    kde5.okular
    kde5.plasma-nm
    kde5.plasma-pa
    mplayer
    networkmanager
    nox
    openjdk8
    python3
    qbittorrent
    smplayer
    unclutter
    unrar
    unrar
    unzip
    vim
    wget
    wmctrl
    xbindkeys
    xorg.libX11
    zip
  ];
}
