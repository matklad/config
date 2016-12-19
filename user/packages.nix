{ config, pkgs, ... }:

let
  custom = (pkgs.callPackage ./custom.nix {});
in {
  environment.systemPackages =  with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.ru
    atool
    bundler
    chromium
    clang
    clementine
    cloc
    cmake
    curl
    custom.idea
    emacs
    file
    flameGraph
    gimp
    git
    glibc
    gnumake
    htop
    imagemagick
    kde4.gwenview
    linuxPackages.perf
    neovim
    networkmanager
    networkmanagerapplet
    nox
    ntfs3g
    oraclejdk8
    python2
    python3
    qbittorrent
    qpdfview
    ruby
    terminus_font
    tree
    unclutter
    unrar
    unzip
    valgrind
    vivaldi
    vlc
    wget
    which
    wmctrl
    xbindkeys
    xfce.xfce4notifyd
    xfce.xfwm4
    xorg.libX11
    xsel
    yakuake
    kde5.konsole
    zip
    zlib
  ];
}
