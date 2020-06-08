with import <nixpkgs> {};
pkgs.mkShell {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2
    ncurses
    gtk3 glib
  ];
}
