with import <nixpkgs> {};
pkgs.mkShell {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2 pcre
    xorg.libX11
    ncurses
    gtk3 glib
    vulkan-loader
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${vulkan-loader}/lib:${xlibs.libX11.out}/lib"
  '';
}
