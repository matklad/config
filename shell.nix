with import <nixpkgs> {};
(mkShell.override { stdenv = llvmPackages_10.stdenv; }) {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2 pcre
    xorg.libX11
    ncurses
    gtk3 glib
    vulkan-loader
    llvmPackages_10.libclang
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="${vulkan-loader}/lib:${xlibs.libX11.out}/lib:${llvmPackages_10.libclang}/lib"
  '';
}
