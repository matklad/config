with import <nixpkgs> {};
(mkShell.override { stdenv = llvmPackages_10.stdenv; }) {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2 pcre freetype fontconfig
    xorg.libX11 xorg.libXcursor xorg.libXrandr xorg.libXi
    ncurses
    gtk3 glib
    libGL vulkan-loader
    llvmPackages_10.libclang
  ];
  shellHook = ''
    export LD_LIBRARY_PATH="\
${vulkan-loader}/lib:\
${xlibs.libX11.out}/lib:\
${xlibs.libXcursor.out}/lib:\
${xlibs.libXrandr.out}/lib:\
${xlibs.libXi.out}/lib:\
${llvmPackages_10.libclang}/lib:\
${libGL}/lib:\
${freetype}/lib:\
${fontconfig.lib}/lib:\
"'';
}
