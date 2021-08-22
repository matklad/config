with import <nixpkgs> {};
(mkShell.override { stdenv = llvmPackages_11.stdenv; }) {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2 pcre freetype fontconfig
    xorg.libX11 xorg.libXcursor xorg.libXrandr xorg.libXrender xorg.libXi xorg.libXext xorg.libXtst xorg.libxcb
    libxkbcommon
    ncurses
    gtk3 glib
    libGL vulkan-loader
    glib
    nettle
    protobuf
  ];
  shellHook = ''
    export LLVM_SYS_110_PREFIX = llvmPackages_11.bintools;
    export LIBCLANG_PATH="${llvmPackages_11.libclang.lib}/lib"
    export LD_LIBRARY_PATH="\
${vulkan-loader}/lib:\
${xlibs.libX11.out}/lib:\
${xlibs.libXcursor.out}/lib:\
${xlibs.libXrandr.out}/lib:\
${xlibs.libXi.out}/lib:\
${xlibs.libXext.out}/lib:\
${xlibs.libXtst.out}/lib:\
${xlibs.libXrender.out}/lib:\
${xlibs.libxcb.out}/lib:\
${libxkbcommon.out}/lib:\
${llvmPackages_10.libclang}/lib:\
${libGL}/lib:\
${freetype}/lib:\
${fontconfig.lib}/lib:\
${libgit2}/lib:\
${openssl.out}/lib:\
${zlib}/lib:\
${glib.out}/lib:\
${nettle.out}/lib:\
"'';
}
