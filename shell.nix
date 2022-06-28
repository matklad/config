with import <nixpkgs> {};
(mkShell.override { stdenv = llvmPackages_13.clang.stdenv; }) {
  buildInputs = [
    pkgconfig
    python cmake
    openssl zlib libgit2 libxml2 pcre freetype fontconfig
    xorg.libX11 xorg.libXcursor xorg.libXrandr xorg.libXrender xorg.libXi xorg.libXext xorg.libXtst xorg.libxcb
    libxkbcommon libevdev
    ncurses
    gtk3 glib
    libGL vulkan-loader
    glib
    nettle
    protobuf
  ];
  shellHook = ''
    export LLVM_SYS_130_PREFIX = llvmPackages_13.bintools;
    export LIBCLANG_PATH="${llvmPackages_13.libclang.lib}/lib"
    export LD_LIBRARY_PATH="\
${vulkan-loader}/lib:\
${xorg.libX11.out}/lib:\
${xorg.libXcursor.out}/lib:\
${xorg.libXrandr.out}/lib:\
${xorg.libXi.out}/lib:\
${xorg.libXext.out}/lib:\
${xorg.libXtst.out}/lib:\
${xorg.libXrender.out}/lib:\
${xorg.libxcb.out}/lib:\
${libxkbcommon.out}/lib:\
${libevdev.out}/lib:\
${llvmPackages_10.libclang}/lib:\
${libGL}/lib:\
${freetype}/lib:\
${fontconfig.lib}/lib:\
${libgit2}/lib:\
${openssl.out}/lib:\
${zlib}/lib:\
${glib.out}/lib:\
${nettle.out}/lib:\
${stdenv.cc.cc.lib}/lib:\
"'';
}
