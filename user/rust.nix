{stdenv, pkgs, fetchurl, zlib, ...}:

let build = {url, sha256, exes} :
  stdenv.mkDerivation rec {
    name = "rust";
    src = fetchurl {
      inherit url sha256;
    };

    dontStrip = true;

    installPhase = ''
      mv rustc $out
      mv cargo/bin/cargo $out/bin/cargo
      rm $out/manifest.in
    '';

    prePatch =''
        mv rust-std-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/ rustc/lib/rustlib/
    '';

    preFixup = let
      rpath = stdenv.lib.concatStringsSep ":" [
        "$out/lib"
        (stdenv.lib.makeLibraryPath [ zlib ])
        ''${stdenv.cc.cc}/lib64''
      ];
    in
      ''
      for executable in ${stdenv.lib.concatMapStringsSep " " (s: "$out/bin/" + s) exes}; do
        patchelf --interpreter "${stdenv.glibc}/lib/${stdenv.cc.dynamicLinker}" \
          --set-rpath "${rpath}" \
          "$executable"
      done
      for library in $out/lib/*.so $cargo/lib/*.so; do
        patchelf --set-rpath "${rpath}" "$library"
      done
      '';
  };

  version = "nightly";
in
{
  rust = build {
    url = "https://static.rust-lang.org/dist/rust-${version}-x86_64-unknown-linux-gnu.tar.gz"; # .sha256
    # sha256 = "d36634bd8df3d7565487b70af03dfda1c43c635cd6f2993f47cd61fda00d890a";
    sha256 = "cb3a2d0c21c51a014402ba08239f15791c8b241ad3d7a75323729998eeb72115";
    exes = ["rustc" "rustdoc" "cargo"];
  };
}
