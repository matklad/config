{stdenv, pkgs, fetchurl, zlib, ...}:

let build = {name, url, sha256, exes} :
  stdenv.mkDerivation rec {
    inherit name;
    src = fetchurl {
      inherit url sha256;
    };

    dontStrip = true;

    installPhase = ''
      mv ${name} $out
      rm $out/manifest.in
    '';


    prePatch = if name == "rustc" then
        ''
        mv rust-std-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/ rustc/lib/rustlib/
        ''
    else
        null;

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

  cargoNight = "2016-01-30";

in
{
  rustc = build {
    name = "rustc";

    url = https://static.rust-lang.org/dist/rust-1.7.0-x86_64-unknown-linux-gnu.tar.gz; # .sha256
    sha256 = "d36634bd8df3d7565487b70af03dfda1c43c635cd6f2993f47cd61fda00d890a";

    exes = ["rustc" "rustdoc"];
  };

  cargo = build {
    name = "cargo";
    url = "https://static.rust-lang.org/cargo-dist/${cargoNight}/cargo-nightly-x86_64-unknown-linux-gnu.tar.gz";
    sha256 = "0yy9g3fb47l9wwsxgfxmw6hcgyffjic1v44ffznwm6067d6y093z";
    exes = [ "cargo" ];
  };
}
