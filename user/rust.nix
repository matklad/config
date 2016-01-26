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

  cargoNight = "2016-01-25";

in
{
  rustc = build {
    name = "rustc";

    url = https://static.rust-lang.org/dist/rust-1.6.0-x86_64-unknown-linux-gnu.tar.gz;
    sha256 = "1xhk50h5kdrlrjis6mfmyg8mv3n53q3yzr7a9vb26i1b8c1cqc46";

    exes = ["rustc" "rustdoc"];
  };

  cargo = build {
    name = "cargo";
    url = "https://static.rust-lang.org/cargo-dist/${cargoNight}/cargo-nightly-x86_64-unknown-linux-gnu.tar.gz";
    sha256 = "0arxb1gi7l5b1ziki2bjd5k0y1fl3y0vymgbp5i7idf20gbfjisp";
    exes = [ "cargo" ];
  };
}
