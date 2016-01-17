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

in
{
  rustc = build {
    name = "rustc";

    # Version 1.5 gives "error: can't find crate for `std` [E0463]"
    url = https://static.rust-lang.org/dist/rust-1.5.0-x86_64-unknown-linux-gnu.tar.gz;
    sha256 = "60b83f74d882ce2ba5bc979b5b0589dca56659f215b3259e7188fed8c50aac9d";

    # Version 1.4 works fine
    # url = https://static.rust-lang.org/dist/rust-1.4.0-x86_64-unknown-linux-gnu.tar.gz;
    # sha256 = "07f5d91zbl4a8f20ilwajai0223j7kq6lns9gjkb6anaa15l5qid";

    exes = ["rustc" "rustdoc"];
  };

  cargo = build {
    name = "cargo";
    url = https://static.rust-lang.org/cargo-dist/cargo-nightly-x86_64-unknown-linux-gnu.tar.gz;
    sha256 = "1v9f2qqa0imdgv3n8srq2l9fpynma9bzsqqk3qjnb733r95a5r6q";
    exes = [ "cargo" ];
  };
}
