{ rustPlatform }:

rustPlatform.buildRustPackage {
  pname = "xtool";
  version = "0.0.0";
  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;

  postInstall = ''
    ln $out/bin/xtool $out/bin/nixup
    ln $out/bin/xtool $out/bin/nixgc
    ln $out/bin/xtool $out/bin/autostart
    ln $out/bin/xtool $out/bin/n
    rm $out/bin/xtool
  '';
}
