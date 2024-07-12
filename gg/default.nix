{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage {
  pname = "gg";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };
}
