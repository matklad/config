{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage {
  pname = "gg";
  version = "0.0.0";
  src = ./.;
  cargoLock.lockFile = ./Cargo.lock;
}
