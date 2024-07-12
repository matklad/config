{ lib, fetchFromGitHub, rustPlatform }:

rustPlatform.buildRustPackage rec {
  pname = "gg";
  version = "0.0.0";

  src = ./.;

  cargoLock = {
    lockFile = ./Cargo.lock;
  };
}
