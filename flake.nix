{
  # inputs.nixpkgs.url = "github:matklad/nixpkgs/matklad/main";
  # inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=60cb88cc491e819c16fc579fd697d33defd2a8e3";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  outputs = inputs@{ self, nixos-hardware, ... }:
    let patches = [
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/292148.diff";
        sha256 = "sha256-ZCDQ7SpGhH8JvAwWzdcyrc68RFEWHxxAj0M2+AvEzIg=";
      }
    ];
    originPkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    nixpkgs = originPkgs.applyPatches {
      name = "nixpkgs-patched";
      src = inputs.nixpkgs;
      patches = map originPkgs.fetchpatch patches;
    };
  in
  {
    nixosConfigurations = {
      Ishmael = nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           ./hosts ./hosts/Ishmael.nix
           nixos-hardware.nixosModules.common-gpu-nvidia-disable
         ];
      };
      Moby= nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           ./hosts ./hosts/Moby.nix
         ];
      };
    };
  };
}
