{
  # inputs.nixpkgs.url = "github:matklad/nixpkgs/matklad/main";
  # inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=60cb88cc491e819c16fc579fd697d33defd2a8e3";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  outputs = inputs@{ self, nixos-hardware, ... }:
    let patches = [
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/292148.diff";
        sha256 = "sha256-gaH4UxKi2s7auoaTmbBwo0t4HuT7MwBuNvC/z2vvugE=";
      }
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/324549.diff";
        sha256 = "sha256-00H4UxKi2s7auoaTmbBwo0t4HuT7MwBuNvC/z2vvugE=";
      }
    ];
    originPkgs = inputs.nixpkgs.legacyPackages."x86_64-linux";
    nixpkgs = originPkgs.applyPatches {
      name = "nixpkgs-patched";
      src = inputs.nixpkgs;
      patches = map originPkgs.fetchpatch patches;
    };
    nixosSystem = import (nixpkgs + "/nixos/lib/eval-config.nix");
    #nixosSystem = inputs.nixpkgs.lib.nixosSystem;
    modulesCommon = [
      ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
      ./hosts
    ];
  in
  {
    nixosConfigurations = {
      Ishmael = nixosSystem {
         system = "x86_64-linux";
         modules = modulesCommon ++ [
           ./hosts/Ishmael.nix
           nixos-hardware.nixosModules.common-gpu-nvidia-disable
         ];
      };
      Moby= nixosSystem {
         system = "x86_64-linux";
         modules = modulesCommon ++ [
           ./hosts/Moby.nix
         ];
      };
    };
  };
}
