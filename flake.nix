{
  inputs = {
    # nixpkgs.url = "github:matklad/nixpkgs/matklad/main";
    # nixpkgs.url = "github:NixOS/nixpkgs?rev=60cb88cc491e819c16fc579fd697d33defd2a8e3";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixos-hardware, ... }:
    let patches = [
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/292148.diff";
        sha256 = "sha256-gaH4UxKi2s7auoaTmbBwo0t4HuT7MwBuNvC/z2vvugE=";
      }
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/338859.diff";
        sha256 = "sha256-/zDD6Ic7+FVF4A7TzNj0b3oaA9lQg0c2XUhlK7qR50A=";
      }
      {
        url = "https://patch-diff.githubusercontent.com/raw/NixOS/nixpkgs/pull/338326.diff";
        sha256 = "sha256-pRoCdBNiXlQKMFhysUEXnsiGY498qW//06xVOjfpUyI=";
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
         specialArgs = { inherit inputs; };
         modules = modulesCommon ++ [
           ./hosts/Ishmael.nix
           nixos-hardware.nixosModules.common-gpu-nvidia-disable
         ];
      };
      Moby= nixosSystem {
         system = "x86_64-linux";
         specialArgs = { inherit inputs; };
         modules = modulesCommon ++ [
           ./hosts/Moby.nix
         ];
      };
    };
  };
}
