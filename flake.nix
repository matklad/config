{
  inputs = {
    # nixpkgs.url = "github:matklad/nixpkgs/matklad/main";
    # nixpkgs.url = "github:NixOS/nixpkgs?rev=60cb88cc491e819c16fc579fd697d33defd2a8e3";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs = inputs@{ self, nixpkgs, nixos-hardware, ... }:
    let nixosSystem = nixpkgs.lib.nixosSystem;
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
