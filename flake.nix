{
  # inputs.nixpkgs.url = "github:matklad/nixpkgs/matklad/main";
  # inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=60cb88cc491e819c16fc579fd697d33defd2a8e3";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  outputs = { self, nixpkgs, nixos-hardware }: {
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
