{
  inputs.nixpkgs.url = "github:matklad/nixpkgs/matklad/poly-vivaldi";
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
