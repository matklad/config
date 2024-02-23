{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.kde2nix.url = "github:nix-community/kde2nix/main";
  outputs = { self, nixpkgs, nixos-hardware, kde2nix }: {
    nixosConfigurations = {
      Ishmael = nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           ./hosts ./hosts/Ishmael.nix
          #  kde2nix.nixosModules.plasma6
           nixos-hardware.nixosModules.common-gpu-nvidia-disable
         ];
      };
      Moby= nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           ./hosts ./hosts/Moby.nix
          #  kde2nix.nixosModules.plasma6
         ];
      };
    };
  };
}
