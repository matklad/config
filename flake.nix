{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.emacs-overlay.url = "github:nix-community/emacs-overlay";
  outputs = { self, nixpkgs, emacs-overlay }: {
    nixosConfigurations = {
      Ishmael = nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           { nixpkgs.overlays = [ (import emacs-overlay) ]; }
           ./hosts ./hosts/Ishmael.nix
         ];
      };
      Moby= nixpkgs.lib.nixosSystem {
         system = "x86_64-linux";
         modules = [
           ({config, pkgs, ...}: { nix.registry.nixpkgs.flake = nixpkgs; })
           { nixpkgs.overlays = [ (import emacs-overlay) ]; }
           ./hosts ./hosts/Moby.nix
         ];
      };
    };
  };
}
