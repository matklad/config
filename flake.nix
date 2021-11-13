{
  outputs = { self, nixpkgs }: {
    nixosConfigurations.moby = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ ./configuration.nix ];
    };
  };
}
