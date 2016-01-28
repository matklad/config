{ config, pkgs, ... }:


with pkgs; {
    idea = (callPackage ./idea-eap.nix { });
} // (callPackage ./rust.nix { })
