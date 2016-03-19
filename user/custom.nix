{ config, pkgs, ... }:


with pkgs; {
    idea = (callPackage ./intellij-idea.nix { });
} // (callPackage ./rust.nix { })
