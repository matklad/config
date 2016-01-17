{ config, pkgs, ... }:


with pkgs; {
    yakuake = (callPackage ./yakuake.nix {
        kdelibs = kde4.kdelibs;
        konsole = kde4.konsole;
    });
    idea = (callPackage ./idea-eap.nix { });
} // (callPackage ./rust.nix { })
