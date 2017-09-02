self: super:

{
  jbsdk = self.callPackage ~/config/nix/jbsdk.nix {};

  idea-community = let 
    version = "2017.2.3";
    sha256 = "727ba10b55c9bba9d21e1703bd86af832d05f25242efddf9dd3b99841d23d71b";
  in
    (super.idea.idea-community.override { jdk = self.jbsdk; })

    .overrideAttrs (attrs: rec {
      inherit version;
      name = "idea-community-${version}";
      src = self.fetchurl {
        inherit sha256;
        url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz";
      };
    });
}