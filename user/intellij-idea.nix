{ stdenv, pkgs, ... }:

with pkgs;

let

  mkIdeaProduct = with stdenv.lib;
  { name, product, version, src, meta }:

  let loName = toLower product;
      hiName = toUpper product;
      execName = concatStringsSep "-" (init (splitString "-" name));
  in

  with stdenv; lib.makeOverridable mkDerivation rec {
    inherit name src meta;
    desktopItem = makeDesktopItem {
      name = execName;
      exec = execName;
      comment = lib.replaceChars ["\n"] [" "] meta.longDescription;
      desktopName = product;
      genericName = meta.description;
      categories = "Application;Development;";
      icon = execName;
    };

    buildInputs = [ makeWrapper patchelf p7zip unzip ];

    patchPhase = ''
        get_file_size() {
          local fname="$1"
          echo $(ls -l $fname | cut -d ' ' -f5)
        }

        munge_size_hack() {
          local fname="$1"
          local size="$2"
          strip $fname
          truncate --size=$size $fname
        }

        interpreter=$(echo ${stdenv.glibc}/lib/ld-linux*.so.2)
        target_size=$(get_file_size bin/fsnotifier64)
        patchelf --set-interpreter "$interpreter" bin/fsnotifier64
        munge_size_hack bin/fsnotifier64 $target_size
        patchelf --set-interpreter "$interpreter" jre/jre/bin/*
    '';

    installPhase = ''
      mkdir -p $out/{bin,$name,share/pixmaps,libexec/${name}}
      cp -a . $out/$name
      ln -s $out/$name/bin/${loName}.png $out/share/pixmaps/${execName}.png
      mv bin/fsnotifier* $out/libexec/${name}/.

      item=${desktopItem}

      makeWrapper "$out/$name/bin/${loName}.sh" "$out/bin/${execName}" 

      ln -s "$item/share/applications" $out/share
    '';

  };

  buildIdea = { name, version, src, license, description }:
    (mkIdeaProduct rec {
      inherit name version src;
      product = "IDEA";
      meta = with stdenv.lib; {
        homepage = "https://www.jetbrains.com/idea/";
        inherit description license;
        longDescription = ''
          IDE for Java SE, Groovy & Scala development Powerful
          environment for building Google Android apps Integration
          with JUnit, TestNG, popular SCMs, Ant & Maven.
        '';
        maintainers = with maintainers; [ edwtjo ];
        platforms = platforms.linux;
      };
    });

in

buildIdea rec {
  name = "intellij-idea";
  version = "2016.1.1";
  description = "Integrated Development Environment (IDE) by Jetbrains, community edition";
  license = stdenv.lib.licenses.asl20;
  src = fetchurl {
    url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz"; # .sha256
    sha256 = "46adaa6e19a605d2b439b0b58d18723b7947c1c56e4bbc142396a2b911de13e4";
  };
}
