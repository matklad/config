{ stdenv, pkgs, ... }:

with pkgs;

let

  mkIdeaProduct = with stdenv.lib;
  { name, product, version, build, src, meta }:

  let loName = toLower product;
      hiName = toUpper product;
      execName = concatStringsSep "-" (init (splitString "-" name));
  in

  with stdenv; lib.makeOverridable mkDerivation rec {
    inherit name build src meta;
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
        if [ "${stdenv.system}" == "x86_64-linux" ]; then
          target_size=$(get_file_size bin/fsnotifier64)
          patchelf --set-interpreter "$interpreter" bin/fsnotifier64
          munge_size_hack bin/fsnotifier64 $target_size
        else
          target_size=$(get_file_size bin/fsnotifier)
          patchelf --set-interpreter "$interpreter" bin/fsnotifier
          munge_size_hack bin/fsnotifier $target_size
        fi
    '';

    installPhase = ''
      mkdir -p $out/{bin,$name,share/pixmaps,libexec/${name}}
      cp -a . $out/$name
      ln -s $out/$name/bin/${loName}.png $out/share/pixmaps/${execName}.png
      mv bin/fsnotifier* $out/libexec/${name}/.

      jdk=${jdk.home}
      item=${desktopItem}

      makeWrapper "$out/$name/bin/${loName}.sh" "$out/bin/${execName}" \
        --prefix PATH : "$out/libexec/${name}:${jdk}/bin:${coreutils}/bin:${gnugrep}/bin:${which}/bin:${git}/bin" \
        --set JDK_HOME "$jdk" \
        --set ${hiName}_JDK "$jdk" \
        --set JAVA_HOME "$jdk"

      ln -s "$item/share/applications" $out/share
    '';

  };

  buildIdea = { name, version, build, src, license, description }:
    (mkIdeaProduct rec {
      inherit name version build src;
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

# buildIdea rec {
#   name = "idea-eap";
#   version = "15.0.4";
#   build = "143.2287";
#   description = "Integrated Development Environment (IDE) by Jetbrains, community edition";
#   license = stdenv.lib.licenses.asl20;
#   src = fetchurl {
#     url = "https://download.jetbrains.com/idea/ideaIC-${build}.tar.gz";
#     sha256 = "65fd9f9b6bc633c29bbea4c245a5c0b6855e484460a5a84946714533b9bc0be4";
#   };
# }

buildIdea rec {
  name = "idea";
  version = "2016.1";
  # build = "143.2287";
  description = "Integrated Development Environment (IDE) by Jetbrains, community edition";
  license = stdenv.lib.licenses.asl20;
  src = fetchurl {
    url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz"; # .sha256
    sha256 = "d8d889c42dfde1f41030e9d3f5850d364109df73a6a35081edc72d7d95f22fbf";
  };
}
