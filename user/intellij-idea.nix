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
    '';

    libraries = [stdenv.cc.libc glib libxml2 libav_0_8 ffmpeg libxslt mesa_noglu xorg.libXxf86vm alsaLib fontconfig 
                 freetype gnome.pango gnome.gtk cairo gdk_pixbuf atk xorg.libX11 xorg.libXext xorg.libXtst xorg.libXi 
		 xorg.libXp xorg.libXt xorg.libXrender stdenv.cc.cc];
    architecture = "amd64";

    installPhase = ''
      mkdir -p $out/{bin,$name,share/pixmaps,libexec/${name}}
      cp -a . $out/$name
      ln -s $out/$name/bin/${loName}.png $out/share/pixmaps/${execName}.png
      mv bin/fsnotifier* $out/libexec/${name}/.

      jrePath=$out/$name/jre/jre
      rpath=
      for i in $libraries; do
          rpath=$rpath''${rpath:+:}$i/lib:$i/lib64
      done

      rpath=$rpath''${rpath:+:}$jrePath/lib/${architecture}/jli
      rpath=$rpath''${rpath:+:}$jrePath/lib/${architecture}/server
      rpath=$rpath''${rpath:+:}$jrePath/lib/${architecture}/xawt
      rpath=$rpath''${rpath:+:}$jrePath/lib/${architecture}

      interpreter=$(echo ${stdenv.glibc}/lib/ld-linux*.so.2)

      patchelf --set-interpreter "$interpreter" --set-rpath "$rpath" $jrePath/bin/*
      find $out -name "*.so" -exec patchelf --set-rpath "$rpath" {} \;

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
  version = "2016.1.3";
  description = "Integrated Development Environment (IDE) by Jetbrains, community edition";
  license = stdenv.lib.licenses.asl20;
  src = fetchurl {
    url = "https://download.jetbrains.com/idea/ideaIC-${version}.tar.gz"; # .sha256
    sha256 = "d1cd3f9fd650c00ba85181da6d66b4b80b8e48ce5f4f15b5f4dc67453e96a179";
  };
}
