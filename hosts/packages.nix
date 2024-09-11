{ inputs,  pkgs}: with pkgs; [
  # Local
  (callPackage ../gg {})
  (callPackage ../xtool {})

  # GUI
  (pkgs.vivaldi.overrideAttrs (oldAttrs: {
    buildPhase = builtins.replaceStrings
      ["for f in libGLESv2.so libqt5_shim.so ; do"]
      ["for f in libGLESv2.so libqt6_shim.so ; do"]
      oldAttrs.buildPhase
    ;
  })).override {
    qt5 = pkgs.qt6;
    commandLineArgs = [ "--ozone-platform=wayland" ];
    # The following two are just my preference, feel free to leave them out
    proprietaryCodecs = true;
    enableWidevine = true;
  }

  anki
  filelight
  gimp
  kdialog
  wezterm
  obs-studio
  qbittorrent
  spotify
  steam-run
  vscode
  zed-editor

  # Comms
  slack
  tdesktop
  # zoom-us

  # Viewers
  audacious
  # qpdfview
  qview
  haruna

  # Langs
  (python3.withPackages (py: [
    py.requests
    py.llm
    (callPackage ./llm-claude-3.nix {})
  ]))
  ant
  cmake
  deno
  dotnet-sdk_8
  gdb
  gnumake
  go
  llvmPackages_18.bintools
  llvmPackages_18.clang
  llvmPackages_18.lldb
  llvmPackages_18.llvm
  llvmPackages_18.stdenv
  lua5_4
  kcachegrind
  hotspot
  maven
  nil
  ninja
  nodejs
  protobuf
  rustup
  valgrind
  zls

  # Archives
  ark
  unrar
  unzip
  atool
  zip
  p7zip

  # Utils
  asciinema
  binutils
  curl
  direnv
  entr
  exfat
  ffmpeg
  file
  fzf
  git
  gitAndTools.gh
  gitu
  gperftools
  graphviz
  htop
  hunspell
  hunspellDicts.en_US
  jq
  kcov
  kdePackages.kconfig
  kdePackages.kdbusaddons
  libimobiledevice
  linuxPackages.perf
  ntfs3g
  patchelf
  pciutils
  rr
  s-tui
  shellcheck
  v4l-utils
  virtiofsd
  wally-cli
  wget
  wl-clipboard

  # Rust stuff
  bat
  du-dust
  eza
  fd
  helix
  hyperfine
  ripgrep
  tokei

  (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
   pkgs.buildFHSUserEnv (base // {
     name = "fhs";
     targetPkgs = pkgs: (base.targetPkgs pkgs) ++ [pkgs.pkg-config pkgs.pam];
     profile = "export FHS=1";
     runScript = "fish";
     extraOutputsToInstall = ["dev"];
   }))
]
