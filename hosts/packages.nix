{pkgs, ...}: with pkgs; [
  # GUI
  (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; })
  filelight
  gimp
  kdialog
  kitty
  # kooha
  obs-studio
  qbittorrent
  spectacle
  vscode

  # Comms
  discord
  slack
  tdesktop
  zoom-us

  # Viewers
  audacious
  qpdfview
  qview
  haruna

  # Langs
  (python3.withPackages (py: [py.requests]))
  ant
  cmake
  deno
  gdb
  gnumake
  go
  llvmPackages_latest.bintools
  llvmPackages_latest.clang
  llvmPackages_latest.lldb
  llvmPackages_latest.llvm
  llvmPackages_latest.stdenv
  lua5_4
  maven
  nil
  ninja
  nodejs
  protobuf
  rustup
  valgrind
  zig_0_10
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
  git
  gitAndTools.gh
  gperftools
  graphviz
  htop
  hunspell
  hunspellDicts.en_US
  jq
  libimobiledevice
  linuxPackages.perf
  lnav
  ntfs3g
  patchelf
  pciutils
  rr
  s-tui
  shellcheck
  v4l-utils
  wally-cli
  wget
  wl-clipboard

  # Rust stuff
  bat
  du-dust
  exa
  fd
  gitui
  helix
  hyperfine
  ripgrep
  tokei

  (let base = pkgs.appimageTools.defaultFhsEnvArgs; in
   pkgs.buildFHSUserEnv (base // {
     name = "fhs";
     targetPkgs = pkgs: (base.targetPkgs pkgs) ++ [pkgs.pkg-config];
     profile = "export FHS=1";
     runScript = "fish";
     extraOutputsToInstall = ["dev"];
   }))
]
