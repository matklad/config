{pkgs, ...}: with pkgs; [
  # GUI
  (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; qt = qt6; })
  filelight
  gimp
  kdialog
  kitty
  obs-studio
  qbittorrent
  spectacle
  vscode
  steam-run
  spotify

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
  dotnet-sdk_7
  gdb
  gnumake
  go
  # llvmPackages_16.bintools
  # llvmPackages_16.clang
  # llvmPackages_16.lldb
  # llvmPackages_16.llvm
  # llvmPackages_16.stdenv
  llvmPackages_15.bintools
  llvmPackages_15.clang
  llvmPackages_15.lldb
  llvmPackages_15.llvm
  llvmPackages_15.stdenv
  lua5_4
  maven
  nil
  ninja
  nodejs
  protobuf
  rustup
  valgrind
  kcachegrind
  # zig_0_11
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
  virtiofsd
  wally-cli
  wget
  wl-clipboard

  # Rust stuff
  bat
  du-dust
  eza
  fd
  gitui
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
