{pkgs, ...}: with pkgs; [
  # GUI
  (vivaldi.override { proprietaryCodecs = true; enableWidevine = false; commandLineArgs = "--disable-features=AllowQt"; })
  anki
  filelight
  gimp
  kdialog
  kitty
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
  qpdfview
  qview
  haruna

  # Langs
  (python3.withPackages (py: [py.requests]))
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
  git
  gitAndTools.gh
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
