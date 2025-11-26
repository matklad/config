

brew "anyzig"
brew "asciinema"
brew "bat"
brew "cmake"
brew "deno"
brew "emacs"
brew "eza"
brew "fd"
brew "ffmpeg"
brew "fish"
brew "fzf"
brew "gh"
brew "git-absorb"
brew "git"
brew "go"
brew "htop"
brew "imagemagick"
brew "kanata"
brew "llm"
brew "ninja"
brew "node"
brew "qemu"
brew "ripgrep"
brew "rustup"
brew "tokei"
brew "unar"
brew "entr"

cask "alfred"
cask "chromium"
cask "claude-code"
cask "font-jetbrains-mono"
cask "ghostty"
cask "hammerspoon"
cask "karabiner-elements"
cask "keymapp"
cask "mullvad-vpn"
cask "obs"
cask "signal"
cask "slack"
cask "spotify"
cask "telegram"
cask "visual-studio-code"
cask "zed"

require 'socket'
hostname = Socket.gethostname
puts hostname

if hostname == "TigerMac.local"
  cask "slack"
end

if hostname == "ahab.local"
  cask "qbittorrent"
  cask "vlc"
end
