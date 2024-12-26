

brew "deno"
brew "fish"
brew "fzf"
brew "gh"
brew "git"
brew "go"
brew "htop"
brew "kanata"
brew "lima"
brew "eza"
brew "node"
brew "ripgrep"
brew "rustup"
brew "unar"
brew "ffmpeg"

cask "font-jetbrains-mono"
cask "hammerspoon"
cask "karabiner-elements"
cask "mullvadvpn"
cask "obs"
cask "slack"
cask "spotify"
cask "telegram"
cask "visual-studio-code"
cask "ghostty"
cask "zed"
cask "keymapp"
cask "alfred"

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
