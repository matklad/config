

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

cask "adguard"
cask "alfred"
cask "font-jetbrains-mono"
cask "ghostty"
cask "hammerspoon"
cask "karabiner-elements"
cask "keymapp"
cask "mullvadvpn"
cask "obs"
cask "signal"
cask "slack"
cask "spotify"
cask "telegram"
cask "visual-studio-code"
cask "zed"
cask "utm"
cask "crystalfetch"

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
