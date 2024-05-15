alias ls "exa -l --icons"
alias icat "kitty +kitten icat --align left"
bind \e\[3\;5~ kill-word
bind \cx 'fish_clipboard_copy; commandline -f kill-whole-line'
set fish_greeting ""
set -U fish_prompt_pwd_dir_length 0
