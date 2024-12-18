set fish_greeting ""
set -U fish_prompt_pwd_dir_length 0

abbr "c" "code"
abbr "g" "git"
abbr "gga" "gg amend"
abbr "ggc" "gg commit"
abbr "gl" "git log"
abbr "gp" "git push"
abbr "gpf" "git push --force-with-lease"
abbr "gs" "git status"
abbr "gup" "git pull --rebase"
abbr "ls" "lsd -l"
abbr "sw" "gg switch"
abbr "swd" "git switch --detach"
abbr "zz" "./zig/zig"

fish_config theme choose termcolors
