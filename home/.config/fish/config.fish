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
abbr "ls" "eza -l"
abbr "sw" "gg switch"
abbr "swd" "git switch --detach"
abbr "zz" "./zig/zig"

alias "rr" "~/config/tools/remote-run.sh  matklad 192.168.2.4"
alias "rs" "~/config/tools/remote-sync.sh matklad 192.168.2.4"
alias "rsh" "ssh matklad@192.168.2.4"
alias q "llm -s \"Answer in as few words as possible. Use a brief style with short replies.\" -m claude-3.5-sonnet";

fish_config theme choose termcolors
