set -U fish_prompt_pwd_dir_length 0
set -U fish_greeting ""

abbr --add g git
abbr --add gs git status --short --branch
abbr --add gp git push
abbr --add gup git pull --rebase
abbr --add gco git checkout
abbr --add gcm git checkout master

abbr --add ls exa -l
abbr --add la exa -la
abbr --add l  exa -l

abbr --add c code
