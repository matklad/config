set -U fish_prompt_pwd_dir_length 0
set -U fish_greeting ""

abbr --add g   git
abbr --add gb  git branch
abbr --add gc  git commit
abbr --add gl  git log
abbr --add gp  git push
abbr --add gpf git push --force-with-lease
abbr --add gs  git status --short --branch
abbr --add gup git pull --rebase
abbr --add sw  git switch
abbr --add swm git switch master
abbr --add swc git switch -c
abbr --add r+  gh pr comment --body 'bors r+'

abbr --add e micro
abbr --add c code
abbr --add ca cargo
abbr --add w code ~/work/

abbr --add ctrlc xclip -selection c
abbr --add ctrlv xclip -selection c -o

abbr --add xpt ./x.py -i --keep-stage 1 test --pass check
abbr --add xpc ./x.py check

abbr --add cat bat -p

abbr --add j julia --banner no

abbr --add perfr perf record -F 9999 --call-graph dwarf
