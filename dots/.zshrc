export PURE_PROMPT_SYMBOL=λ
source "$HOME/.zprezto/init.zsh"
unsetopt extendedglob
source $HOME/config/scripts/init-user-shell.sh

unalias gcf
function gcf() {
    local userrepo=$1
    local repo=`basename $userrepo`
    git clone https://github.com/$userrepo.git
    pushd $repo
    git remote add fork git@github.com:matklad/$repo.git
    git fetch fork
    popd
}

alias g="git"
alias gs="git status"
alias gcm="git checkout master"
alias gup="git pull --rebase"
alias gbda="git branch --merged | egrep -v \"(^\*|master|dev)\" | xargs git branch -d"
alias ctrlc="xclip -selection c"
alias l="ls -lAh"
export VISUAL=nvim
