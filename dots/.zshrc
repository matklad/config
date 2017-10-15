export PURE_PROMPT_SYMBOL=λ
fpath+=~/.zfunc
source "$HOME/.zprezto/init.zsh"
unsetopt extendedglob
source $HOME/config/scripts/init-user-shell.sh

unalias rm
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
