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
    git clone git@github.com:matklad/$repo.git
    pushd $repo
    git remote add upstream git@github.com:$userrepo.git
    git fetch upstream
    git checkout master
    git reset --hard upstream/master
    git branch --set-upstream-to=upstream/master
    popd
}

function gpr() {
    local pr=$1
    git fetch upstream pull/$pr/head:pr-$pr
    git checkout pr-$pr
}

# put this either in bashrc or zshrc
function nixify() {
  if [ ! -e ./.envrc ]; then
    echo "use nix" > .envrc
    direnv allow
  fi
  if [ ! -e default.nix ]; then
    cat > shell.nix <<'EOF'
with import <nixpkgs> {};
pkgs.mkShell {
  buildInputs = [ 
  ];
}
EOF
    e shell.nix
  fi
}
export VISUAL="/home/matklad/config/scripts/e.sh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
RPROMPT=''
eval "$(direnv hook zsh)"
