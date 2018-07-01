#!/usr/bin/env bash
alias vim=nvim
source $HOME/config/scripts/env.sh

alias ls="exa -l"
alias l="exa -l"

alias g="git"
alias gs="git status"
alias gcm="git checkout master"
alias gup="git pull --rebase"
alias gbda="git branch --merged | egrep -v \"(^\*|master|dev)\" | xargs git branch -d"
alias ctrlc="xclip -selection c"
alias sb="sublime"
alias mg="e --eval \"(progn (call-interactively #'magit-status) (call-interactively #'delete-other-windows))\""
export VISUAL=nvim

