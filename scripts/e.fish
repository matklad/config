#!/usr/bin/env fish

emacsclient --create-frame \
    --socket-name=/tmp/emacs1000/server \
    --alternate-editor='' \
    $argv
