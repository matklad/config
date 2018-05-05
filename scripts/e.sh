#!/usr/bin/env bash
tmux set prefix C-t
emacsclient -nw \
	    --socket-name=/tmp/emacs1000/server \
	    --alternate-editor= \
	    "$@"
tmux set prefix C-x
