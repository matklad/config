#!/usr/bin/env bash
emacsclient --create-frame \
	    --socket-name=/tmp/emacs1000/server \
	    --alternate-editor= \
	    "$@"
