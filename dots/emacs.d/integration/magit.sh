#!/bin/sh

file=$1
emacsclient -c -a "" -e \
    "(progn

       (select-frame-set-input-focus (selected-frame))
       ;; Load the file
       (find-file \"$file\")
       (split-window-horizontally)
       (magit-status))"
