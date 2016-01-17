#!/bin/sh

file=$1
line=$2
col=$3
emacsclient -c -a "" -e \
    "(progn

       ;; Load the file
       (find-file \"$file\")

       ;; Jump to the same point as in IntelliJ
       ;; Unfortunately, IntelliJ doesn't always supply the values
       ;; depending on where the open is invoked from; e.g. keyboard
       ;; works, tab context doesn't
       (when (not (string= \"\" \"$line\"))
         (goto-char (point-min))
         (forward-line (1- $2))
         (forward-char (1- $3)))

       (select-frame-set-input-focus (selected-frame)))"
