#!/bin/sh

function find_window {
    windows=$(wmctrl -lx | awk -v name=".Emacs" '$3 ~ name' | grep -v "Hangouts")
}


find_window
if [ $? != 0 ];
then
    emacsclient -c -a "" -e "(select-frame-set-input-focus (selected-frame))"
else
    set -- $windows
    if [ $1 ]; then
        wmctrl -ia $1
    fi
fi
