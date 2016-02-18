#!/bin/bash

function find_window {
    windows=$(wmctrl -lx | awk -v name="$1" '$3 ~ name' | grep -v "Hangouts")
}

key=$1
bin=$2
shift 2
args=$@

find_window $key
if [ $? != 0 ];
then
    $bin $args
    sleep 0.2
fi
find_window $key

set -- $windows
if [ $1 ]; then
    wmctrl -ia $1
fi
