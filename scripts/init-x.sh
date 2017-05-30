#!/usr/bin/env bash
set -e
xbindkeys
sleep 2
xkbcomp ~/config/home-row.xkb $DISPLAY
