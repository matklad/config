#!/usr/bin/env bash
set -e
xbindkeys
rm -fr ~/downloads/* || true
sleep 2
xkbcomp ~/config/home-row.xkb $DISPLAY
