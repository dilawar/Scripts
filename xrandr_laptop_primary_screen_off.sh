#!/usr/bin/env bash
xrandr --output $( xrandr | grep 'eDP.* connected' | head -n1 | awk '{ print $1 }' ) --off
