#!/bin/sh
# Run urxvt daemon
urxvtd -q -o -f
# Set xserver parameters
xset s 0
xset dpms 0 0 1800
# Launch awesome
exec /usr/bin/awesome
