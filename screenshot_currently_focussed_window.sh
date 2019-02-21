#!/usr/bin/env bash
$PICDIR=$HOME/Pictures/SCREENSHOTS
mkdir -p $PICDIR
scrot -u '%Y%m%d_%H%M%S.png' -e 'mv $f $PICDIR'
