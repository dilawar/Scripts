#!/bin/bash 

# This script record your desktop. It calculates your screen resoltion and saves
# the video in ~/Video/output.mkv file. 
# dilawar@ee.iitb.ac.in
Xaxis=$(xrandr -q | grep '*' | uniq | awk '{print $1}' |  cut -d 'x' -f1)
Yaxis=$(xrandr -q | grep '*' | uniq | awk '{print $1}' |  cut -d 'x' -f2)
ffmpeg -f x11grab -s $(($Xaxis))x$(($Yaxis)) -r 25 -i :0.0 -sameq ~/Video/output.mkv
