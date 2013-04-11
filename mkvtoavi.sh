#!/bin/bash 
ffmpeg -i $1 -f avi -fs 20 "$1_[slow_pc].avi"
