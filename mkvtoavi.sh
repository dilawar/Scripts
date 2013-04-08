#!/bin/bash 
ffmpeg -i $1 -vcodec copy -acodec copy "$1_[slow_pc].avi"
