#!/bin/bash 
ffmpeg -i $1 -vcodec copy -acodec copy "$1.avi"
