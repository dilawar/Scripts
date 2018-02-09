#!/bin/bash -
#===============================================================================
#
#          FILE: sound_detect.sh
#
#         USAGE: ./sound_detect.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Thursday 08 February 2018 05:14:35  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

while true; do
    arecord -d 1 /dev/shm/tmp_rec.wav ; sox -t .wav /dev/shm/tmp_rec.wav -n \
        stat 2>&1 | grep "Maximum amplitude" | cut -d ':' -f 2 
    # if [ $? -eq 0 ] ; then
    #      amixer set Master 0
    # else
    #      amixer set Master 80
    # fi
done
