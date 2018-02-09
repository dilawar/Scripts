#!/bin/bash -
#===============================================================================
#
#          FILE: record_mic.sh
#
#         USAGE: ./record_mic.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Friday 09 February 2018 10:15:27  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

NOW=$(date +"%Y_%m_%d__%H_%M_%S")
echo "Recording ..."
arecord -d 30 -t wav /tmp/recording_$NOW.wav
