#!/bin/bash - 
#===============================================================================
#
#          FILE: video_shrink.sh
# 
#         USAGE: ./video_shrink.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 04/14/2017 12:22:15 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
IFNILE=$1
OUTFILE=$2
ffmpeg -i $INFILE -s 640x480 -b:v 512k -vcodec mpeg1video -acodec copy $OUTFILE
