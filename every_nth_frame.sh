#!/bin/bash - 
#===============================================================================
#
#          FILE: every_nth_frame.sh
# 
#         USAGE: ./every_nth_frame.sh 
# 
#   DESCRIPTION:  Shell script to get every nth frame
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 07/20/2016 11:45:30 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -x 
set -e

VIDEOFILE=$1
NTH=$2

FRAMEDIR="$VIDEOFILE_FRAMES"
mkdir -p "$FRAMEDIR"

if [ $# -lt 2 ]; then
    echo "USAGE: $0 video_file nth"
    exit
fi

ffmpeg -i "$VIDEOFILE" -vf "select=not(mod(n\,$NTH))" \
    -vsync vfr "$FRAMEDIR/img_%03d.png"
