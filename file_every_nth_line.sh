#!/bin/bash - 
#===============================================================================
#
#          FILE: file_every_nth_line.sh
# 
#         USAGE: ./file_every_nth_line.sh 
# 
#   DESCRIPTION: Print every n'th line of a file
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Wednesday 18 January 2017 12:19:48  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

if [[ $# -lt 2 ]]; then
    echo "USAGE: $0 filename nth"
    exit
    #statements
fi
FILE="$1"
NTH="$2"

# Don't miss the header.
gawk "NR == 1 || NR % ${NTH} == 0" $FILE
