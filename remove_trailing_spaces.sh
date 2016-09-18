#!/bin/bash - 
#===============================================================================
#
#          FILE: remove_trailing_spaces.sh
# 
#         USAGE: ./remove_trailing_spaces.sh 
# 
#   DESCRIPTION:  Remove trailing spaces.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 09/18/2016 03:02:27 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

if [ $# -lt 1 ]; then
    echo "USAGE: $0 filename"
    exit
fi
sed -i '' -e 's/[ \t]*$//' "$1"
