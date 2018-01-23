#!/bin/bash -
#===============================================================================
#
#          FILE: link_to_real_file.sh
#
#         USAGE: ./link_to_real_file.sh
#
#   DESCRIPTION:  Convert link with real file.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Tuesday 23 January 2018 04:57:50  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

if [ $# -lt 1 ]; then
    echo "USAGE: $0 link_name"
fi

LINKNAME="$1"
[ -L "$LINKNAME" ] && rsync "$(readlink $LINKNAME)" $LINKNAME
