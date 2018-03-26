#!/bin/sh -
#===============================================================================
#
#          FILE: fred.sh
#
#         USAGE: ./fred.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 03/26/2018 09:57:52 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

if [[ $# -lt 1 ]]; then
    echo "Following scripts are available"
    ls $SCRIPT_DIR/fred_imagemagick_script/bin
    exit
fi

SCRIPTNAME=$1;shift;
FREDSCRIPT=$SCRIPT_DIR/fred_imagemagick_script/bin/$SCRIPTNAME
echo "Executing $FREDSCRIPT with args $@"
sh $FREDSCRIPT "$@"
