#!/bin/bash -
#===============================================================================
#
#          FILE: trigger_jackass.sh
#
#         USAGE: ./trigger_jackass.sh
#
#   DESCRIPTION: Trigger jackass.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Tuesday 13 February 2018 11:19:02  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

SERIAL=${ARDUINO_PORT:-/dev/ttyACM0}

stty -F $SERIAL raw speed 38400
cat 'P' > $SERIAL
