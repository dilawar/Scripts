#!/bin/bash -
#===============================================================================
#
#          FILE: log_event.sh
#
#         USAGE: ./log_event.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Friday 09 February 2018 09:34:56  IST
#      REVISION:  ---
#===============================================================================

EVENT=${1:-UNKNOWN}
TIMESTAMP=$(date +"%T")
echo "$TIMESTAMP,$EVENT" >> $HOME/.cache/events.log 
