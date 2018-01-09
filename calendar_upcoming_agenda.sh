#!/bin/bash -
#===============================================================================
#
#          FILE: calendar_upcoming_agenda.sh
#
#         USAGE: ./calendar_upcoming_agenda.sh
#
#   DESCRIPTION: Show upcoming agenda in next 2 hours.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Monday 08 January 2018 09:54:51  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

FROM=$(date +%H:%M)
TO=$(date -d "$FROM today + 90 minutes"  +'%H:%M')
gcalcli --calendar "dilawar" agenda #$FROM $TO
