#!/bin/bash - 
#===============================================================================
#
#          FILE: notify_me.sh
# 
#         USAGE: ./notify_me.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 11/12/2016 10:33:44 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error


# From here:
# http://askubuntu.com/questions/409611/desktop-notification-when-long-running-commands-complete

function alert() 
{
    start=$(date +%s)
    "$@"
    [ $(($(date +%s) - start)) -le 15 ] || \
        notify-send "Notification" \
        "Long running command \"$(echo $@)\" took $(($(date +%s) - start)) seconds to finish"
}
