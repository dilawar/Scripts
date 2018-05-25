#!/bin/bash - 
#===============================================================================
#
#          FILE: date_sync_with_google.sh
# 
#         USAGE: ./date_sync_with_google.sh 
# 
#   DESCRIPTION:  Synchronize date via internet.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Friday 24 February 2017 11:52:03  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
DATE=$(wget -qSO- --max-redirect=0 google.co.in 2>&1 | grep Date:)
echo $DATE
DATE=$(echo $DATE | cut -d' ' -f3-)
echo $DATE
sudo date -s "$(echo $DATE)Z"
