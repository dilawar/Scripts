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
sudo date -s "$(wget -qSO- --max-redirect=0 google.in 2>&1 | grep Date: | cut -d' ' -f5-8)Z"
