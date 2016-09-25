#!/bin/bash - 
#===============================================================================
#
#          FILE: udev_restart.sh
# 
#         USAGE: ./udev_restart.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 09/23/2016 05:34:26 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
sudo devadm control --reload-rules
sudo devadm trigger 

