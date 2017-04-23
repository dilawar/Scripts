#!/bin/bash - 
#===============================================================================
#
#          FILE: ghevar_vbox.sh
# 
#         USAGE: ./ghevar_vbox.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 04/23/2017 10:20:47 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
ssh -Y dilawars@ghevar virtualbox

