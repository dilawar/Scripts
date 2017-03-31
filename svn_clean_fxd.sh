#!/bin/bash - 
#===============================================================================
#
#          FILE: svn_clean_fxd.sh
# 
#         USAGE: ./svn_clean_fxd.sh 
# 
#   DESCRIPTION:  Like git -fxd
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 03/29/2017 01:08:03 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -x
svn st | grep '^?' | awk '{print $2}' | xargs rm -rf
