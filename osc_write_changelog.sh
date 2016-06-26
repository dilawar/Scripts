#!/bin/bash - 
#===============================================================================
#
#          FILE: osc_write_changelog.sh
# 
#         USAGE: ./osc_write_changelog.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 06/26/2016 11:03:13 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
osc vc

