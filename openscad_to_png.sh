#!/bin/bash - 
#===============================================================================
#
#          FILE: openscad_to_png.sh
# 
#         USAGE: ./openscad_to_png.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 06/10/2017 11:24:19 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

openscad -o $1.png $1 --camera=0,0,0,25,0,35,500 --projection
