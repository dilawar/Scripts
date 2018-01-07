i#!/bin/bash -
#===============================================================================
#
#          FILE: graphml_plot.sh
#
#         USAGE: ./graphml_plot.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Sunday 07 January 2018 12:19:48  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
set -e 

FILE=$1; shift
graphml2gv $FILE $FILE.gv
neato -Tpng $@ $FILE.gv > $FILE.png
