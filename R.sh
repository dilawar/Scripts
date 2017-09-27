#!/bin/bash - 
#===============================================================================
#
#          FILE: R.sh
# 
#         USAGE: ./R.sh 
# 
#   DESCRIPTION:  Execute R script.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 05/29/2017 09:24:47 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
if [ ! $1 ]; then 
    echo "USAGE $0 script.R"
    exit;
fi
R BATCH --save < $1
