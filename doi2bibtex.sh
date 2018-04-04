#!/bin/bash -
#===============================================================================
#
#          FILE: doi2bibtex.sh
#
#         USAGE: ./doi2bibtex.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Wednesday 04 April 2018 10:26:09  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
DOI="$1"
curl -LH "Accept: text/bibliography; style=bibtex" http://dx.doi.org/${DOI}
