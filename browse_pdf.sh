#!/bin/bash -
#===============================================================================
#
#          FILE: browse_pdf.sh
#
#         USAGE: ./browse_pdf.sh
#
#   DESCRIPTION: Browse pdf using impressive
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Thursday 11 January 2018 01:28:31  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

PDFS=${1-*.pdf}
impressive -T0 -w $PDFS
