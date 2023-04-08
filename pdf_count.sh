#!/bin/bash -
#===============================================================================
#
#          FILE: pdf_count.sh
#
#         USAGE: ./pdf_count.sh
#
#   DESCRIPTION:  Count words in pdf. User can pass first argument to this
#   script to count something else.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Thursday 08 March 2018 09:17:21  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
PDFFILE=$1;shift
NWORDS=`pdftotext $PDFFILE -enc UTF-8 - | wc -w`
NCHARS=`pdftotext $PDFFILE -enc UTF-8 - | wc -c`
echo "Words: $NWORDS Chars: $NCHARS"
