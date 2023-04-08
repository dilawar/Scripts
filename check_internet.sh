#!/bin/bash -
#===============================================================================
#
#          FILE: check_internet.sh
#
#         USAGE: ./check_internet.sh
#
#   DESCRIPTION:  Check internet connection in NCBS.
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Friday 16 March 2018 11:00:14  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error

# check proxy.
ping -c 5 proxy.ncbs.res.in || notify-send "Proxy server is unreachable"
ping -c 5 nargis.ncbs.res.in || notify-send "NARGIS is unreachable"
ping -c 5 google.com || notify-send "GOOGLE is unreachable"
