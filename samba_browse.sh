#!/bin/bash -
#===============================================================================
#
#          FILE: lookup_samba.sh
#
#         USAGE: ./lookup_samba.sh
#
#   DESCRIPTION: 
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Wednesday 28 March 2018 10:09:17  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                                  # Treat unset variables as an error
sudo nmblookup -S WORKGROUP
