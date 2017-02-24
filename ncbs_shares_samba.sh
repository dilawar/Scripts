#!/bin/bash - 
#===============================================================================
#
#          FILE: ncbs_shares_samba.sh
# 
#         USAGE: ./ncbs_shares_samba.sh 
# 
#   DESCRIPTION:  Operation on smb client
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 02/03/2017 01:46:55 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error

smbclient -L shares.ncbs.res.in -U dilawars
