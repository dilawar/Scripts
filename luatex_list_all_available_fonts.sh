#!/bin/bash - 
#===============================================================================
#
#          FILE: luatex_list_all_available_fonts.sh
# 
#         USAGE: ./luatex_list_all_available_fonts.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 01/16/2017 10:39:35 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
export TEXMF=/usr/share/texmf
mtxrun --generate
mtxrun --script fonts --list --all

