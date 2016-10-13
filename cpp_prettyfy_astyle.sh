#!/bin/bash - 
#===============================================================================
#
#          FILE: cpp_prettyfy_astyle.sh
# 
#         USAGE: ./cpp_prettyfy_astyle.sh 
# 
#   DESCRIPTION: 
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 10/13/2016 03:56:26 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
FILENAME="$1"
if [ ! -f $(FILENAME) ]; then 
    echo "$FILENAME does not exists";
    exit;
fi

astyle -A1 --indent-spaces=4 --break-blocks --pad-oper --pad-paren \
    --pad-header --max-code-length=80 $(FILENAME) 


