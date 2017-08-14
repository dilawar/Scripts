#!/bin/bash -
#===============================================================================
#
#          FILE: update_work.sh
#
#         USAGE: ./update_work.sh
#
#   DESCRIPTION:  Update my $HOME/Work
#
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: Wednesday 26 July 2017 11:19:40  IST
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -e
set -x

# First update the HOME.

(
    cd $HOME/Work
    svn up 
    NOW=$(date +"%Y_%m_%d__%H_%M_%S")
    # Then commit all changes.
    svn ci -m "Auto update on $NOW"
)



