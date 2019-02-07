#!/bin/bash - 
#===============================================================================
#
#          FILE: create_tar.sh
# 
#         USAGE: ./create_tar.sh 
# 
#   DESCRIPTION:  Create archive of a given directory.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 09/05/2016 12:32:06 PM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
#set -x
set -e

if [[ $# -lt 1 ]]; then
    #statements
    echo "USAGE: $0 dir"
    exit
fi
dir=`realpath $1`
dirname=`basename $dir`
NOW=$(date +"%Y_%m_%d__%H_%M_%S")
archiveName="${dirname}_${NOW}.tar.gz"
#archiveDir="$HOME/Documents/archives/"
archiveDir=`pwd`
mkdir -p $archiveDir
archivePath="$archiveDir/$archiveName"

# -h follows sybolic link.
tar cvfzh $archivePath --exclude-vcs $dirname/
echo "Wrote archive to $archivePath, size " `du -h $archivePath`
