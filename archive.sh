#!/bin/bash - 
#===============================================================================
#
#          FILE: archive.sh
# 
#         USAGE: ./archive.sh filename target_path
# 
#   DESCRIPTION: Archive a single file to given directory.
# 
#       OPTIONS: ---
#  REQUIREMENTS: ---
#          BUGS: ---
#         NOTES: ---
#        AUTHOR: Dilawar Singh (), dilawars@ncbs.res.in
#  ORGANIZATION: NCBS Bangalore
#       CREATED: 07/02/2016 09:23:42 AM
#      REVISION:  ---
#===============================================================================

set -o nounset                              # Treat unset variables as an error
set -e

if [ $# -lt 2 ]; then 
    echo "USGAE: $0 filename destination"
    exit 
fi

FILE="$1"
DEST="$2"
mkdir -p ${DEST}

# First, append the timestamp and git-id to filename.
GITID=`git log --pretty=format:'%h' -n 1 || echo ""`
NOW=$(date +"%c" | tr ' :' '__' )

INPUTFILE=`basename $FILE`
EXTENSION="${INPUTFILE##*.}"
FILENAME="${INPUTFILE%.*}"

NEWNAME="${DEST}/${FILENAME}_${GITID}_${NOW}.${EXTENSION}"

echo "Archiving $FILE to $NEWNAME"
rsync -azv --progress $FILE $NEWNAME
