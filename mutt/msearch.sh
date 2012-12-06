#!/bin/sh
#
# $Id: msearch.sh 30 2007-04-08 15:41:53Z vdanen $
#
# wrapper script to execute mairix searches

mairix=/sw/bin/mairix
mairixrc=~/.mairixrc

mbox="`grep mfolder ${mairixrc} | cut -d '=' -f 2`"
echo "Deleting old results folder..."
rm -f ${mbox}
echo "Updating index..."
${mairix} -p
echo "Executing mairix search..."
${mairix} $*
