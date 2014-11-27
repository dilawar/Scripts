#!/bin/bash
# Written by Dilawar Singh, 2014
# dilawars@ncbs.res.in
set -e
if [[ ! $# -eq 1 ]]; then
	echo "USAGE: $0 filename"
	exit
fi

if [ ! -f ./command ]; then
	echo "Create a file called command"
	echo "it should contain all mb commands you want to execute on server"
fi

file="$1"
echo "Copying input file to current directory"
cp $file .
file=`basename $file`
COMMAND="$2"
TIMESTAMP=`date +%Y%m%d%H%M%S`

echo "I am going to execute following command on server"
WORKDIR=Work/NISHMA/$TIMESTAMP
mkdir -p $WORKDIR
echo "Sending file to NARGIS server"
rsync -azv $file run.sh sge.sh command nargis:$WORKDIR
ssh -T nargis << EOF
cd $WORKDIR && qsub sge.sh
EOF
echo "Done"
	
	

