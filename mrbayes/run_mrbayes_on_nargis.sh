#!/bin/bash
# Written by Dilawar Singh, 2014
# dilawars@ncbs.res.in

## This is command template.
command="charset CR = 1-732;
charset cytB = 733-1846;
charset struct = 1847-2716;
[charset 12S  1847-2218;
charset 16S 2219-2716; ]
partition mtgenes = 3: CR, cytB, struct;
set partition = mtgenes;
exclude  1-160 211-260 364-384;
lset applyto=(1,2,3) nst=6 rates=gamma;
unlink statefreq=(all) revmat=(all) shape=(all) pinvar=(all);
prset applyto=(all) ratepr=variable;
mcmcp ngen=10000000 samplefreq=10000 printfreq=1000 nruns=4 nchains=4 mcmcdiagn=yes diagnfreq=1000000 burnin=200;
mcmc burnin=200;
sumt burnin=200;
sump burnin=200;
"
## This is run.sh
run="#!/bin/bash
mb < command.txt > mrbayes.log
"

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

echo "Generating files to be sent to server"
rm -f command.txt run.sh
echo "execute $file;\n$command" > command.txt
echo "$run" > run.sh
chmod +x run.sh

echo "Sending file to NARGIS server"
rsync -azv $file run.sh sge.sh command.txt nargis:$WORKDIR
ssh -T nargis << EOF
cd $WORKDIR && qsub sge.sh
EOF
echo "Done"
	
	

