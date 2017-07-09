#!/bin/bash -l
#
#$ -S /bin/bash
#$ -cwd
#$ -V

## Change the name below (after -N) to suite your requirement
#$ -N MrBayes

## Change the number of cores demanded (after orte) to suite your code-run requirement
#$ -pe orte 10

echo "Starting MPI job at: " `date`
echo "Starting MPI job on: " `hostname`
echo "Total cores demanded: " $NSLOTS
echo "Job name given: " $JOB_NAME
echo "Job ID: " $JOB_ID
echo "Starting MPI job..."

echo "Executing command $*"

## Change the executable to match your path and executable filename (last line with ./xxx)
/usr/lib64/openmpi/bin/mpirun -np $NSLOTS ./run.sh

exit 0
