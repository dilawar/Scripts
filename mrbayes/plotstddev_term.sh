#!/bin/bash

## Plot StdDev trace from a MrBayes *.mcmc file
## 06/09/2008 10:47:39 PM CEST
## 03/11/2014 03:58:06 PM
## By: Johan A. A. Nylander

## Default stop val in MrBayes v. 3.2.3
STOPVALDEFAULT="0.05"

## Check if gnuplot can be found
GNUPLOT=$(which gnuplot)
if [ -x "$GNUPLOT" ]; then
    echo
  else
    echo "gnuplot can not be found in the PATH. Quitting."
    exit 1
fi

## Check number of arguments
if [ $# -gt 2  ] || [ ! $1 ] ; then
  echo
  echo "Usage: `basename $0` file.mcmc <stop value>"
  echo
  exit 1
fi

## Check input file
if [ -f "$1" ] ; then
      FILE=$1
  else
      echo
      echo "File \"$1\" does not exist."
      echo "Usage: `basename $0` file.mcmc <stop value>"
      exit 1
fi

## Check stop value
if [ ! "$2" ] ; then
      STOPVAL=$STOPVALDEFAULT
  elif [ "$2" ] ; then 
      STOPVAL=$2
fi

## Get number of columns in the mcmc file. StdDev should be the last.
NF=$(awk 'END{print NF}' $FILE)

## Do the plotting
echo "Using gnuplot on file: \"$FILE\"."

echo 'set term dumb; plot [:] [0:*] "'$FILE'" u 1:'$NF' t "Average SD of split frequencies" w l lw 3; f(x)='$STOPVAL'; replot f(x) t "Stop value in MrBayes" w l lw 2 lt 2' | $GNUPLOT

echo Done.

