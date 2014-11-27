#!/bin/bash

## Plot column data using gnuplot.
## Version: 10/24/2011 08:09:40 PM
## Johan Nylander
##

## Usage function
function usage {
cat <<End_Of_Usage


`basename $0` version 10/24/2011

What:
           Wrapper for plotting MCMC output with gnuplot (gnuplot required)

By:
           jnylander @ users.sourceforge.net

Usage:
           `basename $0` [-b burnin] [-o file] [[-c column] | [-x column][-y column]] file(s)

Options:
           -b burnin  -- specify the number of generations to be discarded
           -c column  -- specify column number. First column is nr. 1.
           -x column  -- use together with -y to plot column y against column x
           -y column  -- as -c, or use with -x to plot column y against column x
           -t         -- plot in terminal instead of device
           -o file    -- plot to file (png format) instead of device
           -v         -- be verbose
           -h         -- print help message

Examples:
           `basename $0` *.p
           `basename $0` -t *.p
           `basename $0` -o out.png *.p
           `basename $0` -b 50000 -c 2 file.p
           `basename $0` -b 50000 -x 1 -y 2 file

Note:
           Pay attention to gnuplot's ability to use numbering
           of, e.g., MCMC generations/samples!
           For example, compare the output from the following
           commands on an output file (.p) from MrBayes:
           `basename $0` -b 50 file.p
           `basename $0` -b 50 -c 2 file.p
           `basename $0` -b 50 -x 1 -y 2 file.p

           The line count done when using the -v option ignore lines starting
           with # or [, but might include the header line. Gnuplot will ignore
           any commented lines (starting with # or [), and will handle headers
           correctly.

           The script uses the standard unix/linux/macosx tools 'grep', 'head',
           'tail, 'awk', and 'wc'. Portability is, however, mostly untested.

End_Of_Usage
    
}

## Check gnuplot
GNUPLOT=`which gnuplot`
if [ -x "$GNUPLOT" ]; then
    echo ''
  else
    echo "gnuplot can not be found in the PATH. Quitting."
    exit 1
fi

## Read arguments
pflag=
bflag=
cflag=
vflag=
tflag=
fflag=
iflag=
burnin=0
xcolumn=1
ycolumn=2 # Note: likelihood is nr 4 in starbeast output. 2 in MrBayes etc.
DUMMYTERM=
FILETERM=
MBCOMMENT=
IMGFORMAT='png'

while getopts 'x:y:b:c:o:f:vht' OPTION
do
  case $OPTION in
  b)	  bflag=1
		  bval="$OPTARG"
		  ;;
  c)	  cflag=1
		  cval="$OPTARG"
		  ;;
  x)	  xflag=1
		  xval="$OPTARG"
		  ;;
  y)	  yflag=1
		  yval="$OPTARG"
		  ;;
  o)	  oflag=1
		  oval="$OPTARG"
		  ;;
  f)	  fflag=1
		  fval="$OPTARG"
		  ;;
  v)      vflag=1
          ;;
  t)      tflag=1
          ;;
  h)     usage
          exit 2
		  ;;
  esac
done
shift $(($OPTIND - 1))


## Put remaining args in files, and let gnuplot choke on non existing files or wrong flags...
FILES="$*"
if [ "$vflag" ] ; then
    echo "files to read: $FILES"
    for file in $FILES ; do
        linecount=`grep -v '^#' $file | grep -v '^\[' | wc -l` # not counting commented lines (starting with # or [)
        echo "  file $file have"
        echo "    (uncommented) lines: $linecount"
        echo -n "    columns: "
        tail -10 $file | head -1 | awk '{ print NF}' # assuming that a few lines from the end is representative
    done
fi


## Check burnin
if [ "$bflag" ] ; then
    if [ "$bval" -ge 0 ] ; then
        BURNIN=$bval
      else
          echo ''
          echo "arg \"$bval\" is not a number."
          usage
          echo ''
          exit 1
    fi
else
    BURNIN=$burnin
fi


## columns
if [ "$cflag" ] ; then
    YCOLUMN=$cval
elif [ "$yflag" ] ; then
        YCOLUMN=$yval
else
    YCOLUMN=$ycolumn
fi
if [ "$xflag" ] ; then
    XCOLUMN=$xval
else
    XCOLUMN=$xcolumn
fi


## the using argument
if [ "$cflag" ] ; then
    USING="$YCOLUMN"
elif [ "$yflag" ] ; then
    if [ $xflag ] ; then
        USING="$XCOLUMN:$YCOLUMN"
    else
        USING="$YCOLUMN"
    fi
else
    USING="$XCOLUMN:$YCOLUMN"
fi


## plot in terminal or not
if [ "$tflag" ] ; then
    DUMMYTERM="set terminal dumb; "
fi


## plot to file or not
if [ "$oflag" ] ; then
    if [ "$fflag" ]; then
        IMGFORMAT=$fval
    fi
    if [ ! "${oval##*.}" = "$IMGFORMAT" ]; then
        OUTFILE=${oval}.$IMGFORMAT
    else
        OUTFILE=$oval
    fi
    FILETERM="set terminal $IMGFORMAT; set output '$OUTFILE';"
fi


## Plot files with gnuplot
if [ "$vflag" ] ; then
    echo -n "will try to plot column nr: $YCOLUMN"
    if [ "$xval" ] ; then
        echo " against: $XCOLUMN"
    else
        echo ''
    fi
    echo "burnin set to: $BURNIN."
    echo 'gnuplot command:'
    echo ''
    if [ "$oflag" ] ; then
        echo ''$FILETERM'set datafile commentschars "#[";set key right bottom;plot ['$BURNIN':] [:] for [filename in '\"$FILES\"'] filename using '$USING' with lines title filename'
    else
        echo ''$DUMMYTERM'set datafile commentschars "#[";set key right bottom;plot ['$BURNIN':] [:] for [filename in '\"$FILES\"'] filename using '$USING' with lines title filename'
    fi
    echo ''
fi


## Do the actual plotting
if [ "$FILES" ] ; then 
    if [ "$oflag" ] ; then
        echo -e ''$FILETERM'set datafile commentschars "#[";set key right bottom;plot ['$BURNIN':] [:] for [filename in '\"$FILES\"'] filename using '$USING' with lines title filename' | $GNUPLOT
    else
        echo -e ''$DUMMYTERM'set datafile commentschars "#[";set key right bottom;plot ['$BURNIN':] [:] for [filename in '\"$FILES\"'] filename using '$USING' with lines title filename' | $GNUPLOT --persist
    fi
else
    usage
    echo
    exit 1
fi

