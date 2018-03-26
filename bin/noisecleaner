#!/bin/bash
#
# Developed by Fred Weinhaus 10/2/2012 .......... revised 4/25/2015
#
# ------------------------------------------------------------------------------
# 
# Licensing:
# 
# Copyright © Fred Weinhaus
# 
# My scripts are available free of charge for non-commercial use, ONLY.
# 
# For use of my scripts in commercial (for-profit) environments or 
# non-free applications, please contact me (Fred Weinhaus) for 
# licensing arrangements. My email address is fmw at alink dot net.
# 
# If you: 1) redistribute, 2) incorporate any of these scripts into other 
# free applications or 3) reprogram them in another scripting language, 
# then you must contact me for permission, especially if the result might 
# be used in a commercial or for-profit environment.
# 
# My scripts are also subject, in a subordinate manner, to the ImageMagick 
# license, which can be found at: http://www.imagemagick.org/script/license.php
# 
# ------------------------------------------------------------------------------
# 
####
#
# USAGE: noisecleaner [-m method] [-n numiter] [-s stopdiff] [-f format] 
# [-p] infile outfile
# 
# USAGE: noisecleaner [-h or -help]
# 
# OPTIONS:
# 
# -m     method       method of cleaning noise; choices are: 1 using  
#                     IM -enhance and 2 using IM -despeckle; default=1
# -n     numiter      number of iterations; integer>0; default=5
# -s     stopdiff     rmse stopping difference between between iterations; 
#                     0<=float<=1; default=0 (do not stop until numiter reached)
# -f     format       output image format; choices are last (iteration) or 
#                     all (iterations); default=last
# -p                  show progress to terminal
# 
###
# 
# NAME: NOISECLEANER 
# 
# PURPOSE: To iteratively clean noise from an image.
# 
# DESCRIPTION: NOISECLEANER iteratively cleans noise from an image. Two 
# methods are available using either IM -enhance or IM -despeckle.
# 
# 
# ARGUMENTS: 
# 
# -m method ... METHOD of cleaning noise. The choices are: 1 using  
# IM -enhance and 2 using IM -despeckle. The default=1
# 
# -n numiter ... NUMITER is the number of iterations to apply. Values are \
# integers>0. The default=5.
# 
# -s stopdiff ... STOPDIFF is the rmse stopping difference between between 
# iterations. Values are 0<=floats<=1. The default=0 which indicates not to 
# stop until numiter is reached. If a value larger than zero is specified, then 
# processing will quit when the rmse difference between iterations is less than 
# or equal to this value. Values larger than 0 will slow the processing in 
# order to compute the rmse difference.
# 
# -f format ... FORMAT is the desired output image format. The choices are 
# last (l), which saves only the last iteration reached or all (a), which saves  
# a multiframe (multilayer) tiff file with each iteration as a frame (layer).
# The default is last. Note the output image file name will have its suffix 
# replaced by .tif
# 
# -p ... show iteration PROGRESS to terminal
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
method=1			# 1 for -enhance and 2 for -despeckle
numiter=5			# number of iterations; integer>0; default=5
stopdiff=0			# rmse stopping difference between between iterations; float>0; default ignore; nominal 0.005
format="last"		# output saves last interation or all iterations
progress="no"		# show progress to terminal

# set directory for temporary files
dir="."    # suggestions are dir="." or dir="/tmp"

# set up functions to report Usage and Usage with Description
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
PROGNAME=`basename $PROGNAME`          # base name of program
usage1() 
	{
	echo >&2 ""
	echo >&2 "$PROGNAME:" "$@"
	sed >&2 -e '1,/^####/d;  /^###/g;  /^#/!q;  s/^#//;  s/^ //;  4,$p' "$PROGDIR/$PROGNAME"
	}
usage2() 
	{
	echo >&2 ""
	echo >&2 "$PROGNAME:" "$@"
	sed >&2 -e '1,/^####/d;  /^######/g;  /^#/!q;  s/^#*//;  s/^ //;  4,$p' "$PROGDIR/$PROGNAME"
	}


# function to report error messages
errMsg()
	{
	echo ""
	echo $1
	echo ""
	usage1
	exit 1
	}


# function to test for minus at start of value of second part of option 1 or 2
checkMinus()
	{
	test=`echo "$1" | grep -c '^-.*$'`   # returns 1 if match; 0 otherwise
    [ $test -eq 1 ] && errMsg "$errorMsg"
	}

# test for correct number of arguments and get values
if [ $# -eq 0 ]
	then
	# help information
   echo ""
   usage2
   exit 0
elif [ $# -gt 11 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
	while [ $# -gt 0 ]
		do
			# get parameter values
			case "$1" in
		  -h|-help)    # help information
					   echo ""
					   usage2
					   exit 0
					   ;;
				-m)    # get  method
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID METHOD SPECIFICATION ---"
					   checkMinus "$1"
					   method=`expr "$1" : '\([0-9]*\)'`
					   [ $method -ne 1 -a $method -ne 2 ] && errMsg "--- METHOD=$method MUST BE EITHER 1 OR 2 ---"
					   ;;
				-n)    # get numiter
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NUMITER SPECIFICATION ---"
					   checkMinus "$1"
					   numiter=`expr "$1" : '\([0-9]*\)'`
					   [ "$numiter" = "" ] && errMsg "--- NUMITER=$numiter MUST BE A NON-NEGATIVE INTEGER ---"
					   test1=`echo "$numiter <= 0" | bc`
					   [ $test1 -eq 1 ] && errMsg "--- NUMITER=$numiter MUST BE AN INTEGER GREATER THAN 0 ---"
					   ;;
				-s)    # get stopdiff
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID STOPDIFF SPECIFICATION ---"
					   checkMinus "$1"
					   stopdiff=`expr "$1" : '\([.0-9]*\)'`
					   [ "$stopdiff" = "" ] && errMsg "--- STOPDIFF=$stopdiff MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   test1=`echo "$stopdiff < 0" | bc`
					   test2=`echo "$stopdiff > 1" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- STOPDIFF=$stopdiff MUST BE A FLOAT BETWEEN 0 AND 1 ---"
					   ;;
				-f)    # get  format
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FORMAT SPECIFICATION ---"
					   checkMinus "$1"
					   format=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$format" in 
					   		last|l) format="last" ;;
					   		all|a) format="all" ;;
					   		*) errMsg "--- FORMAT=$format IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-p)    # get progress
					   progress="yes"
					   ;;
				 -)    # STDIN and end of arguments
					   break
					   ;;
				-*)    # any other - argument
					   errMsg "--- UNKNOWN OPTION ---"
					   ;;
		     	 *)    # end of arguments
					   break
					   ;;
			esac
			shift   # next option
	done
	#
	# get infile and outfile
	infile="$1"
	outfile="$2"
fi

# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"


# set directory for temporary files
# tmpdir="/tmp"
tmpdir="."

dir="$tmpdir/NOISECLEANER.$$"

mkdir "$dir" || errMsg "--- FAILED TO CREATE TEMPORARY FILE DIRECTORY ---"
trap "rm -rf $dir;" 0
trap "rm -rf $dir; exit 1" 1 2 3 15
trap "rm -rf $dir; exit 1" ERR


# test input image
convert -quiet "$infile" +repage  "$dir/tmpI.mpc" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"

# get output file name and suffix
outname=`echo "$outfile" | sed -n 's/^\(.*\)[\.].*$/\1/p'`
suffix=`echo "$outfile" | sed -n 's/^.*[\.]\(.*\)$/\1/p'`

if [ "$method" = "1" ]; then
	proc="-enhance"
elif [ "$method" = "2" ]; then
	proc="-despeckle"
fi


if [ "$progress" = "yes" ]; then
	echo ""
fi

if [ "$format" = "last" ]; then
	convert $dir/tmpI.mpc $dir/tmpR_0.mpc
	for ((i=1; i<=numiter; i++)); do
		j=$((i-1))
		convert $dir/tmpR_$j.mpc $proc $dir/tmpR_$i.mpc
		if [ "$stopdiff" = "0" -a "$progress" = "yes" ]; then
			echo "$i"
		elif [ "$stopdiff" != "0" -a "$progress" = "yes" ]; then
			diff=`compare -metric rmse $dir/tmpR_$j.mpc $dir/tmpR_$i.mpc null: 2>&1 | sed -n 's/^.*[(]\(.*\)[)].*$/\1/p'`
			echo "$i $diff"
			test=`convert xc: -format "%[fx:($diff<=$stopdiff)?1:0]" info:`
			# note if loop runs to completion it will add 1 to i, which has to be subtracted afterwards
			# if loop breaks it does not add 1 to i, so add it here
			if [ $test -eq 1 ]; then
				i=$((i+1)) 
				break
			fi
		fi
	done
	k=$((i-1))
#	echo "i=$i; k=$k"
	convert $dir/tmpR_$k.mpc "${outname}.${suffix}"

elif [ "$format" = "all" ]; then
	convert $dir/tmpI.mpc $dir/tmpR_0.mpc
	# use subshell to save all images to miff
	(
	for ((i=1; i<=numiter; i++)); do
		j=$((i-1))
		convert $dir/tmpR_$j.mpc $proc $dir/tmpR_$i.mpc
		convert $dir/tmpR_$i.mpc miff:-
		#subshell variables need to be sent to stderr to show on terminal
		if [ "$stopdiff" = "0" -a "$progress" = "yes" ]; then
			echo >&2 "$i"
		elif [ "$stopdiff" != "0" -a "$progress" = "yes" ]; then
			diff=`compare -metric rmse $dir/tmpR_$j.mpc $dir/tmpR_$i.mpc null: 2>&1 | sed -n 's/^.*[(]\(.*\)[)].*$/\1/p'`
			echo >&2 "$i $diff"
			test=`convert xc: -format "%[fx:($diff<=$stopdiff)?1:0]" info:`
			# note if loop runs to completion it will add 1 to i, which has to be subtracted afterwards
			# if loop breaks it does not add 1 to i, so add it here
			if [ $test -eq 1 ]; then
				i=$((i+1)) 
				break
			fi
		fi
	done
	) | convert - "${outname}.tif"
fi

if [ "$progress" = "yes" ]; then
	echo ""
fi


exit 0

