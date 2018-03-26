#!/bin/bash
#
# Developed by Fred Weinhaus 5/27/2008 .......... revised 4/25/2015
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
# USAGE: glow [-a amount] [-s softening] infile outfile
# USAGE: pixelize [-h or -help]
#
# OPTIONS:
#
# -a      amount           glow amount; float; amount>=1; default=1.5
# -s      softening        softening; integer; soft>=0; default=0
#
###
#
# NAME: GLOW 
# 
# PURPOSE: To create a glowing effect in an image.
# 
# DESCRIPTION: GLOW creates a glowing effect in an image by 
# applying a gain factor (amount) and optionally a blurring (soft). 
# 
# OPTIONS: 
# 
# -a amount ... AMOUNT of glow expressed as a gain factor. Values floats  
# greater than or equal to 1. A value of 1 produces no glow. Larger 
# values increase the glow. Typical values are between 1 and 2. The 
# default is 1.5.
#
# -s softening ... SOFTENING creates a soft blurring effect to the glow. 
# Values are integers greater than or equal to 0. A value of 0 produces 
# no softening. Larger values increase the softening. Typical values 
# are between 0 and 50. The default is 0.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
amount=1.5
soft=0

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
elif [ $# -gt 6 ]
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
				-a)    # get amount
					   shift  # to get the next parameter - amount
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT SPECIFICATION ---"
					   checkMinus "$1"
					   amount=`expr "$1" : '\([.0-9]*\)'`
					   [ "$amount" = "" ] && errMsg "AMOUNT=$amount MUST BE A FLOAT"
		   			   amounttest=`echo "$amount < 1" | bc`
					   [ $amounttest -eq 1 ] && errMsg "--- AMOUNT=$amount MUST BE A FLOAT GREATER THAN OR EQUAL TO ONE ---"
					   ;;
				-s)    # get soft
					   shift  # to get the next parameter - soft
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SOFT SPECIFICATION ---"
					   checkMinus "$1"
					   soft=`expr "$1" : '\([0-9]*\)'`
					   [ "$soft" = "" ] && errMsg "SOFT=$soft MUST BE A NON-NEGATIVE INTEGER"
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


# test if image an ordinary, readable and non-zero size
if [ -f $infile -a -r $infile -a -s $infile ]
	then
	: 'Do Nothing'
else
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

#process image
if [ "$soft" = "0" ]
	then
	convert $infile -evaluate multiply $amount $outfile
else
	sigma=`convert xc: -format "%[fx:$soft/3]" info:`
	amount=`convert xc: -format "%[fx:$amount-1]" info:`
	convert $infile \( +clone -evaluate multiply $amount -blur 0x$sigma \) \
		-compose plus -composite "$outfile"
fi
exit 0