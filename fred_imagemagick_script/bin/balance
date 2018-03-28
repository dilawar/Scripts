#!/bin/bash
#
# Developed by Fred Weinhaus 5/10/2010 .......... 11/7/2014
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
# USAGE: balance "shadows,midtones,highlights" [-b bias] infile outfile
# USAGE: balance [-h or -help]
#
# OPTIONS:
#
# "shadows,midtones,highlights"      percent change in values in shadows, 
#                                    midtones, highlight areas in the image;
#                                    positive or negative floats
# -b             bias                percent shift in midtone location; 
#                                    positive or negative floats; default is 
#                                    no change from mean of image.
#
###
#
# NAME: BALANCE 
# 
# PURPOSE: Enhance the shadows, midtones and highlight regions of the image.
# 
# DESCRIPTION: BALANCE enhances the shadows, midtones and highlight regions 
# of the image. This is done according to a three point piece-wise linear 
# transformation, where the three points are at input graylevels: 
# 0, image mean, 255 (in 8-bit system). The transformation is applied as a 
# look up table using -clut.
# 
# 
# OPTIONS: 
# 
# "shadows,midtones,highlights" ... Percent change in values in the shadows, 
# midtones and highlight areas of the image. Values are positive or negative 
# floats. A value of "0,0,0" means the output will be the same as the input.
#
# -b bias ... BIAS is the percent shift of the midtone location from the mean 
# value of the input. Values are positive or negative floats. The default=0 
# indicates no change from the global mean value of all channels of the 
# input image.
# 
# REQUIREMENT: IM version 6.3.5-7 or higher due to the use of -clut.
#
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
values="0,0,0"		# percent changes in shadows, midtones, hightlights
bias="0"			# percent shift in midtone locaation

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
elif [ $# -gt 5 ]
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
				-b)    # get bias
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID BIAS SPECIFICATION ---"
					   #checkMinus "$1"
					   bias=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$bias" = "" ] && errMsg "--- BIAS=$bias MUST BE A POSITIVE OR NEGATIVE FLOAT ---"
 					   ;;
		   [-0-9,]*)   # get values for shadows,midtones,highlights
		   			   values=$1
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

# set temp files
tmpA1="$dir/balance_1_$$.mpc"
tmpB1="$dir/balance_1_$$.cache"
tmpA2="$dir/balance_2_$$.mpc"
tmpB2="$dir/balance_2_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" ERR

# test input image
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"


# find midpoint
midx=`convert $tmpA1 -format "%[fx:round(255*(mean+$bias/100))]" info:`
midx=`convert xc: -format "%[fx: $midx<0?0:$midx]" info:`
midx=`convert xc: -format "%[fx: $midx>255?255:$midx]" info:`
midy=`convert xc: -format "%[fx:$midx/255]" info:`
#echo "midx=$midx; midy=$midy"

# get individual values
numvals=`echo $values | tr "," " " | wc -w`
[ $numvals -ne 3 ] && errMsg "--- THREE VALUES ARE REQUIRED FOR SHADOWS,MIDTONES,HIGHLIGHTS ---"
values=`echo $values | sed 's/[ ]*//g'`
vals=`echo $values | cut -d, -f 1`
valm=`echo $values | cut -d, -f 2`
valh=`echo $values | cut -d, -f 3`
vals=`convert xc: -format "%[fx:$vals/100]" info:`
valm=`convert xc: -format "%[fx:$midy+$valm/100]" info:`
valh=`convert xc: -format "%[fx:1+$valh/100]" info:`
#echo "$vals, $valm, $valh"

# create 256 long gradient for use as lut
x1=0
y1=$vals
x2=$midx
y2=$valm
x3=255
y3=$valh
convert -size 256x1 xc: -fx "(i>=0&&i<$midx)?(i-$x1)*($y2-$y1)/($x2-$x1)+$y1:(i-$x2)*($y3-$y2)/($x3-$x2)+$y2" $tmpA2
#vv=`echo $values | tr "," "_" | tr "-" "m"`
#bb=`echo $bias | tr "-" "m"`
#im_profile $tmpA2 balance_${vv}_b${bb}_profile.gif

# process image
convert $tmpA1 $tmpA2 -clut "$outfile"

exit 0

