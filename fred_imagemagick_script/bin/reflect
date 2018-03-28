#!/bin/bash
#
# Developed by Fred Weinhaus 12/4/2014 .......... revised 4/25/2015
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
# USAGE: reflect [-c centercolumn] infile
# USAGE: reflect [-h or -help]
#
# OPTIONS:
#
# -c    centercolumn    center column (x coordinate) of input image about 
#                       which to reflect horizontally; integer>=0; default 
#                       is the image horizontal center
#
###
#
# NAME: REFLECT
# 
# PURPOSE: To split an image, reflect each side and appends them to form 
# horizontally symmetric images.
# 
# DESCRIPTION: REFLECT splits an image into two parts at the center column. 
# Each side is reflected and append to form left and right symmetric images 
# and also a blended image. The 3 output files will be named from the input 
# image as inname_left.suffix, inname_right.suffix and inname_blend.suffix
# 
# OPTIONS: 
# 
# -c centercolumn ... CENTERCOLUMN is the center column (x coordinate) of the 
# input image about which to reflect horizontally. Values are 
# 0<=interger<=imagewidth. The default is the image horizontal center.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# no fixed default values

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
elif [ $# -gt 3 ]
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
				-c)    # get ccol
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID CENTERCOLUMN SPECIFICATION ---"
					   checkMinus "$1"
					   ccol=`expr "$1" : '\([0-9]*\)'`
					   [ "$ccol" = "" ] && errMsg "--- CENTERCOLUMN=$ccol MUST BE A NON-NEGATIVE INTEGER ---"
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
fi

# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"


# set directory for temporary files
dir="."    # suggestions are dir="." or dir="/tmp"


# setup temporary images
tmpA1="$dir/reflect_1_$$.mpc"
tmpB1="$dir/reflect_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1;" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1; exit 1" ERR


# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"


# get names
inname=`convert -ping -quiet "$infile" -format "%t" info:`
outname=`convert -ping -quiet "$infile" -format "%t" info:`
suffix=`convert -ping -quiet "$infile" -format "%e" info:`

# get image dimensions
ww=`convert -ping $tmpA1 -format "%w" info:`
hh=`convert -ping $tmpA1 -format "%h" info:`

# get default centercolumn
if [ "$ccol" = "" ]; then
	ccol=`convert xc: -format "%[fx:round($ww/2)]" info:`
fi

# compute right half size
wr=$((ww-ccol))

# test which side is smaller and compute 2*width-1
if [ $ccol -le $wr ]; then
	wd=$((2*ccol-1))
else
	wd=$((2*wr-1))
fi

# process image
convert -respect-parenthesis $tmpA1 +repage \
\( -clone 0 -crop ${ccol}x+0+0 +repage \( +clone -flop -gravity east -chop 1x0 \) \
	+append +write "${outname}_left.$suffix" \) \
\( -clone 0 -crop ${wr}x+${ccol}+0 +repage \( +clone -flop -gravity west -chop 1x0 \) \
	-reverse +append +write "${outname}_right.$suffix" \) \
-delete 0 -gravity center -crop ${wd}x+0+0 +repage \
-define compose:args=50,50 -compose blend -composite \
"${outname}_blend.$suffix"


exit 0
