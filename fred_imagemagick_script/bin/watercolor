#!/bin/bash
#
# Developed by Fred Weinhaus 6/28/2017 .......... revised 6/28/2017
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
# USAGE: watercolor [-s smoothing] [-e edge] [-m mixing] [-c contrast] infile outfile
# USAGE: watercolor [-h|-help]
#
# OPTIONS:
#
# -s     smoothing     smoothing of image texture; integer>=0; default=0 (no smoothing)
# -e     edge          edge gain; float>=1; default=5   
# -m     mixing        mixing of edge content with smoothed image; 0<=integer<=100; 
#                      larger is more edge content; default=33
# -c     contrast      contrast increase factor; float>=0; default=0 (no change)
#
###
#
# NAME: WATERCOLOR
# 
# PURPOSE: To apply a watercolor effect to an image.
# 
# DESCRIPTION: WATERCOLOR applies a watercolor effect to an image.
# 
# OPTIONS: 
# 
# -s smoothing ... SMOOTHING of image texture. Values are integers>=0. The default=0 
# (no smoothing). IMPORTANT: use odd integers to avoid image shift by half pixel.
# 
# -e edge ... EDGE gain. Values are floats>=1. The default=5.
#  
# -m mixing ... MIXINT of edge content with smoothed image. Values are 0<=integer<=100. 
# Larger is more edge content. The default=33.
# 
# -c contrast ... CONTRAST increase factor. Values are floats>=0. The  default=0 
# (no change).
# 
# Reference: 
# http://www.photoshopessentials.com/photo-effects/watercolor-painting-photoshop-cs6/
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values

smoothing=0			# smoothing of image texture; integer>=0
edge=5				# edge gain; float>0; default=5
mixing=33			# mixing of image and luminized edge image; 0<=integer<=100
contrast=0			# contrast adjustment; float>=0

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
elif [ $# -gt 10 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
	while [ $# -gt 0 ]
		do
			# get parameter values
			case "$1" in
		     -help)    # help information
					   echo ""
					   usage2
					   exit 0
					   ;;
				-s)    # get smoothing
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SMOOTHING SPECIFICATION ---"
					   checkMinus "$1"
					   smoothing=`expr "$1" : '\([0-9]*\)'`
					   [ "$smoothing" = "" ] && errMsg "--- SMOOTHING=$smoothing MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-e)    # get edge
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID EDGE SPECIFICATION ---"
					   checkMinus "$1"
					   edge=`expr "$1" : '\([.0-9]*\)'`
					   [ "$edge" = "" ] && errMsg "--- EDGE=$edge MUST BE A NON-NEGATIVE FLOAT ---"
					   test1=`echo "$edge < 1" | bc`
					   [ $test1 -eq 1 ] && errMsg "--- EDGE=$edge MUST BE GREATER THAN OR EQUAL TO 1 ---"
					   ;;
				-m)    # get mixing
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MIXING SPECIFICATION ---"
					   #checkMinus "$1"
					   mixing=`expr "$1" : '\([0-9]*\)'`
					   [ "$mixing" = "" ] && errMsg "--- MIXING=$mixing MUST BE A NON-NEGATIVE INTEGER ---"
					   test=`echo "$mixing > 100" | bc`
					   [ $test -eq 1 ] && errMsg "--- MIXING=$mixing MUST BE AN INTEGER BETWEEN -0 AND 100 ---"
					   ;;
				-c)    # get contrast
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID CONTRAST SPECIFICATION ---"
					   checkMinus "$1"
					   contrast=`expr "$1" : '\([.0-9]*\)'`
					   [ "$contrast" = "" ] && errMsg "--- CONTRAST=$contrast MUST BE A NON-NEGATIVE FLOAT ---"
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


# setup temporary images
tmpA1="$dir/watercolor_1_$$.mpc"
tmpB1="$dir/watercolor_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1; exit 0" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15

# read the input image into the temporary cached image and test if valid
convert -quiet -regard-warnings "$infile" +repage $tmpA1 ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

if [ $smoothing -ne 0 ]; then
	sproc="-mean-shift ${smoothing}x${smoothing}+10%"
else
	sproc=""
fi

if [ "$contrast" != 0 ]; then
	cproc="-sigmoidal-contrast ${contrast}x50%"
else
	cproc=""
fi

convert $tmpA1 $cproc $sproc \
	\( -clone 0 -define convolve:scale='!' \
	-define morphology:compose=Lighten \
	-morphology Convolve  'Sobel:>' \
	-negate -evaluate pow $edge \) \
	\( -clone 0 -clone 1 -compose luminize -composite \) \
	-delete 1 -define compose:args=$mixing -compose blend -composite \
	"$outfile"


exit 0






