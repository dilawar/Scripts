#!/bin/bash
#
# Developed by Fred Weinhaus 6/22/2013 .......... revised 4/29/2015
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
# USAGE: draganeffect [-b brightness] [-c contrast] [-d darkness ] 
# [-s saturation] infile outfile
# USAGE: draganeffect [-h or -help]
# 
# OPTIONS:
# 
# -b     brightness     brightness factor; float>=0; default=1 no change
# -c     contrast       contrast; float; nominally -10 to 10; default=0 
# -d     darkness       shadow darkness; float>=1; default=1
# -s     saturation     saturation; integer>=0; 100 (no change); default=150
# 
###
# 
# NAME: DRAGANEFFECT 
# 
# PURPOSE: To apply a Dragan-like effect to an image.
# 
# DESCRIPTION: DRAGANEFFECT applies a Dragan-like effect to an image to enhance 
# wrinkles creating a "gritty" effect.
# 
# 
# ARGUMENTS: 
# 
# -b brightness ... BRIGHTNESS is a brightness factor. Values are floats>=0. 
# The default=1 or no change. Increase brightness is larger than 1.
# Decrease brightness is less than 1.
# 
# -c contrast ... CONTRAST is a sigmoidal contrast. Values are floats nominally 
# in the range of -10 to 10. Positive values increase contrast and negative 
# values decrease contrast. The default=0 (no change).
# 
# -d darkness ... DARKNESS is the shadow darkening factor. Values are 
# floats>=1. The default=1 or no change. Darker shadows is larger than 1.
# 
# -s saturation ... SATURATION. Values are integer>=0. A value of 100 is no 
# change. The default=150.
# 
# References:
# http://andrzejdragan.com/
# http://www.dmimaging.net/photography-trends-dragan-effect/ 
# http://photoshopfrenzy.com/?p=94
#
# REQUIREMENTS: IM 6.6.1.9 due to the use of -morphology DoG
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
brightness=1		# float>=0; 1=no change; >1 is brighter; <1 is darker
contrast=0			# nominally -10 to 10; 0 is no change
darkness=1			# 1 is nominal; float>=1; darkens shadows
saturation=150		# 100 is no change
radius=5			# fixed value; does not change result significantly

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
		  -h|-help)    # help information
					   echo ""
					   usage2
					   exit 0
					   ;;
				-b)    # get  brightness
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BRIGHTNESS SPECIFICATION ---"
					   checkMinus "$1"
					   brightness=`expr "$1" : '\([.0-9]*\)'`
					   [ "$brightness" = "" ] && errMsg "--- BRIGHTNESS=$brightness MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   ;;
				-c)    # get  contrast
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID CONTRAST SPECIFICATION ---"
					   #checkMinus "$1"
					   contrast=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$contrast" = "" ] && errMsg "--- CONTRAST=$contrast MUST BE A FLOAT VALUE ---"
					   ;;
				-d)    # get  darkness
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DARKNESS SPECIFICATION ---"
					   checkMinus "$1"
					   darkness=`expr "$1" : '\([.0-9]*\)'`
					   [ "$darkness" = "" ] && errMsg "--- DARKNESS=$darkness MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   test1=`echo "$darkness < 1" | bc`
					   [ $test1 -eq 1 ] && errMsg "--- DARKNESS=$darkness MUST BE A FLOAT GREATER THAN OR EQUAL TO 1 ---"
					   ;;
				-s)    # get saturation
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SATURATION SPECIFICATION ---"
					   checkMinus "$1"
					   saturation=`expr "$1" : '\([0-9]*\)'`
					   [ "$saturation" = "" ] && errMsg "--- SATURATION=$saturation MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
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
tmpA1="$dir/draganeffect_1_$$.mpc"
tmpB1="$dir/draganeffect_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1;" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1; exit 1" ERR

# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- 1 FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"


# get im version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# set up darkness
darkness=`convert xc: -format "%[fx:3/$darkness]" info:`

# set up brightness
if [ "$brightness" = "1" ]; then 
	brightening=""
else
	brightening="-evaluate multiply $brightness"
fi

# set up contrast
if [ "$contrast" = "0" ]; then 
	contrasting=""
else
	test1=`convert xc: -format "%[fx:sign($contrast)]" info:`
	abscontrast=`convert xc: -format "%[fx:abs($contrast)]" info:`
	if [ $test1 -eq 1 ]; then
		contrasting="-sigmoidal-contrast ${abscontrast}x50%"
	else
		contrasting="+sigmoidal-contrast ${abscontrast}x50%"
	fi
fi

# set up darkness
if [ "$darkness" = "1" ]; then 
	darkening=""
else
	# -clamp needed for HDRI
	darkening="-evaluate multiply $darkness -clamp"
fi

# set up for biasing
if [ "$im_version" -ge "07000000" ]; then
	biasing="-define convolve:bias=50%"
else
	biasing="-bias 50%"
fi


# process image
# first line: read image, enhance brightness, sigmoidal-contrast and saturation 
# second line: clone, desaturate, darken and compose multiply with input
# third line: clone previous result and apply high pass filter using DoG
# fourth line: clone both use overlay composite to apply high pass filter to enhanced input
# fifth line: clone enhanced input and desaturate, delete temps
# sixth line: use hardlight composite to apply desaturated image to enhanced image and save output
convert $tmpA1 $brightening $contrasting -modulate 100,$saturation,100 \
	\( -clone 0 -modulate 100,0,100 $darkening \) -compose multiply -composite -clamp \
	\( -clone 0 $biasing -define convolve:scale=1 -morphology Convolve DoG:0,0,$radius -clamp \) \
	\( -clone 0 -clone 1 -compose overlay -composite \) \
	\( -clone 0 -modulate 100,0,100 \) -delete 0,1 \
	-compose hardlight -composite "$outfile"


exit 0

