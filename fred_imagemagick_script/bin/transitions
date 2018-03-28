#!/bin/bash
#
# Developed by Fred Weinhaus 5/29/2008 .......... revised 5/3/2015
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
# USAGE: transitions [-m mode] [-f frames] [-d delay] [-p pause] [-r] [-e] infile1 infile2 maskfile [outfile]
# USAGE: transitions [-h or -help]
#
# OPTIONS:
#
# -m      mode           mode of transition; wipe or dissolve; default=wipe
# -f      frames         number of frames in animation; frames>1; default=20
# -d      delay          delay between frames; delay>0; default=20
# -p      pause          pause delay for two undistorted input images;
#                        pause>0; default=50
# -r                     reverse the animation sequence and append it to the end
# -e                     exponentiate maskfile to make the animation 
#                        start more gradually. Mostly useful for mode=dissolve
# 
# The two input files and mask file must be the same size.
# 
# The output file must be of type that supports multi-frames, such as gif.
# 
# If no output file is specified, the animation will be displayed automatically 
# but not saved to a file
#
###
#
# NAME: TRANSITIONS 
# 
# PURPOSE: To apply an animated transition between two images.
# 
# DESCRIPTION: TRANSITIONS applies an animated transition between two images
# using a mask image to control the transition. The first image will show 
# where the mask is black and the second image will show where the mask is 
# white. The mask will be made gradually more an more white as each frame 
# is composited. NOTE that this is not a true warping morph. It is simply 
# an animated masked composite.
# 
# OPTIONS: 
# 
# -m mode ... MODE of transition. Values are wipe or dissolve. The default is wipe.
# Note that dissolve is generally only useful for very gradual graylevel changes 
# in the mask image, such as a linear or radial gradient.
#
# -f frames ... FRAMES is the total number of frames in the animation (including 
# infile1 and infile2 as the start and end frames. Values are integers > 1. The 
# default is 20.
#
# -d delay ... DELAY between frames. Values are integers>0. The default=20
#
# -p pause ... PAUSE is the delay to use for the first and last frame of the 
# animation, i.e. the delay for each of the input images. The default=50
# 
# -r ... If supplied, then reverse the animation sequence, remove the first and 
# last frames of the reversed sequence and append these reversed frames to  
# the end of the animation.
# 
# -e ... If supplied, then the maskfile will be exponentiated to make the 
# animation start more gradually. This is useful for most dissolve mode 
# transitions.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
mode="wipe"			#wipe or dissolve
frames=20
delay=20
pause=50
mask="linear"		#linear or exponential
reverse="no"
view="no"

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
elif [ $# -gt 14 ]
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
				-m)    # get mode
					   shift  # to get the next parameter - mode
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode="$1"
					   [ "$mode" != "wipe" -a "$mode" != "dissolve" ] && errMsg "MODE=$mode MUST BE EITHER WIPE OR DISSOLVE ---"
					   ;;
				-f)    # get frames
					   shift  # to get the next parameter - frames
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FRAMES SPECIFICATION ---"
					   checkMinus "$1"
					   frames=`expr "$1" : '\([0-9]*\)'`
					   [ "$frames" = "" ] && errMsg "FRAMES=$frames MUST BE AN INTEGER"
		   			   framestest=`echo "$frames <= 1" | bc`
					   [ $framestest -eq 1 ] && errMsg "--- FRAMES=$frames MUST BE AN INTEGER GREATER THAN 1 ---"
					   ;;
				-d)    # get delay
					   shift  # to get the next parameter - delay
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DELAY SPECIFICATION ---"
					   checkMinus "$1"
					   delay=`expr "$1" : '\([0-9]*\)'`
					   [ "$delay" = "" ] && errMsg "DELAY=$delay MUST BE AN INTEGER"
		   			   delaytest=`echo "$delay < 1" | bc`
					   [ $delaytest -eq 1 ] && errMsg "--- DELAY=$delay MUST BE A POSITIVE INTEGER ---"
					   ;;
				-p)    # get pause
					   shift  # to get the next parameter - pause
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID PAUSE SPECIFICATION ---"
					   checkMinus "$1"
					   pause=`expr "$1" : '\([0-9]*\)'`
					   [ "$pause" = "" ] && errMsg "PAUSE=$pause MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-r)    # set frame reversal append
					   reverse="yes"
					   ;;
				-e)    # set exponentiation
					   mask="exponential"
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
	infile1="$1"
	infile2="$2"
	maskfile="$3"
	outfile="$4"
fi

# test that infile1 provided
[ "$infile1" = "" ] && errMsg "NO INPUT FILE 1 SPECIFIED"

# test that infile2 provided
[ "$infile2" = "" ] && errMsg "NO INPUT FILE 2 SPECIFIED"

# test that maskfile provided
[ "$maskfile" = "" ] && errMsg "NO MASKFILE SPECIFIED"


# set temporary files
tmpA="$dir/transitions_1_$$.mpc"
tmpB="$dir/transitions_1_$$.cache"
tmpC="$dir/transitions_2_$$.mpc"
tmpD="$dir/transitions_2_$$.cache"
tmpE="$dir/transitions_3_$$.mpc"
tmpF="$dir/transitions_3_$$.cache"
tmp0="$dir/transitions_0_$$.gif"
trap "rm -f $tmpA $tmpB $tmpC $tmpD $tmpE $tmpF $tmp0;" 0
trap "rm -f $tmpA $tmpB $tmpC $tmpD $tmpE $tmpF $tmp0; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB $tmpC $tmpD $tmpE $tmpF $tmp0; exit 1" ERR

# get im_version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# colorspace RGB and sRGB swapped between 6.7.5.5 and 6.7.6.7 
# though probably not resolved until the latter
# then -colorspace gray changed to linear between 6.7.6.7 and 6.7.8.2 
# then -separate converted to linear gray channels between 6.7.6.7 and 6.7.8.2,
# though probably not resolved until the latter
# so -colorspace HSL/HSB -separate and -colorspace gray became linear
# but we need to use -set colorspace RGB before using them at appropriate times
# so that results stay as in original script
# The following was determined from various version tests using transitions
# with IM 6.7.4.10, 6.7.6.10, 6.7.9.0
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
fi



if convert -quiet "$infile1" +repage "$tmpA"
	then
	: ' Do Nothing '
else
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

if convert -quiet "$infile2" +repage "$tmpC"
	then
	: ' Do Nothing '
else
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

if [ "$mask" = "exponential" ]
	then
	# exponentiate mask so that transition does not show much of second image too early
	if convert -quiet "$maskfile" -fx "(10^(u)-1)/9" +repage "$tmpE"
		then
		: ' Do Nothing '
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
	fi
elif [ "$mask" = "linear" ]
	then
	if convert -quiet "$maskfile" +repage "$tmpE"
		then
		: ' Do Nothing '
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
	fi
fi

# check image sizes
w1=`convert $tmpA -format "%w" info:`
h1=`convert $tmpA -format "%h" info:`
w2=`convert $tmpC -format "%w" info:`
h2=`convert $tmpC -format "%h" info:`
w3=`convert $tmpE -format "%w" info:`
h3=`convert $tmpE -format "%h" info:`
[ $w1 -ne $w2 -a $h1 -ne $h2 -a $w1 -ne $w3 -a $h1 -ne $h3 ] && errMsg "--- IMAGE SIZES DO NOT MATCH ---"

# test if hdri and if so limit addition to 100% (white)
is_hdri=`convert -list configure | \
	sed -n 's/^.*--enable-hdri.*$/1/p'`
if [ "$is_hdri" != "" ]
	then
	min="-evaluate min 100%"
else
	min=""
fi
echo "min=$min"

echo ""
echo "Generating $frames Frames:"
echo ""

frames=`expr $frames - 1`
echo "0"
i=1
convert $tmpA $tmp0
if [ "$mode" = "dissolve" ]
	then
	while [ $i -le $frames ]
		do
		echo $i
		aa=`convert xc: -format "%[fx:100*$i/$frames]" info:`
		convert $tmpA $tmpC \( $tmpE -evaluate add $aa% $min \) $setcspace -composite miff:- |\
			convert -delay $delay $tmp0 -page +0+0 - -page +0+0 $tmp0
		i=`expr $i + 1`
	done
elif [ "$mode" = "wipe" ]
	then
	while [ $i -le $frames ]
		do
		echo $i
		tt=`convert xc: -format "%[fx:100($frames-$i)/$frames]" info:`
		convert $tmpA $tmpC \( $tmpE -threshold $tt% \) $setcspace -composite miff:- |\
			convert -delay $delay $tmp0 -page +0+0 - -page +0+0 $tmp0
		i=`expr $i + 1`
	done
fi

# change delay of first and last image
if [ $pause -ne $delay ]
	then
	convert $tmp0 -coalesce \
		\( -clone 0 -set delay $pause \) -swap 0,-1 +delete \
		\( +clone  -set delay $pause \) -swap -1,-2 +delete \
		-quiet -layers Optimize $tmp0
fi

if [ "$reverse" = "yes" ]
	then
	echo ""
	echo "Reversing Animation - Please Wait"
	convert $tmp0 -coalesce \( -clone -2-1 \) \
		-quiet -layers Optimize $tmp0
fi

if [ "$outfile" != "" ]
	then
	convert $tmp0 -loop 0 "$outfile"
else
	animate $tmp0
fi
exit 0