#!/bin/bash
# 
# Developed by Fred Weinhaus 8/12/2017 .......... revised 8/12/2017
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
# USAGE: tinyplanet [-d dimension] [-b bgcolor] [-r rotate] [-f fade] [-s smooth] 
# [-t threshold] [-n newseed] [-I increment] [-D delay] [-L loop] infile outfile
# USAGE: tinyplanet [-h or -help]
#
# OPTIONS:
# 
# -d     dimension      square dimension of output; integer>0; default is the height 
#                       of the input
# -b     bgcolor        bgcolor is the color for the area outside the 
#                       circle of the fisheye area; any valid IM color is permitted or 
#                       the special name of "stars" to create a black background with 
#                       random point star field; use none to make the background 
#                       transparent; default=white
# -r     rotate         (initial) rotation angle for the output relative to the top of 
#                       input being north on the output; floats>=0; default=0
# -f     fade           amount of fade in pixels between the image and background color 
#                       at the top of the image for viewtype=planet; integer>=0; 
#                       default=0
# -s     smooth         amount of smoothing for antialiasing when bgcolor=none; 
#                       float>=0; default=0
# -t     threshold      threshold value for controlling the number (density) of random 
#                       stars; float>=0; larger values produce less stars; default=99
# -n     newseed        random seed for creating the star field when bgcolor=stars
# -I     increment      angular increment in degrees for generating a rotating animation;
#                       float>0; to avoid irregular rotation, use a value that divides 
#                       evenly into 360; default is no animation
# -D     delay          delay between frames of the animation; integer>=0; default=20
# -L     loop           number of loops of the animation; integer>=0; default=0 means 
#                       unlimited looping
# 
###
# 
# NAME: TINYPLANET 
# 
# PURPOSE: To transform a spherical panorama to a linear fisheye view.
# 
# DESCRIPTION: TINYPLANET is designed to transform a spherical (equirectangular) panorama 
# to a linear fisheye view. The spherical (equirectangular) panorama's pixels represent 
# x=longitude and y=latitude. That is, both x and y are angular units. Longitude 
# corresponds to 360 degree range. Thus the left and right sides must wrap contiguously.
# This means there should be a width to height aspect of 2:1. An normal image can be 
# faked to simulate a spherical panorama by cropping to w/h aspect  of 2:1 (width is 
# twice the height), then mirroring it horizontally, appending horizontally, then 
# cropping the center half and rolling it horizontally 50%. The script also allows the 
# output to have any rotation and can be animated in rotation. For animations, be sure 
# to use an output format that supports animation, such as gif.
# 
# 
# ARGUMENTS: 
# 
# -d dimension ... DIMENSION is the square dimension of output image. Values are 
#  integers>0. The default is the height of the input.
# 
# -b bgcolor ... BGCOLOR is the color for the area outside the circle of the fisheye 
# region. Any valid IM color is permitted or the special name of "stars" to create 
# a black background with random point star field. Use "none" to make the background 
# transparent. The default=white.
# 
# # -r rotate ... ROTATE is the (initial) rotation angle for the output relative to 
# the top of input being north on the output. Values are floats>=0. The default=0.
#
# -f fade ... FADE is the amount of fade in pixels between the top of the input image 
# and background color for viewtype=planet. This is a preprocessing step so that 
# the outside of the fisheye circular region will fade to the background color. 
# Values are integers>=0. The default=0.
# 
# -s smooth ... SMOOTH is the amount of smoothing for antialiasing when bgcolor=none or 
# stars. Values are float>=0. The default=0.
#
# -t threshold ... THRESHOLD value for controlling the number (density) of random 
# stars. Values are floats>=0. Larger values produce less stars. The default=99. 
# Nominal values are 99 to about 99.9.
# 
# -n newseed .. .NEWSEED is the random seed for creating the star field when 
# bgcolor=stars. Values are integers>=0. The default is totally random star placement 
# from run-to-run.
# 
# -I increment ... INCREMENT is the angular increment in degrees for generating a 
# rotating animation. Values are floats>0. To avoid irregular rotation, use a value 
# that divides evenly into 360 (e.g. 2,5,10,15,20,30,45). The default is no animation.
# 
# -D delay ... DELAY between frames of the animation. Values are integers>=0. The 
# default=20. It is not recommended to use 0.
# 
# -L loop ... LOOP is the number of loops of the animation. Values are integers>=0. 
# The default=0 means unlimited looping
#
# NOTE: For animations, be sure to use an output format that supports animation, such 
# as gif.
# 
# REFERENCES:
# http://www.photographymad.com/pages/view/little-planet-photos-5-simple-steps-to-making-panorama-worlds
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
dimension=""			# output square dimension; default=height of input
bgcolor="white"			# background color
rotate=0				# (initial) rotation angle in deg
fade=0					# fade height (fade at top to bgcolor); integer>=0
smooth=0				# smoothing for antialiasing if bgcolor=none or stars; float>=0
threshold=99			# threshold for stars; smaller shows more starts; nominal about 99 to 99.9
newseed=""				# seed for random stars; integer>=0
increment=""			# animation angular increment (divisible into 360)
delay=20				# animation delay
loop=0					# animation number of loops
 

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
elif [ $# -gt 22 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else

increment=""			# animation angular increment (divisible into 360)
delay=20				# animation delay
loop=0					# animation number of loops

	while [ $# -gt 0 ]
		do
			# get parameter values
			case "$1" in
		  -h|-help)    # help information
					   echo ""
					   usage2
					   exit 0
					   ;;
				-d)    # get dimension
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DIMENSION SPECIFICATION ---"
					   checkMinus "$1"
					   dimension=`expr "$1" : '\([0-9]*\)'`
					   [ "$dimension" = "" ] && errMsg "--- DIMENSION=$dimension MUST BE A NON-NEGATIVE INTEGER ---"
					   test=`echo "$dimension == 0" | bc`
					   [ $test -eq 1 ] && errMsg "--- DIMENSION=$dimension MUST BE AN INTEGER GREATER THAN 0 ---"
					   ;;
				-b)    # get  bgcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
					   ;;
				-r)    # get rotate
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID ROTATE SPECIFICATION ---"
					   checkMinus "$1"
					   rotate=`expr "$1" : '\([.0-9]*\)'`
					   [ "$rotate" = "" ] && errMsg "--- ROTATE=$rotate MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-f)    # get fade
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FADE SPECIFICATION ---"
					   checkMinus "$1"
					   fade=`expr "$1" : '\([0-9]*\)'`
					   [ "$fade" = "" ] && errMsg "--- FADE=$fade MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-s)    # get smooth
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SMOOTH SPECIFICATION ---"
					   checkMinus "$1"
					   smooth=`expr "$1" : '\([.0-9]*\)'`
					   [ "$smooth" = "" ] && errMsg "--- SMOOTH=$smooth MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-t)    # get threshold
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID THRESHOLD SPECIFICATION ---"
					   checkMinus "$1"
					   threshold=`expr "$1" : '\([.0-9]*\)'`
					   [ "$threshold" = "" ] && errMsg "--- THRESHOLD=$threshold MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-n)    # get newseed
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NEWSEED SPECIFICATION ---"
					   checkMinus "$1"
					   newseed=`expr "$1" : '\([0-9]*\)'`
					   [ "$newseed" = "" ] && errMsg "--- NEWSEED=$newseed MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-I)    # get increment
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID INCREMENT SPECIFICATION ---"
					   checkMinus "$1"
					   increment=`expr "$1" : '\([.0-9]*\)'`
					   [ "$increment" = "" ] && errMsg "--- INCREMENT=$increment MUST BE A NON-NEGATIVE FLOAT ---"
					   test=`echo "$increment == 0" | bc`
					   [ $test -eq 1 ] && errMsg "--- INCREMENT=$increment MUST BE A FLOAT GREATER THAN 0 ---"
					   ;;
				-D)    # get delay
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DELAY SPECIFICATION ---"
					   checkMinus "$1"
					   delay=`expr "$1" : '\([0-9]*\)'`
					   [ "$delay" = "" ] && errMsg "--- DELAY=$delay MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-L)    # get loop
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID LOOP SPECIFICATION ---"
					   checkMinus "$1"
					   loop=`expr "$1" : '\([0-9]*\)'`
					   [ "$loop" = "" ] && errMsg "--- LOOP=$loop MUST BE A NON-NEGATIVE INTEGER ---"
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
tmpA1="$dir/tinyplanet_1_$$.mpc"
tmpB1="$dir/tinyplanet_1_$$.cache"
tmpA2="$dir/tinyplanet_2_$$.mpc"
tmpB2="$dir/tinyplanet_2_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 0" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" 1 2 3 15

# test that input exists and is readable
if [ ! -f "$infile" -a ! -r "$infile" -a ! -s "$infile" ]; then
	errMsg "--- INFILE $infile DOES NOT EXIST OR IS NOT READABLE OR HAS ZERO SIZE ---"
fi

# get input size
declare `convert -ping "$infile" -format "ww=%w\nhh=%h\n" info:`

# set up for resizing and output size
if [ "$dimension" != "" ]; then
	wd=$dimension
	ht=$dimension
	percent=`convert xc: -format "%[fx:100*$dimension/$hh]" info:`
	resizing="-resize $percent%"
else
	wd=$hh
	ht=$hh
	resizing=""
fi

# setup for bgcolor
if [ "$bgcolor" = "stars" ]; then
	bcolor="none"
else
	bcolor=$bgcolor
fi

if [ "$rotate" != "0" ]; then
	rollval=`convert xc: -format "%[fx:round($ww*$rotate/360)]" info:`
	rolling="-roll +${rollval}+0"
else
	rolling=""
fi

# set up for seeding
if [ "$newseed" != "" ]; then
	seeding="-seed $newseed"
else
	seeding=""
fi

# create stars image
if [ "$bgcolor" = "stars" ]; then
	convert -size ${wd}x${ht} xc:black $seeding +noise random -channel g -separate +channel -threshold $threshold% $tmpA2
fi

# read the input image into the temporary cached image and test if valid
if [ "$fade" = "0" ]; then
	convert -quiet "$infile" +repage $rolling $resizing $tmpA1 ||
		echo "--- 1 FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"
else
	convert -quiet "$infile" +repage \
		\( -size ${ww}x${fade} gradient:"${bcolor}-none" \) \
		-gravity north -compose over -composite $rolling $resizing $tmpA1 ||
		echo "--- 1 FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"
fi

# process image
if [ "$increment" = "" ]; then
	if [ "$bgcolor" = "stars" ]; then
		convert $tmpA1 -rotate 180  \
			-virtual-pixel HorizontalTile -background $bcolor \
			+distort Polar 0 +repage -rotate 180 \
			$tmpA2 +swap -compose over -composite +monitor \
			"$outfile"
	else
		convert $tmpA1 -rotate 180  \
			-virtual-pixel HorizontalTile -background $bcolor \
			+distort Polar 0 +repage -rotate 180 \
			"$outfile"		
	fi
	
else
	# get new width and roll increment
	ww=`convert -ping $tmpA1 -format "%w" info:`
	rollinc=`convert xc: -format "%[fx:round($ww*$increment/360)]" info:`
	numframes=`convert xc: -format "%[fx:floor(360/$increment)]" info:`
	echo "ww=$ww; rollinc=$rollinc; numframes=$numframes;"

	# use subshell processing to collect rotated frames into miff: format
	(
	for ((i=0; i<numframes; i++)); do
		rollval=`convert xc: -format "%[fx:$i*$rollinc]" info:`	
		echo >&2 "i=$i; roll=$rollval"
		if [ "$bgcolor" = "stars" ]; then
			convert $tmpA1 -roll +${rollval}+0 -rotate 180  \
				-virtual-pixel HorizontalTile -background $bcolor \
				+distort Polar 0 +repage -rotate 180 \
				$tmpA2 +swap -compose over -composite +monitor \
				miff:-
		else
			convert $tmpA1 -roll +${rollval}+0 -rotate 180  \
				-virtual-pixel HorizontalTile -background $bcolor \
				+distort Polar 0 +repage -rotate 180 \
				miff:-	
		fi
	done
	) | convert -delay $delay - -loop $loop "$outfile"

fi 

exit 0






