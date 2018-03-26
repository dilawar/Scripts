#!/bin/bash
#
# Developed by Fred Weinhaus 2/18/2008 .......... revised 10/12/2014
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
# USAGE: aspect widthxheight [-m mode] [-c color] [-g gravity] [-f filter] infile outfile
# USAGE: aspect [-h or -help]
#
# OPTIONS:
#
# widthxheight           desired size (width x height); value in pixels
# -m      mode           crop or pad; default is crop
# -c      color          background color if pad; any IM color specification 
#                        or none or trans for transparent; default is black
# -g      gravity        gravity location for cropped area; North, East, South 
#                        West or Center; default is Center
# -f      filter         resize filter; any valid IM filter; default is Lanczos
#
###
#
# NAME: ASPECT
# 
# PURPOSE: To resize an image to a specific size allowing either cropping or padding 
# to deal with the aspect ratio change.
# 
# DESCRIPTION: ASPECT resizes an image to a specific size allowing cropping 
# or padding with a constant background color or transparent background. In either 
# case, the aspect ratio of the image will be compared to the aspect ratio of the 
# desired size and cropped or padded to fill one or the other dimension accordingly. 
# 
# 
# OPTIONS: 
# 
#
# widthxheight ... WIDTHxHEIGHT specifies the desired output size. WIDTH and HEIGHT 
# values must be specified as integer pixels.
#
# -m mode ... MODE specifies to either crop or pad the image when resizing it to 
# the desired size. The default is crop.
#
# -c color ... COLOR specifies the background color to use when mode=pad. Any 
# valid IM color specification may be used. To make the background transparent, 
# set the color either to none or trans. The default is black.
# 
# -g gravity ... GRAVITY is the region of the image to use when mode=crop. 
# Values may be North, East, South, West or Center. The default is Center.
#
# -f filter ... FILTER is any valid IM resize filter. The default is Lanczos.
#
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
mode="pad"			# mode: pad or crop
bgcolor="black"		# color or none for transparent
gravity="center"	# gravity location
filter="lanczos"	# resize filter (default=lanczos)


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
	 [0-9]*x[0-9]*)    # get size
					   # test size values
					   size="$1"
					   [ "$size" = "" ] && errMsg "SIZE=$size IS NOT A NUMBER"
					   ;;
		 		-m)    # mode
					   shift  # to get the next parameter - mode
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   # test mode values
					   mode="$1"
					   [ "$mode" != "pad" -a "$mode" != "crop" ] && errMsg "--- MODE=$mode IS NOT A VALID VALUE ---"
					   ;;
		 		-c)    # region
					   shift  # to get the next parameter - color
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
					   ;;
		 		-g)    # gravity
					   shift  # to get the next parameter - gravity
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GRAVITY SPECIFICATION ---"
					   checkMinus "$1"
					   # test gravity values
					   gravity="$1"
					   [ "$gravity" != "North" -a "$gravity" != "north" -a "$gravity" != "East" -a "$gravity" != "east" -a "$gravity" != "South" -a "$gravity" != "south" -a "$gravity" != "West" -a "$gravity" != "west" -a "$gravity" != "Center" -a "$gravity" != "center" ] && errMsg "--- GRAVITY=$gravity IS NOT A VALID VALUE ---"
					   ;;
				-f)    # get filter
					   shift  # to get the next parameter - filter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FILTER SPECIFICATION ---"
					   checkMinus "$1"
					   filter="$1"
					   ;;
 				 -)    # STDIN, end of arguments
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

# setup temporary images and auto delete upon exit
# use mpc/cache to hold input image temporarily in memory
tmpA="$dir/aspect_$$.mpc"
tmpB="$dir/aspect_$$.cache"
trap "rm -f $tmpA $tmpB;" 0
trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB; exit 1" ERR

# test if infile exists and compute dimensions
if convert -quiet "$infile" +repage "$tmpA"
	then
	: 'do nothing'
else
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

dwidth0=`echo "$size" | cut -dx -f1`
dheight0=`echo "$size" | cut -dx -f2`

[ "$dwidth0" = "" ] && errMsg "--- NO WIDTH SPECIFIED ---"
[ "$dheight0" = "" ] && errMsg "--- NO HEIGHT SPECIFIED ---"

aspect=`convert $tmpA -format "%[fx:w/h]" info:`
test0=`convert xc: -format "%[fx:$aspect>=1?1:0]" info:`
# if portrait mode, swap dwidth and dheight
if [ $test0 -eq 0 ]; then
	aspect=`convert xc: -format "%[fx:1/$aspect]" info:`
	dwidth=$dheight0
	dheight=$dwidth0
else
	dwidth=$dwidth0
	dheight=$dheight0
fi
test1=`convert xc: -format "%[fx:$aspect>(${dwidth}/${dheight})?1:0]" info:`
test2=`convert xc: -format "%[fx:$aspect>=1?1:0]" info:`
#echo "dwidth=$dwidth; dheight=$dheight; aspect=$aspect; test0=$test0; test1=$test1; test2=$test2;"

if [ "$mode" = "crop" ]; then

	if [ $test1 -eq 1 ]; then
		# "aspect larger than desired aspect (wider than tall)"
		resize="${dheight}x${dheight}^"
	elif [ $test2 -eq 1 ]; then
		# "aspect less than desired aspect (wider than tall) but greater than or equal to 1"
		resize="${dwidth}x${dwidth}"
	else
		# "aspect less than 1 (taller than wide)"
		resize="${dwidth}x${dwidth}^"
	fi
	convert $tmpA -filter $filter -resize $resize \
		-gravity $gravity -crop ${dwidth0}x${dheight0}+0+0 +repage "$outfile"

elif [ "$mode" = "pad" ]; then

	if [ $test1 -eq 1 ]; then
		# "aspect larger than desired aspect (wider than tall)"
		resize="${dwidth}x${dwidth}"
	elif [ $test2 -eq 1 ]; then
		# "aspect less than desired aspect (wider than tall) but greater than or equal to 1"
		resize="${dheight}x${dheight}^"
	else
		# "aspect less than 1 (taller than wide)"
		resize="${dheight}x${dheight}"
	fi
	convert \( -size ${dwidth0}x${dheight0} xc:$bgcolor \) \
		\( $tmpA -filter $filter -resize $resize \) \
		-gravity $gravity -composite +repage "$outfile"

else
	errMsg "--- UNKNOWN METHOD ---"
fi

exit 0