#!/bin/bash
#
# Developed by Fred Weinhaus 11/12/2012 .......... revised 4/25/2015
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
# USAGE: edgefx [-s strength] [-c compose] [-m mix] [-N] infile outfile
# USAGE: edgefx [-h or -help]
#
# OPTIONS:
#
# -s     strength      edge strength (i.e. gain or intensity); float>=0; 
#                      default=5
# -c     compose       compose method to composite edge image with original;
#                      most mathematical, lighting or channel IM compose 
#                      methods allowed; default=over simple blending;
# -m     mix           mix percent between compose processed edge image and 
#                      the original; 0<=integer<=100; default=100 for full 
#                      edge composed image (0 for original image)
# -N                   Negate (reverses) edge polarity
#
###
#
# NAME: EDGEFX 
# 
# PURPOSE: To extract the edges in an image and optionally composes them with  
# the image to create interesting effects.
# 
# DESCRIPTION: EDGEFX extracts the edges in an image and optionally composes 
# them with the image to create interesting effects. The polarity of the 
# extracted edge may be reversed. The 8-directional sobel edge operator is 
# used to extract the edges.
# 
# OPTIONS: 
# 
# -s  strength ... STRENGTH is the edge strength (i.e., gain or intensity). 
# Values are floats>=0. The default=5.
# 
# -c compose ... COMPOSE is the compose method used to combine the edge image 
# with the original image. Most mathematical, lighting or channel IM compose 
# methods are allowed. The default=over, which is just simple blending
#
# -m mix ... MIX is the mix percent between compose processed edge image and 
# the original. Mix=0 results in the original image. Mix=100 is results in the 
# compose processed edge image. The default=100. When used with the default 
# compose method, this produces the simple edge extracted image.
# 
# -N ... NEGATE (reverses) edge polarity.
# 
# REQUIREMENTS: IM 6.6.2.2 due to the use of rotated kernels in -morphology 
# convolve.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
strength=5			# edge strength
compose="over"		# compose method
mix=100				# mixing percent of edge composed image with the original
neg="no"            # negate edges

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
elif [ $# -gt 9 ]
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
				-s)    # get strength
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID STRENGTH SPECIFICATION ---"
					   checkMinus "$1"
					   strength=`expr "$1" : '\(.[0-9]*\)'`
					   [ "$strength" = "" ] && errMsg "--- STRENGTH=$strength MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-c)    # get  compose
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COMPOSE SPECIFICATION ---"
					   checkMinus "$1"
					   compose=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$compose" in 
					   		over) ;;
					   		overlay) ;;
					   		multiply) ;;
					   		hard_light) ;;
					   		soft_light) ;;
					   		pegtop_light) ;;
					   		pin_light) ;;
					   		linear_light) ;;
					   		vivid_light) ;;
					   		linear_dodge) ;;
					   		linear_burn) ;;
					   		color_dodge) ;;
					   		color_burn) ;;
					   		difference) ;;
					   		exclusion) ;;
					   		lighten) ;;
					   		darken) ;;
					   		lightenintensity) ;;
					   		darkenintensity) ;;
					   		screen) ;;
					   		hue) ;;
					   		saturate) ;;
					   		luminize) ;;
					   		colorize) ;;
					   		*) errMsg "--- COMPOSE=$compose IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-m)    # get mix
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MIX SPECIFICATION ---"
					   checkMinus "$1"
					   mix=`expr "$1" : '\([0-9]*\)'`
					   [ "$mix" = "" ] && errMsg "--- MIX=$mix MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$mix < 0" | bc`
					   testB=`echo "$mix > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- MIX=$mix MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-N)    # get negate
					   neg="yes"
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
tmpdir="$dir"

dir="$tmpdir/SOFTFOCUS.$$"

mkdir "$dir" || errMsg "--- FAILED TO CREATE TEMPORARY FILE DIRECTORY ---"
trap "rm -rf $dir;" 0
trap "rm -rf $dir; exit 1" 1 2 3 15
trap "rm -rf $dir; exit 1" ERR

# read input image into temporary memory mapped (mpc) format image
convert -quiet "$infile" +repage $dir/tmpI.mpc ||
	echo  "--- FILE $thefile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"


# set up for negate edges
if [ "$neg" = "yes" ]; then
	neg="-negate"
else
	neg=""
fi

# process image
convert $dir/tmpI.mpc \
	\( -clone 0 -define convolve:scale='!' \
	-define morphology:compose=Lighten \
	-morphology Convolve  'Sobel:>' \
	-negate -evaluate pow $strength $neg \) \
	\( -clone 0 -clone 1 -compose $compose -composite \) \
	-delete 1 -define compose:args=$mix -compose blend -composite \
	"$outfile"


exit 0