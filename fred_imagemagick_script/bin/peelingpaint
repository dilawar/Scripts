#!/bin/bash
#
# Developed by Fred Weinhaus 8/5/2013 .......... revised 4/25/2015
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
# USAGE: peelingpaint [-m mode] [-s strength] [-g gain] infile texture outfile
# USAGE: peelingpaint [-h or -help]
#
# OPTIONS:
#
# -m     mode         mode for output; choices are: cracks (c) or scratches (s);
#                     default=cracks
# -s     strength     strength (darkness) of cracks; integer>=0; default=100
# -g     gain         gain applied by composite mask; 0<=integer<=100; 
#                     default=75
# 
###
#
# NAME: PEELINGPAINT 
# 
# PURPOSE: To apply a peeling paint effect to an image.
# 
# DESCRIPTION: PEELINGPAINT applies a peeling paint effect to an image by 
# compositing with a peeling paint texture image. There are two modes. The 
# first is all cracks. The second (scratches) is a mix of dark cracks and 
# light scratches.
# 
# OPTIONS: 
# 
# -m mode ... MODE for output. The choices are: cracks (c) or scratches (s).
# Mode=cracks will be dark cracks. Mode=scratches will be some very dark cracks 
# and bright scratches.  The default=cracks.
# 
# -s strength ... STRENGTH (darkness) of cracks. Values are integers>=0. The 
# default=100.  This argument brightens/darkens the texture image (using 
# -evaluate pow).
# 
# -g gain ... GAIN applied by mask. Values are 0<=integer<=100. The default=75. 
# This argument brightens/darkens the composite mask used to control the mixing 
# of the image and texture (using -white-threshold). 
# 
# NOTE: Both strength and gain produce darkening or lightening of the cracks. 
# My nominal mode of operation is to set the strength to 100. Then adjust the 
# gain to a value that produces cracks that are strong but do not add other 
# artifacts to the image. Then I reduce the strength to create less cracks if 
# desired.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
mode="cracks" 	# cracks or scratches
strength=100	
gain=75


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
				-m)    # get  mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$mode" in 
					   		cracks|c) mode="cracks" ;;
					   		scratches|s) mode="scratches" ;;
					   		*) errMsg "--- MODE=$mode IS AN INVALID VALUE ---" 
					   	esac
					   ;;
				-s)    # get strength
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID STRENGTH SPECIFICATION ---"
					   checkMinus "$1"
					   strength=`expr "$1" : '\([0-9]*\)'`
					   [ "$strength" = "" ] && errMsg "STRENGTH=$strength MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-g)    # get gain
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GAIN SPECIFICATION ---"
					   checkMinus "$1"
					   gain=`expr "$1" : '\([0-9]*\)'`
					   [ "$gain" = "" ] && errMsg "GAIN=$size MUST BE A NON-NEGATIVE INTEGER"
		   			   testA=`echo "$gain < 0" | bc`
		   			   testB=`echo "$gain > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- GAIN=$gain MUST BE AN INTEGER BETWEEEN 0 AND 100 --- "
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
	texturefile="$2"
	outfile="$3"
fi

# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"

# test that texturefile provided
[ "$texturefile" = "" ] && errMsg "NO TEXTURE FILE SPECIFIED"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"


# setup temporary images
tmpA1="$dir/peelingpaint_1_$$.mpc"
tmpB1="$dir/peelingpaint_1_$$.cache"
tmpA2="$dir/peelingpaint_2_$$.mpc"
tmpB2="$dir/peelingpaint_2_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" ERR


# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

# read the texture image into the temporary cached image and test if valid
convert -quiet -regard-warnings "$texturefile" +repage "$tmpA2" ||
	errMsg "--- FILE $texturefile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

# get input size
dim=`convert $tmpA1 -format "%wx%h" info:`

# set up gain
gain=$((100-gain))

# set up brightness
if [ "$strength" = "100" ]; then
	strengthen=""
else
	strength=`convert xc: -format "%[fx:$strength/100]" info:`
	strengthen="-evaluate pow $strength"
fi

# set up mode
[ "$mode" = "cracks" ] && cmethod="bumpmap"
[ "$mode" = "scratches" ] && cmethod="hardlight"

convert $tmpA1 \( $tmpA2 -resize ${dim}\^ -gravity center -crop ${dim}+0+0 +repage $strengthen \) \
	\( +clone -negate -white-threshold $gain% \) -compose $cmethod -composite "$outfile"


exit 0
