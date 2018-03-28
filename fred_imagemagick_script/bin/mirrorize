#!/bin/bash
#
# Developed by Fred Weinhaus 5/25/2008 .......... revised 4/25/2015
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
# USAGE: mirrorize [-r region] infile outfile
# USAGE: mirrorize [-h or -help]
#
# OPTIONS:
#
# -r      region         region of image to be mirrored;
#                        half image values: North, South, East, West;
#                        image quadrant values: NorthWest, NorthEast, 
#                        SouthWest, SouthEast; Default=West
#
###
#
# NAME: MIRRORIZE 
# 
# PURPOSE: To create a mirror effect in an image.
# 
# DESCRIPTION: MIRRORIZE creates a mirror effect in an image by 
# mirroring any half-image to create the other half or by mirroring 
# any image quadrant to create the other quadrants. 
# 
# OPTIONS: 
# 
# -r region ... REGION is the region to be mirrored. This can be any 
# half image specified as North, South, East or West. It can also be 
# any image quadrant specified as NorthWest, NorthEast, SouthWest or 
# SouthEast. The default is West.
# 
# NOTE: if the image has any odd dimension, then after the mirrorizing, 
# the output image will be one pixel smaller in that dimension.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
region="West"

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
elif [ $# -gt 4 ]
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
				-r)    # get  region
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID REGION SPECIFICATION ---"
					   checkMinus "$1"
					   region="$1"
					   case "$1" in 
					   		North) ;;
					   		north) ;;
					   		South) ;;
					   		south) ;;
					   		East) ;;
					   		east) ;;
					   		West) ;;
					   		west) ;;
					   		NorthWest) ;;
					   		northwest) ;;
					   		NorthEast) ;;
					   		northeast) ;;
					   		SouthWest) ;;
					   		southwest) ;;
					   		SouthEast) ;;
					   		southeast) ;;
					   		*) errMsg "--- DIRECTION=$direction IS AN INVALID VALUE ---" 
					   	esac
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

tmpA="$dir/mirror_1_$$.mpc"
tmpB="$dir/mirror_1_$$.cache"
tmpC="$dir/mirror_2_$$.mpc"
tmpD="$dir/mirror_2_$$.cache"
trap "rm -f $tmpA $tmpB $tmpC $tmpD;" 0
trap "rm -f $tmpA $tmpB $tmpC $tmpD; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB $tmpC $tmpD; exit 1" ERR

ww=`convert $infile -format "%w" info:`
hh=`convert $infile -format "%h" info:`
ww2=`convert $infile -format "%[fx:floor(w/2)]" info:`
hh2=`convert $infile -format "%[fx:floor(h/2)]" info:`

#echo "ww=$ww"
#echo "hh=$hh"
#echo "ww2=$ww2"
#echo "hh2=$hh2"

#convert region to lowercase
region=`echo "$region" | tr "[:upper:]" "[:lower:]"`


if [ "$region" = "west" ]
	then
	convert $infile[${ww2}x${hh}+0+0] \( +clone -flop \) +append "$outfile"
elif [ "$region" = "east" ]
	then
	convert $infile[${ww2}x${hh}+${ww2}+0] \( +clone -flop \) +swap +append "$outfile"
elif [ "$region" = "north" ]
	then
	convert $infile[${ww}x${hh2}+0+0] \( +clone -flip \) -append "$outfile"
elif [ "$region" = "south" ]
	then
	convert $infile[${ww}x${hh2}+0+${hh2}] \( +clone -flip \) +swap -append "$outfile"
elif [ "$region" = "northwest" ]
	then
	convert $infile[${ww2}x${hh2}+0+0] \( -clone 0 -flop \) \
		\( -clone 0 -flip \) \( -clone 0 -rotate 180 \) \
		\( -clone 0 -clone 1 +append \) \
		\( -clone 2 -clone 3 +append \) \
		-delete 0-3 -append \
		"$outfile"
elif [ "$region" = "northeast" ]
	then
	convert $infile[${ww2}x${hh2}+${ww2}+0] \( -clone 0 -flop \) \
		\( -clone 0 -flip \) \( -clone 0 -rotate 180 \) \
		\( -clone 1 -clone 0 +append \) \
		\( -clone 3 -clone 2 +append \) \
		-delete 0-3 -append \
		"$outfile"
elif [ "$region" = "southwest" ]
	then
	convert $infile[${ww2}x${hh2}+0+${hh2}] \( -clone 0 -flop \) \
		\( -clone 0 -flip \) \( -clone 0 -rotate 180 \) \
		\( -clone 2 -clone 3 +append \) \
		\( -clone 0 -clone 1 +append \) \
		-delete 0-3 -append \
		"$outfile"
elif [ "$region" = "southeast" ]
	then
	convert $infile[${ww2}x${hh2}+${ww2}+${hh2}] \( -clone 0 -flop \) \
		\( -clone 0 -flip \) \( -clone 0 -rotate 180 \) \
		\( -clone 3 -clone 2 +append \) \
		\( -clone 1 -clone 0 +append \) \
		-delete 0-3 -append \
		"$outfile"
fi
exit 0




