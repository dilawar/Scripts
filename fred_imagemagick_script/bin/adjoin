#!/bin/bash
#
# Developed by Fred Weinhaus 12/3/2008 .......... revised 10/9/2014
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
# USAGE: adjoin [-m mode] [-g gravity] [-b bgcolor] infile1 ... infileN outfile
# USAGE: adjoin [-h or -help]
#
# OPTIONS:
#
# -m      mode           append mode: horizontal (or H) or vertical (or V)
# -g      gravity        gravity justification; default=H
#                        for mode=H, options are: north, center, south;
#                        for mode=V, options are: east, center, west;
#                        default=center
# -b      bgcolor        background color for fill areas; default=black
#
###
#
# NAME: ADJOIN
# 
# PURPOSE: To append multiple images together with positional justification.
# 
# DESCRIPTION: ADJOIN appends multiple images together with positional justification. 
# Images may be appended either horizontally or vertically. For horizontal 
# appending, position justification may be north (or top), center or 
# south (or bottom). For vertical appending, position justification may 
# be east (or right), center or west (or left). Any valid IM 
# color including none (for transparent) may be used for fill areas.
# 
# 
# OPTIONS: 
# 
#
# -m mode ... MODE is the direction for appending. Choices are either 
# horizontal (or H) or vertical (or V). If horizontal, the images will be 
# appended in sequence from left to right. If vertical, the images will be 
# appended in sequence from top to bottom. The default=horizontal.
#
# -g gravity ... GRAVITY identifies the desired positional justification 
# when appending. For mode=horizontal, positional justification may be north 
# (or top), center or south (or bottom). For mode=vertical, positional
# justification may be east (or right), center or west (or left). 
# The default=center.
# 
# -b bgcolor ... BGCOLOR specifies the background color to use for the fill 
# areas. Any valid IM color specification may be used. To make the background 
# transparent, set the color to none. The default is black.
# 
# IMPORTANT: This script may only work for IM 6.4.6-6 or higher due to a 
# bug fix in -extent at that release.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
mode="horizontal"	# mode: horizontal or vertical
bgcolor="black"		# color or none for transparent
gravity="center"	# gravity location


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
elif [ $# -lt 3 ]
	then
	errMsg "--- TOO FEW ARGUMENTS WERE PROVIDED ---"
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
		 		-m)    # mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   # test mode values
					   mode="$1"
					   case "$mode" in 
					   		horizontal) mode=horizontal ;;
					   		H) mode=horizontal ;;
					   		h) mode=horizontal ;;
					   		vertical) mode=vertical ;;
					   		V) mode=vertical ;;
					   		v) mode=vertical ;;
					   		*) errMsg "--- MODE=$mode IS AN INVALID VALUE ---" 
					   	esac
					   ;;
		 		-b)    # bgcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
					   ;;
		 		-g)    # gravity
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GRAVITY SPECIFICATION ---"
					   checkMinus "$1"
					   # test gravity values
					   gravity="$1"
					   gravity=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$gravity" in 
					   		center) gravity=center ;;
					   		north) gravity=north ;;
					   		top) gravity=north ;;
					   		south) gravity=south ;;
					   		bottom) gravity=south ;;
					   		east) gravity=east ;;
					   		right) gravity=east ;;
					   		west) gravity=west ;;
					   		left) gravity=west ;;
					   		*) errMsg "--- GRAVITY=$gravity IS AN INVALID VALUE ---" 
					   	esac
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
fi


# get infiles and outfile
fileArray=($@)
nfiles=$#
[ $nfiles -lt 3 ] && errMsg "--- TOO FEW IMAGES WERE PROVIDED ---"
lastfile=`expr $nfiles - 1`
outfile="${fileArray[$lastfile]}"

# test that each infile is valid and put names into list
i=0
infilelist=""
while [ $i -lt $lastfile ]; do
	# test if image an ordinary, readable and non-zero size
	testfile="${fileArray[$i]}"
	if [ -f $testfile -a -r $testfile -a -s $testfile ]
		then
		infilelist="$infilelist $testfile"
		else
			errMsg "--- FILE $testfile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
			exit 1
	fi
	i=`expr $i + 1`
done

# remove leading space
infilelist=${infilelist# }


# test that infilelist provided
[ "$infilelist" = "" ] && errMsg "NO INPUT FILES SPECIFIED"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"

# get im version
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
# The following was determined from various version tests using adjoin 
# with one linear grayscale image and two color images.
# with IM 6.7.4.10, 6.7.6.10, 6.7.8.6
if [ "$im_version" -lt "06070606" ]; then
	cspace="RGB"
else
	cspace="sRGB"
fi

if [ "$mode" = "horizontal" ]; then
	[ "$gravity" != "north" -a "$gravity" != "center" -a "$gravity" != "south" ] && errMsg "--- INVALID GRAVITY SPECIFIED ---"
	maxh=0
	for img in $infilelist; do
	hh=`convert $img -ping -format "%h" info:`
	if [ $hh -gt $maxh ]; then
	maxh=$hh
	fi
	done
	convert $infilelist -gravity $gravity -background $bgcolor \
		-extent x${maxh} -set colorspace $cspace +append $outfile

elif [ "$mode" = "vertical" ]; then
	[ "$gravity" != "east" -a "$gravity" != "center" -a "$gravity" != "west" ] && errMsg "--- INVALID GRAVITY SPECIFIED ---"
	maxw=0
	for img in $infilelist; do
	ww=`convert $img -ping -format "%w" info:`
	if [ $ww -gt $maxw ]; then
	maxw=$ww
	fi
	done
	convert $infilelist -gravity $gravity -background $bgcolor \
		-extent ${maxw}x -set colorspace $cspace -append "$outfile"
fi
exit 0