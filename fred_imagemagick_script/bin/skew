#!/bin/bash
#
# Developed by Fred Weinhaus 8/6/2008 ..........4/15/2015
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
# USAGE: skew [-a amount] [-m mode] [-d direction] [-v vpmethod] [-b bgcolor] infile outfile
# USAGE: skew [-h or -help]
#
# OPTIONS:
#
# -a      amount           amount of skew as either degrees or pixels as 
#                          specified by mode; default=0
# -m      mode             degrees or pixels; default=degrees
# -d      direction		   b2r, b2l, r2b or r2t; first letter indicates 
#                          which edge (bottom or right) to skew and the 
#                          last letter indicates which direction to skew 
#                          (right, left, bottom or top)
# -v      vpmethod         virtual-pixel method to fill background area 
#                          around skewed region; default=transparent
# -b      bgcolor          skew fill color when vpmethod=background; any
#                          IM color specification or none; default is white
#
###
#
# NAME: SKEW
# 
# PURPOSE: Applies a skew distortion to an image.
# 
# DESCRIPTION: SKEW applies a skew distortion to an image in either the X 
# or the Y direction. The amount of skew can be specified in degrees or pixels. 
#  
# OPTIONS: 
# 
# -a amount ... AMOUNT of skew in either degrees or pixels as specified by the 
# mode parameter. The default is 0 or no skew.
#
# -m mode ... MODE specifies whether the amount is in degrees or pixels. The 
# default is degrees.
# 
# -d direction ... DIRECTION specifies whether the skew is horizontal or vertical 
# and at the same time whether positive or negative. This is done via the following 
# options: b2r, b2l, r2b or r2t. These stand for: shift the bottom edge to the right, 
# shift the bottom edge to the left, shift the right edge to the bottom and shift 
# the right edge to the top. The default is b2r (shift the bottom edge to the right).
# 
# -v vpmethod ... VPMETHOD is the virtual-pixel method to use to fill the area added 
# due to the skew. Any valid IM virtual-pixel method is allowed. The default is 
# background.
#
# -b bgcolor ... BGCOLOR specifies the background color to use as the fill color for 
# areas added due to the skew when the virtual-pixel method is background. Any valid 
# IM color may be used including none. The default is none. Note that none will only 
# provide transparency for the fill areas, if the output image type supports transparency, 
# e.g. png or gif. If none is specified and the output type does not support transparency, 
# such as for jpg, then the resulting fill color will end up black.
# 
# REQUIREMENTS: This script works properly as of IM 6.4.2-7 when some fixes were made to 
# -affine -transform. For IM versions prior to this, the script will work to skew the 
# image. The virtual-pixel methods are ignored, But background color control may not 
# work as specified. Results may end up with transparent areas even if undesired for 
# output image types that support transparency, such as gif and png. Also there may be  
# an artifact showing for -d r2l results, especially when the background is transparent.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
amount=0
mode="degrees"			#pixels or degrees
direction="b2r"			#b2r, b2l, r2b, r2t
vpmethod="transparent" 	#virtual pixel method
bgcolor="white"			#none or any color


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
elif [ $# -gt 12 ]
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
				-a)    # get amount
					   shift  # to get the next parameter - amount
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT SPECIFICATION ---"
					   checkMinus "$1"
					   amount=`expr "$1" : '\([.0-9]*\)'`
					   [ "$amount" = "" ] && errMsg "AMOUNT=$amount MUST BE A NON-NEGATIVE FLOAT"
					   ;;
		 		-m)    # mode
					   shift  # to get the next parameter - mode
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   # test mode values
					   mode="$1"
					   [ "$mode" != "degrees" -a "$mode" != "pixels" ] && errMsg "--- MODE=$mode IS NOT A VALID VALUE ---"
					   ;;
		 		-d)    # direction
					   shift  # to get the next parameter - direction
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DIRECTION SPECIFICATION ---"
					   checkMinus "$1"
					   # test direction values
						case "$1" in
							b2r|B2R)	direction="$1"
										;;
							b2l|B2L)	direction="$1"
										;;
							r2b|R2B)	direction="$1"
										;;
							r2t|R2T)	direction="$1"
										;;
								  *)	errMsg "--- DIRECTION=$1 IS INVALID ---"
								  		;;
						esac
						;;
			   -v)    # get  vpmethod
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID VIRTUAL-PIXEL SPECIFICATION ---"
					   checkMinus "$1"
					   vpmethod="$1"
					   ;;
		 		-b)    # bgcolor
					   shift  # to get the next parameter - color
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
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
tmpA="$dir/skew_$$.mpc"
tmpB="$dir/skew_$$.cache"
trap "rm -f $tmpA $tmpB;" 0
trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB; exit 1" ERR

# test if valid input image
if convert -quiet "$infile" +repage "$tmpA"
	then
	: ' do nothing '
else
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

# get IM version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# enable alpha processing if "transparent"
if [ "$vpmethod" = "transparent" ]; then
	proc="-channel rgba -alpha on"
elif [ "$vpmethod" = "background" -a "$bgcolor" = "none" ]; then
	proc="-channel rgba -alpha on"
else
	proc=""
fi

if [ "$im_version" -ge "06040208" ]; then
	if [ "$mode" = "degrees" ]; then
		case "$direction" in
			b2r|B2R)	tanang=`convert xc: -format "%[fx:tan($amount*pi/180)]" info:`
						convert $tmpA \
						$proc -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,0,$tanang,1,0,0" "$outfile"
						;;
			b2l|B2L)	tanang=`convert xc: -format "%[fx:-tan($amount*pi/180)]" info:`
echo "tanang=$tanang;"
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,0,$tanang,1,0,0" "$outfile"
						;;
			r2b|R2B)	tanang=`convert xc: -format "%[fx:tan($amount*pi/180)]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,$tanang,0,1,0,0" "$outfile"
						;;
			r2t|R2T)	tanang=`convert xc: -format "%[fx:-tan($amount*pi/180)]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,$tanang,0,1,0,0" "$outfile"
						;;
		esac
	
	elif [ "$mode" = "pixels" ]; then
	   amount=`expr "$amount" : '\([0-9]*\)'`
	   [ "$amount" = "" ] && errMsg "AMOUNT=$amount MUST BE A NON-NEGATIVE INTEGER"
		case "$direction" in
			b2r|B2R)	tanang=`convert $tmpA -format "%[fx:$amount/h]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,0,$tanang,1,0,0" "$outfile"
						;;
			b2l|B2L)	tanang=`convert $tmpA -format "%[fx:-$amount/h]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,0,$tanang,1,0,0" "$outfile"
						;;
			r2b|R2B)	tanang=`convert $tmpA -format "%[fx:$amount/w]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,$tanang,0,1,0,0" "$outfile"
						;;
			r2t|R2T)	tanang=`convert $tmpA -format "%[fx:-$amount/w]" info:`
						convert $tmpA \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						+distort AffineProjection "1,$tanang,0,1,0,0" "$outfile"
						;;
		esac
	fi
else
	if [ "$mode" = "degrees" ]; then
		case "$direction" in
			b2r|B2R)	tanang=`convert xc: -format "%[fx:tan($amount*pi/180)]" info:`
						convert $tmpA -affine 1,0,$tanang,1,0,0 \
						$proc -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			b2l|B2L)	tanang=`convert xc: -format "%[fx:-tan($amount*pi/180)]" info:`
						convert $tmpA -affine 1,0,$tanang,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			r2b|R2B)	tanang=`convert xc: -format "%[fx:tan($amount*pi/180)]" info:`
						convert $tmpA -affine 1,$tanang,0,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			r2t|R2T)	tanang=`convert xc: -format "%[fx:-tan($amount*pi/180)]" info:`
						convert $tmpA -affine 1,$tanang,0,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
		esac
	
	elif [ "$mode" = "pixels" ]; then
	   amount=`expr "$amount" : '\([0-9]*\)'`
	   [ "$amount" = "" ] && errMsg "AMOUNT=$amount MUST BE A NON-NEGATIVE INTEGER"
		case "$direction" in
			b2r|B2R)	tanang=`convert $tmpA -format "%[fx:$amount/h]" info:`
						convert $tmpA -affine 1,0,$tanang,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			b2l|B2L)	tanang=`convert $tmpA -format "%[fx:-$amount/h]" info:`
						convert $tmpA -affine 1,0,$tanang,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			r2b|R2B)	tanang=`convert $tmpA -format "%[fx:$amount/w]" info:`
						convert $tmpA -affine 1,$tanang,0,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
			r2t|R2T)	tanang=`convert $tmpA -format "%[fx:-$amount/w]" info:`
						convert $tmpA -affine 1,$tanang,0,1,0,0 \
						$proc  -virtual-pixel $vpmethod -background $bgcolor \
						-transform "$outfile"
						;;
		esac
	fi
fi
exit 0