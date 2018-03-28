#!/bin/bash
# 
# Developed by Fred Weinhaus 9/18/2007 .......... revised 4/25/2015
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
# USAGE: binomialedge [-w width] [-k kind] [-m mix] [-e edge] [-t threshold] [-b blur] infile outfile
# USAGE: binomialedge [-h or -help]
# 
# OPTIONS:
# 
# -w        width             width of square filter; width=3, 5 or 7; default=3
# -k        kind              kind=high or low (pass filter); default=high
# -m        mix               mixing percent with original image; 
#                             mix=integer 0 to 100; default=20
# -e        edge          	  edge method used to threshold edges;
#                             edge=0 is grayscale thresholding;
#                             edge=1 is binary thresholding;
#                             edge=2 is no edge masking
#                             default=0
# -t        threshold         percent edge threshold value; 0 to 100; 
#                             default=25
# -b        blur              edge blurring distance; float >= 0;
#                             default=1
# -h                          get help information
# -help                       get help information
#   
###
# 
# NAME: BINOMIALEDGE
# 
# PURPOSE: To sharpen (or blur) an image near edges using a Binomial shaped 
# filter. 
# 
# DESCRIPTION: BINOMIALEDGE generates an output image which sharpenes (or 
# blurs) only near edges using a Binomial shaped filter. The script first 
# generates a mix of the image with either a high pass sharpened version of 
# the image or low pass blurred version of the image. A high pass edge image 
# is then thresholded and used as a mask to blend the sharpened or blurred 
# image with the original image.
# 
# The basic blended low pass filtering formula is F = (1-m)*I + m*L, where I is
# the original image, L is the Binomial low pass filtered image and m = mix/100. 
# When m=0, we get only the original image and when m=1, we get only the low pass
# Gaussian filtered image. For intermediate value of m, we get a blend of the
# image and the Binomial low pass filtered image. For high pass filtering, we 
# form the high pass filter by subtracting the low pass filter from the original 
# Thus in the formula above L is replaced by H=(I-L), and as the high pass image  
# is primarily black we simply add the percentage of the high pass image to the  
# original image so that F = I + m*H, which also can be expressed as 
# F = (1+m)*I - m*L. Once the high pass sharpened image or low pass blurred image 
# is created, a high pass edge image is then thresholded and used as a mask to 
# blend the sharpened or blurred image with the original image.
# 
# For width=3, the 1D binomial series is 1 2 1. For width=5, the 1D binomial 
# series is 1 4 6 4 1. For width=7, the 1D binomial series is 1 6 15 20 15 6 1.
# To form the 2D binomial filters, the outer product of the 1D column with the 
# 1D row is computed.
#
# For width=3, this becomes:
# 1 2 1
# 2 4 2
# 1 2 1    
#
# For width=5, this becomes:
# 1  4  6  4 1
# 4 16 24 16 4
# 6 24 36 24 6
# 4 16 24 16 4
# 1  4  6  4 1
#
# For width=7, this becomes:
#  1   6  15  20  15   6  1
#  6  36  90 120  90  36  6
# 15  90 225 300 225  90 15
# 20 120 300 400 300 120 20
# 15  90 225 300 225  90 15
#  6  36  90 120  90  36  6
#  1   6  15  20  15   6  1
# 
# For more information about the binomial coefficients, see 
# <a href="http://en.wikipedia.org/wiki/Pascal%27s_triangle" target="_blank">Pascal's Triangle</a>.
# 
# OPTIONS: 
# 
# -w width is dimension of a square convolution kernel. Width can be 3, 5 or 7. 
# the default is 3. For example a width of 3 will generate a 3x3 convolution 
# kernel. 
# 
# -k kind ... KIND is the kind of filter, either a high pass or low pass filter 
# are allowed. Thus kind=high or low. The default=high. A low pass filter will 
# cause blurring and a high pass filtered image will produce sharpening.
# 
# -m mix ... MIX is the percentage mixing factor to use to blend the filtered  
# result with the original image. For kind=low, a value of mix=0, results in 
# the original image and a value of mix=100 results in a pure low pass filtered 
# image. For low pass filtering, a larger value for mix will produce more blurring. 
# For kind=high, the mix percentage of the high pass filtered image will be 
# added to the original image. A value of mix=0, results in the original image.
# A larger value for mix will sharpen or highlight the edges more. The default 
# is mix=20.
# 
# -e edge ... EDGE is the method used to threshold the standard  
# deviation image to get an edge mask image for use in compositing the 
# original and sharpened image. Vaules may be 0, 1 or 2. A value of 0 
# indicates that thresholding will be done using -black-threshold so that any  
# value below the threshold is black but values above remain grayscale edges. 
# A value of 1 indicates that thresholding will be done using -threshold to 
# generate a binary edge mask image. A value of 2 indicates that no mask will  
# be used. Thus sharpening (or blurring) will be done throughout the image. 
# Value 0 produces the least edge sharpening (or blurring). Value 1 produces a 
# greater amount of edge sharpening (or blurring). Value 2 sharpens (or blurs) 
# throughout the image. The default is 0.
# 
# -t threshold ... THRESHOLD is the percentage threshold value to use. Values 
# are integers between 0 and 100. The lower the threshold the more edges will 
# be sharpened. Threshold is not needed or ignored for method=2. The default 
# is 25.
# 
# -b blur ... BLUR is the edge blurring distance to spread the edges wider 
# by a small amount. Values are floats >= 0. The default is 1.
#
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 
# 

# set default params
width=3
mix=20
kind="high"
edge=0
thresh=25
bd=1.0

# set directory for temporary files
dir="."    # suggestions are dir="." or dir="/tmp"

# define binomial coefficients
bin3="1 2 1"
bin5="1 4 6 4 1"
bin7="1 6 15 20 15 6 1"

# set up functions to report Usage and Usage with Description
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
PROGNAME=`basename $PROGNAME`          # base name of program
usage1() 
	{
	echo >&2 ""
	echo >&2 "$PROGNAME:" "$@"
	sed >&2 -n '/^###/q;  /^#/!q;  s/^#//;  s/^ //;  4,$p' "$PROGDIR/$PROGNAME"
	}
usage2() 
	{
	echo >&2 ""
	echo >&2 "$PROGNAME:" "$@"
	sed >&2 -n '/^######/q;  /^#/!q;  s/^#*//;  s/^ //;  4,$p' "$PROGDIR/$PROGNAME"
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
elif [ $# -eq 3 -o $# -eq 5 -o $# -eq 7 -o $# -eq 9 -o $# -eq 11 -o $# -eq 13 -o $# -gt 14 ]
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
				-w)    # get width
					   shift  # to get the next parameter - width
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID WIDTH SPECIFICATION ---"
					   checkMinus "$1"
					   width="$1"
					   # test width values
					   [ $width -ne 3 -a $width -ne 5 -a $width -ne 7 ] && errMsg "--- WIDTH=$width IS NOT A VALID VALUE ---"
					   ;;
				-k)    # get kind
					   shift  # to get the next parameter - kind
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID KIND SPECIFICATION ---"
					   checkMinus "$1"
					   kind="$1"
					   # test width values
					   [ "$kind" != "low" -a "$kind" != "high" -a "$kind" != "edge" ] && errMsg "--- KIND=$kind IS NOT A VALID VALUE ---"
					   ;;
				-m)    # get mix
					   shift  # to get the next parameter - mix
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MIX SPECIFICATION ---"
					   checkMinus "$1"
					   # test mix values
					   mix=`expr "$1" : '\([0-9]*\)'`
					   [ "$mix" = "" ] && errMsg "--- MIX=$mix MUST BE AN INTEGER ---"
					   mixtestA=`echo "$mix < 0" | bc`
					   mixtestB=`echo "$mix > 100" | bc`
					   [ $mixtestA -eq 1 -o $mixtestB -eq 1 ] && errMsg "--- MIX=$mix MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-e)    # edge method
					   shift  # to get the next parameter - edge
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID EDGE METHOD SPECIFICATION ---"
					   checkMinus "$1"
					   edge="$1"
					   [ $edge -ne 0 -a $edge -ne 1 -a $edge -ne 2 ] && errMsg "--- EDGE=$edge MUST BE EITHER 0, 1 OR 2 ---"
					   ;;
				-t)    # threshold
					   shift  # to get the next parameter - thresh
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID THRESHOLD SPECIFICATION ---"
					   checkMinus "$1"
					   thresh=`expr "$1" : '\([0-9]*\)'`
					   [ "$thresh" = "" ] && errMsg "--- THRESHOLD=$thresh MUST BE A NON-NEGATIVE INTEGER ---"
					   thresholdtestA=`echo "$thresh < 0" | bc`
					   thresholdtestB=`echo "$thresh > 100" | bc`
					   [ $thresholdtestA -eq 1 -o $thresholdtestB -eq 1 ] && errMsg "--- THRESHOLD=$thresh MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-b)    # blur
					   shift  # to get the next parameter - bd
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BLUR DISTANCE SPECIFICATION ---"
					   checkMinus "$1"
					   bd=`expr "$1" : '\([.0-9]*\)'`
					   [ "$bd" = "" ] && errMsg "--- BLUR=$bd MUST BE A NON-NEGATIVE FLOATING POINT VALUE (with no sign) ---"
					   bdtest=`echo "$bd < 0" | bc`
					   [ $bdtest -eq 1 ] && errMsg "--- BLUR=$bd MUST BE A NON-NEGATIVE FLOATING POINT VALUE ---"
					   ;;
				 -)    # STDIN and end of arguments
					   break
					   ;;
				-*)    # any other - argument
					   errMsg "--- UNKNOWN OPTION ---"
					   ;;
				*)     # end of arguments
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
[ "$infile" = "" ] && errMsg "--- NO INPUT FILE SPECIFIED ---"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "--- NO OUTPUT FILE SPECIFIED ---"

tmpA="$dir/binomialedge_$$.mpc"
tmpB="$dir/binomialedge_$$.cache"
tmp0="$dir/binomialedge_0_$$.miff"
tmp1="$dir/binomialedge_1_$$.miff"
trap "rm -f $tmpA $tmpB $tmp0 $tmp1;" 0
trap "rm -f $tmpA $tmpB $tmp0 $tmp1; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB $tmp0 $tmp1; exit 1" ERR

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
# The following was determined from various version tests using binomialedge.
# with IM 6.7.4.10, 6.7.6.10, 6.7.8.7
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
fi

if convert -quiet "$infile" +repage $setcspace "$tmpA"
	then
	: 'do nothing'
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

num=`expr $width \* $width`

# function to make outer product
outerProduct()
	{
	bin=$1
	binArr=($bin)
	i=0
	while [ $i -lt $width ]
		do
		j=0
		while [ $j -lt $width ]
			do
			rowArr[$j]=`echo "scale=0; (${binArr[$i]} * ${binArr[$j]}) / 1" | bc`
			j=`expr $j + 1`
		done
		binomial[$i]=${rowArr[*]}
		i=`expr $i + 1`
	done
	}


[ $width -eq 3 ] && outerProduct "$bin3"
[ $width -eq 5 ] && outerProduct "$bin5"
[ $width -eq 7 ] && outerProduct "$bin7"


# get IM 1D kernel
kern1D=${binomial[*]}
kern1D=`echo $kern1D | sed 's/ /,/g'`

if [ "$bd" = "0" ]
	then
	blurstr=""
else
	blurstr="-blur 0x$bd"
fi

if [ $edge -eq 0 ]
	then
	threshstr="-black-threshold $thresh%"
elif [ $edge -eq 1 ]
	then
	threshstr="-threshold $thresh%"
fi

# get thresholded edge image
if [ $edge != 2 ]
	then
	convert $tmpA \( $tmpA -convolve "$kern1D" \) +swap -compose minus -composite \
		-colorspace Gray $blurstr -contrast-stretch 0% $threshstr $tmp0
fi

#process the image
if [ "$kind" = "low" ]
	then
	convert $tmpA -convolve "$kern1D" $tmp1
#	composite -blend $mix% $tmp1 $tmpA $tmp1
	convert $tmpA $tmp1 -define compose:args=$mix% -compose blend -composite $tmp1
elif [ "$kind" = "high" ]
	then
	convert $tmpA \( $tmpA -convolve "$kern1D" \) +swap -compose minus -composite -normalize $tmp1
#	composite -blend $mix%x100% $tmp1 $tmpA $tmp1
	convert $tmpA $tmp1 -define compose:args=$mix%x100% -compose blend -composite $tmp1
fi

# composite input image and sharpened image using edge mask image
if [ $edge != 2 ]
	then
	convert $tmpA $tmp1 $tmp0 -composite "$outfile"
else
	convert $tmp1 "$outfile"
fi
exit 0
