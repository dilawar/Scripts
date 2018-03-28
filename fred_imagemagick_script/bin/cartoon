#!/bin/bash
#
# Developed by Fred Weinhaus 3/6/2013 .......... revised 2/19/2017
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
# USAGE: cartoon [-p pattern] [-n numlevels] [-m method] [-e edgeamount] 
# [-b brightness] [-s saturation] infile outfile
# USAGE: cartoon [-h or -help]
# 
# OPTIONS:
# 
# -p      pattern           segmentation pattern (shape); 0<=integer<=100;
#                           default=70
# -n      numlevels         number of desired segmentation levels; integer>=2;
#                           default=6
# -m      method            edge method; 1, 2, 3 or 4; default=1
# -e      edgeamount        amount of edges; float>=0; default=4
# -b      brightness        brightness of cartoon; integer>=0; default=100
# -s      saturation        saturation of cartoon; integer>=0; default=100
# 
###
# 
# NAME: CARTOON 
# 
# PURPOSE: To create a cartoon-like appearance to an image.
# 
# DESCRIPTION: CARTOON creates a cartoon-like appearance to an image. The 
# image is smoothed and then multiplied by a grayscale version of the image 
# with the desired number of levels to produce the segmented appearance.  
# The pattern parameter changes the shape of the segmentation for the given 
# number of levels. Edges are then superimposed onto the image. 
# 
# 
# ARGUMENTS: 
# 
# -p pattern ... PATTERN is the pattern or shape of the segmentation. Values 
# are between 0 and 100. The default=70.
# 
# -n numlevels ... NUMLEVELS is the desired number of segmentation levels.  
# Values are integers>=2. The default=6.
# 
# -m method ... edge METHOD. Choices are 1, 2, 3 or 4. The default=1. 
# 
# -e edgeamount ... EDGEAMOUNT is the amount of edges to overlay on the 
# cartoon. Values are floats>=0. The default=4.
# 
# -b brightness ... BRIGHTNESS of the cartoon. Values are integer>=0. The
# default=100 (no change).
# 
# -s saturation ... SATURATION of the cartoon. Values are integer>=0. The
# default=150.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
pattern=70			# segmentation pattern; 0<=integer<=100
numlevels=6			# numlevels; integer>=2
method=1            # edge method; 1 or 2
edgeamount=4		# edge amount; float>=0
brightness=100		# brightness; integer>=0
saturation=150		# saturation; integer>=0

# fixed arguments
edgewidth=2			# edge width; integer>=0
edgethresh=90		# edge threshold; 0<=integer<=100
edgegain=4			# edge gain for method=2

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
				-p)    # get pattern
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID PATTERN SPECIFICATION ---"
					   checkMinus "$1"
					   pattern=`expr "$1" : '\([0-9]*\)'`
					   [ "$pattern" = "" ] && errMsg "--- PATTERN=$pattern MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$pattern < 0" | bc`
					   test2=`echo "$pattern > 100" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- PATTERN=$pattern MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-n)    # get  numlevels
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NUMLEVELS SPECIFICATION ---"
					   checkMinus "$1"
					   numlevels=`expr "$1" : '\([0-9]*\)'`
					   [ "$numlevels" = "" ] && errMsg "--- NUMLEVELS=$numlevels MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test=`echo "$numlevels < 2" | bc`
					   [ $test -eq 1 ] && errMsg "--- NUMLEVELS=$numlevels MUST BE AN INTEGER GREATER THAN 1 ---"
					   ;;
				-m)    # get  method
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID METHOD SPECIFICATION ---"
					   checkMinus "$1"
					   method=`expr "$1" : '\([0-9]*\)'`
					   [ "$method" = "" ] && errMsg "--- METHOD=$method MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$method <= 0" | bc`
					   test2=`echo "$method >= 5" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- METHOD=$method MUST BE EITHER 1, 2, 3 or 4 ---"
					   ;;
				-e)    # get  edgeamount
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID EDGEAMOUNT SPECIFICATION ---"
					   checkMinus "$1"
					   edgeamount=`expr "$1" : '\([.0-9]*\)'`
					   [ "$edgeamount" = "" ] && errMsg "--- EDGEAMOUNT=$edgeamount MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   ;;
				-b)    # get brightness
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BRIGHTNESS SPECIFICATION ---"
					   checkMinus "$1"
					   brightness=`expr "$1" : '\([0-9]*\)'`
					   [ "$brightness" = "" ] && errMsg "--- BRIGHTNESS=$brightness MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
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
tmpA1="$dir/cartoon_1_$$.mpc"
tmpB1="$dir/cartoon_1_$$.cache"
tmpA2="$dir/cartoon_2_$$.mpc"
tmpB2="$dir/cartoon_2_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 0" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" 1 2 3 15
#trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" ERR

# get im version
im_version=`convert -list configure | \
sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# colorspace RGB and sRGB swapped between 6.7.5.5 and 6.7.6.7 
# though probably not resolved until the latter
# then -colorspace gray changed to linear between 6.7.6.7 and 6.7.8.2 
# then -separate converted to linear gray channels between 6.7.6.7 and 6.7.8.2,
# though probably not resolved until the latter
# so -colorspace HSL/HSB -separate and -colorspace gray became linear
# but we need to use -set colorspace RGB before using them at appropriate times
# so that results stay as in original script
# The following was determined from various version tests using cartoon.
# with IM 6.7.4.10, 6.7.6.10, 6.7.7.7, 6.8.3.7
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
if [ "$im_version" -le "06070707" ]; then
	proc="-gamma 2.2"
else
	proc="-colorspace sRGB"
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
	proc="-gamma 2.2"
fi

# read the input image into the temporary cached image, convert to depth 8 and selective blur
convert -quiet "$infile" +repage -depth 8 -selective-blur 0x5+10% "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

ww=`identify -ping -format "%w" $tmpA1`
hh=`identify -ping -format "%h" $tmpA1`

# set up median filter
if [ "$im_version" -ge "06060806" ]; then
	medproc="-statistic median 3x3"
else
	medproc="-median 1"
fi


# convert to grayscale and posterize to create mask image
# convert $tmpA1 -level 0x$pattern% $setcspace -colorspace gray -posterize $numlevels -colorspace $cspace -depth 8 $tmpA2
convert $tmpA1 -level 0x$pattern% $setcspace -colorspace gray -posterize $numlevels -depth 8 $proc $tmpA2

if [ $method -eq 1 ]; then
	# process image
	# multiply the blurred posterized graycale mask with the smoothed input
	# convert smoothed input to grayscale
	# negate and blur
	# colordodge composite the grayscale and negated/blurred version to make edgewidth image
	# use power to amplify and then threshold and median filter
	# multiply composite the edgewidth with the blended image
	convert $tmpA1 \( $tmpA2 -blur 0x1 \) \
		\( -clone 0 -clone 1 -compose over -compose multiply -composite -modulate $brightness,$saturation,100 \) \
		\( -clone 0 $setcspace -colorspace gray \) \
		\( -clone 3 -negate -blur 0x${edgewidth} \) \
		\( -clone 3 -clone 4 -compose over -compose colordodge -composite \
			-evaluate pow $edgeamount -threshold $edgethresh% $medproc \) \
			-delete 0,1,3,4 -compose over -compose multiply -composite "$outfile"
elif [ $method -eq 2 ]; then
	# process image
	# multiply the blurred posterized graycale mask with the smoothed input
	# convert smoothed input to grayscale
	# apply high pass filter to grayscale, use power to amplify and threshold
	# multiply composite the edge image with the smoothed color image
	convert $tmpA1 \( $tmpA2 -blur 0x1 \) \
		\( -clone 0 -clone 1 -compose over -compose multiply -composite -modulate $brightness,$saturation,100 \) \
		\( -clone 0 $setcspace -colorspace gray -negate -define convolve:scale=$edgegain \
			-morphology Convolve DoG:0,0,${edgewidth} -negate \
			-evaluate pow $edgeamount -white-threshold $edgethresh% \) \
		-delete 0,1 -compose over -compose multiply -composite "$outfile"
elif [ $method -eq 3 ]; then
	# process image
	# multiply the blurred posterized graycale mask with the smoothed input
	# convert smoothed input to grayscale
	# create sobel edges
	# multiply composite the edge image with the smoothed color image
	convert $tmpA1 \( $tmpA2 -blur 0x1 \) \
		\( -clone 0 -clone 1 -compose over -compose multiply -composite -modulate $brightness,$saturation,100 \) \
		\( -clone 0 $setcspace -colorspace gray -define convolve:scale='!' \
			-define morphology:compose=Lighten -morphology Convolve  'Sobel:>' \
			-negate -evaluate pow $edgeamount -white-threshold $edgethresh% \) \
		-delete 0,1 -compose over -compose multiply -composite "$outfile"
elif [ $method -eq 4 ]; then
	# process image
	# multiply the blurred posterized graycale mask with the smoothed input
	# convert smoothed input to grayscale
	# create morphology edges
	# multiply composite the edge image with the smoothed color image
	convert $tmpA1 \( $tmpA2 -blur 0x1 \) \
		\( -clone 0 -clone 1 -compose over -compose multiply -composite -modulate $brightness,$saturation,100 \) \
		\( -clone 0 $setcspace -colorspace gray -define convolve:scale='!' \
			-morphology edge diamond:1 \
			-negate -evaluate pow $edgeamount -white-threshold $edgethresh% \) \
		-delete 0,1 -compose over -compose multiply -composite "$outfile"
fi


exit 0	
