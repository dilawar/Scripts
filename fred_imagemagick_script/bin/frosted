#!/bin/bash
#
# Developed by Fred Weinhaus 7/13/2009 .......... revised 4/25/2015
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
# USAGE: frosted [-s spread] [-b bluramt] [-r reseed] infile outfile
# USAGE: frosted [-h or -help]
#
# OPTIONS:
#
# -s      spread             spread distance of dispersion; integer>=0; default=5
# -b      bluramt            amount to blur image before dispersion; integer>=0; default=5
# -r      reseed             forced seed value; integer>0; default will randomly change seed
#
###
#
# NAME: FROSTED 
# 
# PURPOSE: To apply a frosted glass effect to an image.
# 
# DESCRIPTION: FROSTED applies a frosted glass effect to an image by 
# using a sinusoidally modulaated random displacement map.
# 
# OPTIONS: 
# 
# -s spread ... SPREAD distance of dispersion. Values are integers>=0.
# Typical values range from 2 to 20. The default=5
#
# -b bluramt ... BLURAMT is the amount of blurring of the image in pixels 
# before applying the dispersion. Values are integers>=0. The default=5.
#
# -r reseed ... RESEED is the forced seed value to use for randomization. This 
# permits the pattern to be repeated. The default is to change the seed value 
# randomly each time the script is run, thus causing somewhat different 
# patterns each time the script is run.
# 
# NOTE: For IM prior to 6.4.8-5, the script uses -fx and may be a little slow.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
spread=5
bluramt=5
density=10

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
elif [ $# -gt 8 ]
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
				-s)    # get spread
					   shift  # to get the next parameter - spread
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SPREAD SPECIFICATION ---"
					   checkMinus "$1"
					   spread=`expr "$1" : '\([0-9]*\)'`
					   [ "$spread" = "" ] && errMsg "SPREAD=$spread MUST BE AN INTEGER"
#		   			   spreadtest=`echo "$spread < 1" | bc`
#					   [ $spreadtest -eq 1 ] && errMsg "--- SPREAD=$spread MUST BE A POSITIVE INTEGER ---"
					   ;;
				-b)    # get bluramt
					   shift  # to get the next parameter - bluramt
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BLURAMT SPECIFICATION ---"
					   checkMinus "$1"
					   bluramt=`expr "$1" : '\([0-9]*\)'`
					   [ "$bluramt" = "" ] && errMsg "BLURAMT=$bluramt MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-r)    # get  reseed
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RESEED SPECIFICATION ---"
					   checkMinus "$1"
					   reseed=`expr "$1" : '\([0-9]*\)'`
					   [ "$reseed" = "" ] && errMsg "--- RESEED=$reseed MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
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
tmpA1="$dir/frosted_1_$$.mpc"
tmpA2="$dir/frosted_1_$$.cache"
tmp0="$dir/frosted_0_$$.miff"
trap "rm -f $tmpA1 $tmpA2 $tmp0;" 0
trap "rm -f $tmpA1 $tmpA2 $tmp0; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpA2 $tmp0; exit 1" ERR

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
# The following was determined from various version tests using frosted.
# with IM 6.7.4.10, 6.7.6.10, 6.7.8.7
# NOTE: IM 6.7.7.8 and 6.7.7.9 do not seem to work properly
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
fi


# read the input images into the temp files and test validity.
if [ "$bluramt" != "0" -a "$bluramt" != "0.0" ]; then
convert -quiet "$infile" -blur 0x${bluramt} +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"
else
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"
fi

# compute image dimensions
ww=`convert $infile -format "%[fx:w]" info:`
hh=`convert $infile -format "%[fx:h]" info:`

# set up seed
if [ "$reseed" = "" ]; then
	seed=""
else
	seed="-seed $reseed"
fi


# create noise image
convert -size ${ww}x${hh} xc: $seed +noise Random \
	-virtual-pixel tile \
	$setcspace -colorspace gray -contrast-stretch 0% \
    $tmp0


# process image with noise image as displacement map
if [ "$im_version" -ge "07000000" ]; then
# need to convert grayscale $tmp0 to color in IM 7
	convert $tmp0 -colorspace sRGB\
		-channel R -evaluate sine $density \
		-channel G -evaluate cosine $density \
		-channel RG -separate $tmpA1 -insert 0 \
		$setcspace -define compose:args=${spread}x${spread} \
		-compose displace -composite "$outfile"
elif [ "$im_version" -ge "06050304" ]; then
	convert $tmp0 \
		-channel R -evaluate sine $density \
		-channel G -evaluate cosine $density \
		-channel RG -separate $tmpA1 -insert 0 \
		$setcspace -define compose:args=${spread}x${spread} \
		-compose displace -composite "$outfile"
elif [ "$im_version" -ge "06040805" ]; then
	# create multi-image miff (sine tmpA1 cosine), then pass to composite -displace
	convert $tmp0 \
		-channel R -evaluate sine $density \
		-channel G -evaluate cosine $density \
		-channel RG -separate $tmpA1 -insert 0 miff:- | \
		convert - $setcspace -define compose:args=${spread}x${spread} -compose displace -composite "$outfile"

# old last lines
#		-channel RG -separate $tmpA1 +swap miff:- | \
#		composite - -displace ${spread}x${spread} "$outfile"

elif [ "$im_version" -ge "06040400" ]; then
	# use -fx to create multi-image miff (sine tmpA1 cosine), then pass to composite -displace
	convert $tmp0 \
		-channel R -monitor -fx "0.5+0.5*sin(2*pi*u*$density)" \
		-channel G -monitor -fx "0.5+0.5*cos(2*pi*u*$density)" \
		-channel RG -separate $tmpA1 -insert 0 miff:- | \
		convert - $setcspace -define compose:args=${spread}x${spread} -compose displace -composite "$outfile"

# old last lines
#		-channel RG -separate $tmpA1 +swap miff:- | \
#		composite - -displace ${spread}x${spread} "$outfile"

else
	convert $tmpA1 $tmp0 -monitor \
		-fx "xx=i+$spread*sin($density*v*2*pi); yy=j+$spread*cos($density*v*2*pi); u.p{xx,yy}" \
		"$outfile"
fi
exit 0
