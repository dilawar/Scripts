#!/bin/bash
#
# Developed by Fred Weinhaus 8/17/2009 .......... revised 7/28/2015
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
# USAGE: spectrum [-m] [-s scale] [-g] infile outfile
# USAGE: spectrum [-h or -help]
#
# OPTIONS:
#
# -m                  generate magnitude from Fourier Transform 
#                     of the input
# -s      scale       scaling constant; integer greater than zero;
#                     default will be automatically computed.
# -g                  convert spectrum to grayscale
#
###
#
# NAME: SPECTRUM 
# 
# PURPOSE: To compute the spectrum image from the magnitude of the Fourier 
# Transform of an image.
# 
# DESCRIPTION: SPECTRUM computes a spectrum image from the magnitude image
# generated from the Fourier Transform of an image. The spectrum is basically 
# a scaled log of the magnitude image. See -evaluation log. The log and 
# scaling are provided to emphasize the lower values in the magnitude image 
# relative to the larger values. The input may be the magnitude of the Fourier 
# Transform, if already available. If the -m option is provided, it means that 
# the magnitude of the Fourier Transforms should be computed from the spatial 
# domain input image.
# 
# 
# OPTIONS: 
# 
# -m ... Compute magnitude of the Fourier Transform of the spatial domain 
# image that is the input. The default, when left off, is to assume that 
# the input image is already the magnitude of the Fourier Transform.
# 
# -s scale ... SCALE is the scaling constant used with the log. Values are 
# integers greater than zero. Values are convert to 10^scale for use with 
# log. Typical values range from 2 to 5. Larger values will bring out more 
# low amplitude detail. But too large a value will bring out excessive noise. 
# The default is to compute a scaling value automatically.
# 
# -g ... Convert magnitude input to grayscale to produce a grayscale spectrum.
# 
# As an final step, pure black (graylevel=0) is converted to graylevel 1 on 
# a scale of 0 to 255. This is done so that after the spectrum is masked with 
# pure black to eliminate noise or apply a filter, the result can be thresholded 
# at value=0 (see -threshold 0) to convert it to a binary mask image.
# 
# REQUIREMENTS: IM IM 6.3.9-1 or higher due to the use of -format "%[min]" and
# "%[max]". Also requires FFTW delegate library to compute the Fourier 
# Transform. Q8 IM compilation is not generally recommended and may not carry 
# enough precision for the Fourier Transform.
# 
# See http://www.fmwconcepts.com/imagemagick/fourier_transforms/fourier.html 
# for more details about the Fourier Transform with ImageMagick.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
scale=""
makegray="no"
makemag="no"

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
elif [ $# -gt 6 ]
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
				-s)    # get scale
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SCALE SPECIFICATION ---"
					   checkMinus "$1"
					   scale=`expr "$1" : '\([0-9]*\)'`
					   [ "$scale" = "" ] && errMsg "SCALE=$scale MUST BE A NON-NEGATIVE INTEGER"
		   			   scaletest=`echo "$scale <= 0" | bc`
					   [ $scaletest -eq 1 ] && errMsg "--- SCALE=$scale MUST BE GREATER THAN 0 ---"
					   ;;
				-g)    # convet to grayscale
					   makegray="yes"
					   ;;
				-m)    # compute magnitude
					   makemag="yes"
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

tmpA="$dir/spectrum_$$.mpc"
tmpB="$dir/spectrum_$$.cache"
trap "rm -f $tmpA $tmpB;" 0
trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB; exit 1" ERR

# read the input image into the temp file and test validity.
if [ "$makemag" = "yes" ]; then
	makemag="-fft -delete 1"
else
	makemag=""
fi

# get IM version
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
# The following was determined from various version tests using spectrum.
# Note: tested with 6.7.4.10, 6.7.6.10, 6.7.8.6
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
fi

# set up conversion to grayscale
if [ "$makegray" = "yes" ]; then
	makegray="$setcspace -colorspace gray"
else
	makegray=""
fi


convert -quiet "$infile" $makegray $makemag +repage "$tmpA" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"


# function to get linear stretch parameters
levelParms()
	{
	img="$1"
	im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`
	if [ "$im_version" -ge "06030901" ]
		then 
		min=`convert $img -format "%[min]" info:`
		max=`convert $img -format "%[max]" info:`
	else
		errMsg "--- REQUIRES IM 6.3.9-1 OR HIGHER ---"
	fi
	}


# process image
if [ "$scale" != "" ]; then
	scale=`convert xc: -format "%[fx:int(10^$scale) - 1]" info:`
	levelParms $tmpA
	convert $tmpA -level ${min},${max} \
		-evaluate log $scale -fill "rgb(1,1,1)" -opaque black "$outfile"
else
	levelParms $tmpA
	convert $tmpA -level ${min},${max} $tmpA
	scale=`convert $tmpA -format "%[fx:floor(exp(log(mean)/log(0.5)))]" info:`
	convert $tmpA -evaluate log $scale -fill "rgb(1,1,1)" -opaque black "$outfile"
echo ""
echo "scale=$scale"
echo ""
fi

exit 0
