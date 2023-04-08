#!/bin/bash
#
# Developed by Fred Weinhaus 10/05/2007 .......... revised 4/25/2015
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
# USAGE: sharp [-m method] [-f factor] infile outfile
# USAGE: sharp [-st] infile
# USAGE: sharp [-h or -help]
#
# OPTIONS:
#
# -m              method         method=0 is edge extract; method=1 is sharpen
#                                default=1
# -f              factor         sharpening/edge extraction gain factor; 
#                                factor>=0; float; default=2                              
# -st                            get image statistics
# -h or -help                    get help
#
###
#
# NAME: SHARP 
# 
# PURPOSE: To sharpen an image or extract edges adaptively. 
# 
# DESCRIPTION: SHARP is an adaptive technique to either sharpen an image 
# or extract edges. It applies an adaptive gain factor based upon the image's 
# local standard deviation to a high pass filtered version of the image and adds 
# that to a low pass filtered version of the image.
#
# The adaptive formula R = method*M + G*H. Here R is the resulting image. 
# M is the low pass filtered image which is the local mean image generated  
# by applying a 3x3 average convolution to the input image, I. H = (I - M)  
# is the high pass filtered image. G is the gain image, which is computed as 
# G = (factor*std)/(S + (factor*std/mg)). Here std is the image's global  
# standard deviation, mg is a maximum gain constant and S is the image's  
# local standard deviation in the 3x3 neighborhood.
# 
# Arguments: 
#
# -h or -help    ---  displays help information. 
# 
# -m method method is either 0 or 1. A value of 0 for method indicates edge 
# extraction and value of 1 for method indicates sharpening. The default=1.
# 
# -f factor factor is the sharpening/edge extraction gain factor. It is a multiplier 
# to the image's actual standard deviation. The value for factor must be greater than 
# or equal to 0. A value of about 0.5 leaves the image nearly unchanged. A smaller   
# value blurs the image or extract more edges. A larger value sharpens the image 
# or extracts fewer edges. This transition value is not exact and is likely 
# image statistics dependent as the result is a mix of the low pass filtered 
# image (not the original image) and the adaptive high pass filtered image. 
# Factor is floating point number. The default=2.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#
# set default value for method and factor and maxgain
method=1
factor=2
stats="false"
mg=5
#
# set directory for temporary files
dir="."    # suggestions are dir="." or dir="/tmp"
#
#
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
#
# function to report error messages
errMsg()
	{
	echo ""
	echo $1
	echo ""
	usage1
	exit 1
	}
#
# function to test for minus at start of value of second part of option 1 or 2
checkMinus()
	{
	test=`echo "$1" | grep -c '^-.*$'`   # returns 1 if match; 0 otherwise
    [ $test -eq 1 ] && errMsg "$errorMsg"
	}
#
#
# test for correct number of arguments and get values
if [ $# -eq 0 ]
	then
	# help information
	echo ""
	usage2
	exit 0
elif [ $# -eq 3 -o $# -eq 5 -o $# -gt 6 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
	while [ $# -gt 0 ]
		do
		# get parameters
		case "$1" in
	  -h|-help)    # help information
				   echo ""
				   usage2
				   ;;
		   -st)    # image statistics
				   stats="true"
				   ;;
			-m)    # method
				   shift  # to get the next parameter - method
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID METHOD SPECIFICATION ---"
				   checkMinus "$1"
				   method="$1"
				   [ $method -ne 0 -a $method -ne 1 ] && errMsg "--- METHOD=$method MUST BE EITHER 0 OR 1 ---"
				   ;;
			-f)    # factor
				   shift  # to get the next parameter - factor
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID FACTOR SPECIFICATION ---"
				   checkMinus "$1"
				   factor=`expr "$1" : '\([.0-9]*\)'`
				   [ "$factor" = "" ] && errMsg "--- FACTOR=$factor MUST BE A NON-NEGATIVE FLOATING POINT VALUE (with no sign) ---"
				   factortest=`echo "$factor < 0" | bc`
				   [ $factortest -eq 1 ] && errMsg "--- FACTOR=$factor MUST BE A NON-NEGATIVE FLOATING POINT VALUE ---"
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
	[ "$stats" = "false" ] && outfile="$2"
fi


# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"

# test that outfile provided
if [ "$stats" = "false" ]
	then
	[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"
fi


# setup temporary images and auto delete upon exit
# use mpc/cache to hold input image temporarily in memory
tmpA="$dir/sharp_$$.mpc"
tmpB="$dir/sharp_$$.cache"
tmpM="$dir/sharp_mean_$$.miff"
tmpS="$dir/sharp_std_$$.miff"
trap "rm -f $tmpA $tmpB $tmpM $tmpS;" 0
trap "rm -f $tmpA $tmpB $tmpM $tmpS; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB $tmpM $tmpS; exit 1" ERR


# function to get min, max, mean, std from Brightness channel (or Graylevel image)
function imagestats
	{
	data=`convert \( $1 -colorspace Gray \) -verbose info:`
	min=`echo "$data" | sed -n 's/^.*[Mm]in:.*[(]\([0-9.]*\).*$/\1/p ' | head -1`
	[ "$min" = "" ] && errMsg "--- MIN NOT FOUND --- "
	max=`echo "$data" | sed -n 's/^.*[Mm]ax:.*[(]\([0-9.]*\).*$/\1/p ' | head -1`
	[ "$max" = "" ] && errMsg "--- MAX NOT FOUND --- "
	mean=`echo "$data" | sed -n 's/^.*[Mm]ean:.*[(]\([0-9.]*\).*$/\1/p ' | head -1`
	[ "$mean" = "" ] && errMsg "--- MEAN NOT FOUND --- "
	std=`echo "$data" | sed -n 's/^.*[Ss]tandard.*[(]\([0-9.]*\).*$/\1/p ' | head -1`
	[ "$std" = "" ] && errMsg "--- STD NOT FOUND --- "
	#
	# express as percent
	# Note: divide by 1 needed to force bc to honor scale=1; otherwise get 6 digits after decimal point
	min=`echo "scale=1; $min * 100 / 1" | bc`
	max=`echo "scale=1; $max * 100 / 1" | bc`
	mean=`echo "scale=1; $mean * 100 / 1" | bc`
	std=`echo "scale=1; $std * 100 / 1" | bc`
	}


ave="1,1,1,1,1,1,1,1,1"


if [ "$stats" = "true" ]
	then
	imagestats $infile
	echo ""
	echo "Min (0-100) = $min"
	echo "Max (0-100) = $max"
	echo "Mean (0-100) = $mean"
	echo "Std (0-...) = $std"
	echo ""
	exit 0
else
	echo ""
	echo "Please Wait - This May Take Some Time"
	echo ""
	# convert $infile to mpc format
	if convert -quiet "$infile" +repage "$tmpA"
		then
		: 'do nothing special'
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
	fi
fi
	
# get image stats
imagestats $tmpA	

# get mean
convert $tmpA -define convolve:scale=1 -convolve "$ave" $tmpM

# get std = sqrt( ave(x^2) - ave(x)^2 )
# -evaluate pow 0.5 is equivalent to sqrt
convert \( $tmpA $tmpA -compose multiply -composite -define convolve:scale=1 -convolve "$ave" \) \
	\( $tmpM $tmpM -compose multiply -composite \) +swap \
	-compose minus -composite -evaluate pow 0.5 $tmpS
	
# compute gain and final image
[ `echo "scale=6; $factor < .001" | bc` -eq 1 ] && factor=.001
dstd=`echo "scale=5; $factor * $std / 100" | bc`
dsdmg=`echo "scale=5; $dstd / $mg" | bc`
gain="gn=($dstd)/(u[2]+($dsdmg));"
if [ $method -eq 0 ]
	then
	convert $tmpA $tmpM $tmpS -monitor -fx "$gain (gn*(u-v))" -normalize "$outfile"
elif [ $method -eq 1 ]
	then
	convert $tmpA $tmpM $tmpS -monitor -fx "$gain (v+(gn*(u-v)))" "$outfile"
else
	errMsg "--- INVALID METHOD ---"
fi

exit 0