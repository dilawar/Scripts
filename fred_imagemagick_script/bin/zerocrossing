#!/bin/bash
#
# Developed by Fred Weinhaus 3/24/2014 .......... revised  4/29/2015
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
# USAGE: zerocrossing [-w width ] [-b brightness] [-s smoothing] [-g] infile outfile
# USAGE: zerocrossing [-h or -help]
#
# OPTIONS:
#
# -e     edge        edge type; choices are sobel (s) or morphologic (m); 
#                    default=sobel
# -l     laplace     laplacian type; choices are: 0, 2 and 5; default=0
# -a     amplify     zero crossing percent amplification; integer>=0; 
#                    default=100 (no amplification)
# -s     size        2x morphologic disk size; values are integer>=2; 
#                    default=4 (values will be divided by 2 later)
# -g                 convert image to grayscale before getting edges
# 
###
#
# NAME: ZEROCROSSING 
# 
# PURPOSE: To apply a zero crossing edge detector to image.
# 
# DESCRIPTION: ZEROCROSSING applies a zero crossing edge detector to image 
# in order to thin the edges. The user may select either sobel or morphologic 
# for the basic type of edge detection as well as the type of laplacian used 
# for detecting the zero crossings.
# 
# OPTIONS: 
#
# -e edge ... EDGE type. The choices are sobel (s) or morphologic (m). The 
# default=sobel.
#
# -l laplace ... LAPLACIAN type. The choices are: 0, 2 and 5. The default=0.
# 
# -a amplify ... AMPLIFICATION is the zero crossing percent amplification. 
# Values are integer>=0. The default=100 (no amplification).
# 
# -s size ... SIZE is twice the morphologic disk size. Values are integer>=2.  
# The default=4 (values will be divided by 2 later).
#
# -g ... converts the input image to GRAYSCALE before extracting edges.
# 
# REQUIREMENTS: IM 6.5.9.3 is required for the use of -morphology edges. IM 
# 6.6.1.8 is required for the use of sobel edges.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
edge="sobel"  		# sobel, morphologic
laplace="0"			# laplacian type: 0,2,5
amplify=100			# zerocrossing amplification
size=4			    # morphology 2x disk size: integer>=2; default=4
grayscale="no"		# convert to grayscale

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
				-e)    # get  edge
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID EDGE SPECIFICATION ---"
					   checkMinus "$1"
					   edge=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$edge" in 
					   		sobel|s) edge="sobel" ;;
					   		morphologic|m) edge="morphologic" ;;
					   		*) errMsg "--- EDGE=$edge IS AN INVALID VALUE ---" ;;
					   	esac
					   ;;
				-l)    # get  laplace
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID LAPLACE SPECIFICATION ---"
					   checkMinus "$1"
					   laplace=`expr "$1" : '\([025]\)'`
					   [ "$laplace" = "" ] && errMsg "--- LAPLACE=$laplace MUST BE EITHER 0, 2 OR 5 ---"
					   ;;
				-a)    # get amplify
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMPLIFY SPECIFICATION ---"
					   checkMinus "$1"
					   amplify=`expr "$1" : '\([0-9]*\)'`
					   [ "$amplify" = "" ] && errMsg "--- AMPLIFY=$amplify MUST BE AN INTEGER ---"
					   ;;
				-a)    # get amplify
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMPLIFY SPECIFICATION ---"
					   checkMinus "$1"
					   amplify=`expr "$1" : '\([0-9]*\)'`
					   [ "$amplify" = "" ] && errMsg "--- AMPLIFY=$amplify MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-s)    # get size
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SIZE SPECIFICATION ---"
					   checkMinus "$1"
					   size=`expr "$1" : '\([0-9]*\)'`
					   [ "$size" = "" ] && errMsg "SIZE=$size MUST BE A NON-NEGATIVE INTEGER ---"
					   test1=`echo "$dark < 2" | bc`
					   [ $test1 -eq 1 ] && errMsg "--- SIZE=$size MUST BE AN INTEGER GREATER THAN 1 ---"
					   ;;
				-g)    # set grayscale
					   grayscale="yes"
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

# setup temporary images
tmpA1="$dir/zerocrossing_1_$$.mpc"
tmpB1="$dir/zerocrossing_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1;" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1; exit 1" ERR


# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

# get im version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# set up grayscaling
if [ "$grayscale" = "yes" ]; then
	gproc="-colorspace gray"
else
	gproc=""
fi

# set up size --- divide by 2
size=`convert xc: -format "%[fx:$size/2]" info:`


# setup edge processing
if [ "$edge" = "sobel" ]; then
	eproc="-define convolve:scale=100% -define morphology:compose=Lighten -morphology Convolve Sobel:>"
elif [ "$edge" = "morphologic" ]; then
	eproc="-morphology edge disk:$size -auto-level "
fi

# set up for biasing
if [ "$im_version" -ge "07000000" ]; then
	biasing="-define convolve:bias=50%"
else
	biasing="-bias 50%"
fi

# create zero crossing
# line1: convert to grayscale if desired
# line2: create sobel or morphologic edges
# line3: create laplacian
# lines4-7: apply gradient magnitude to laplacian to get zero crossings
# note: cannot just threshold laplacian at mid gray, since background flat areas are also gray
# line 8: delete tmps and multiply zero crossing laplacian by edge (sobel or morphologic) image
convert $tmpA1 $gproc \
	\( -clone 0 $eproc \) \
	\( -clone 0 $biasing -morphology Convolve Laplacian:$laplace -clamp \) \
		\( -clone 2 -define convolve:scale="$amplify%" -morphology convolve sobel:0 -clamp -evaluate pow 2 \) \
		\( -clone 2 -define convolve:scale="$amplify%" -morphology convolve sobel:90 -clamp -evaluate pow 2 \) \
		-delete 0,2 \
		\( -clone 1 -clone 2 -compose plus -composite -evaluate pow 0.5 -negate -threshold 0 -negate \) \
	-delete 1,2 -compose multiply -composite \
	"$outfile"


exit 0
