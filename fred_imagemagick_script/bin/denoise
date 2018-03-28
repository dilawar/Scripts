#!/bin/bash
#
# Developed by Fred Weinhaus 4/22/2010 .......... revised 4/29/2015
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
# USAGE: denoise [-m method] [-f filter] [-s subsection] [-n nstd] 
# [-u unsharp] [-g gain] infile outfile
# USAGE: denoise [-h or -help]
# 
# OPTIONS:
# 
# -m      method            method of filtering; mean or median; default=mean
# -f      filter            filter size; float>0; default=2
# -s      subsection        subsection of image to compute noise standard deviation;
#                           WIDTHxHEIGHT+XOFF+YOFF; default is no subsection
#                           and get noise standard deviation from nstd argument
# -n      nstd              noise standard deviation estimate expressed as 
#                           percentage of std value in range 0 to 1; 
#                           0<=float<=100; default is estimated automatically
# -u      unsharp           unsharp masking sigma to apply after filtering;
#                           float>=0; default is no unsharp masking
# -g      gain              unsharp masking gain to apply after filtering;
#                           float>=0; default=1
# 
###
# 
# NAME: DENOISE 
# 
# PURPOSE: To reduces the noise in an image.
# 
# DESCRIPTION: DENOISE reduces the noise in an image. It uses the formula:
# input + gain*(input - mean), where mean is a local mean computed in the 
# neighborhood of each pixel and gain=max(0,(std-nstd)/std), where std is the 
# local standard deviation computed in the neighborhood of each pixel. This is 
# an implementation of the Lee Filter. See Lee, J.S., 1981. Speckle Analysis 
# and Smoothing of Synthetic Aperture Radar Images. Computer Graphics and Image
# Processing, Vol. 17:24-32.
# 
# 
# ARGUMENTS: 
# 
# -m method ... METHOD of filtering. Choices are: mean or median. Default=mean.
# 
# -f filter ... FILTER size. Values are floats>0. The default=2.
# 
# -s subsection ... Subsection is a rectangular homogeneous region where the 
# noise standard deviation will be computed if provided. Otherwise, nstd, 
# must be provided. The values are in the form of the usual IM subsection 
# expressed as WIDTHxHEIGHT+XOFF+YOFF. The default is no subsection provided.
# 
# -n nstd ... NSTD is an estimate of the noise standard deviation in the image
# expressed as a percentage (100 times std values in the range of 0 to 1). Thus 
# values for nstd are 0<=float<=100. The default is estimated automatically.
# 
# -u unsharp ... UNSHARP masking sigma to apply as a post processing step to 
# sharpen the image. Values are floats>=0. The default=0 for no unsharp masking.
# 
# -g gain ... UNSHARP masking gain to apply as a post processing step to 
# sharpen the image. Values are floats>=0. The default=1.
# 
# References:
# Lee, J.S., 1981. Speckle Analysis and Smoothing of Synthetic Aperture Radar Images. Computer Graphics and Image Processing, Vol. 17:24-32.
# http://www.isprs.org/proceedings/XXXVI/1-W41/makaleler/Rajabi_Specle_Noise.pdf
# J. Immerkaer, "Fast Noise Variance Estimation", Comput. Vis. Image Understand., vol. 64, pp. 300-302, Sep. 1996
# http://www.iaeng.org/IJCS/issues_v37/issue_1/IJCS_37_1_09.pdf
# 
# NOTE: This script may be a bit slow due to the use of -fx for version of IM 
# prior to 6.8.0.5.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
method="mean"           # mean or median
filter=2				# filter size; float>0
subsection=""			# subsection for computing noise std
nstd=""					# noise standard deviation percent; float
unsharp=0				# unsharp filtering sigma; float>0
gain=0					# unsharp filtering gain; float>0

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
elif [ $# -gt 16 ]
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
		 		-m)    # method
					   shift  # to get the next parameter - mode
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID METHOD SPECIFICATION ---"
					   checkMinus "$1"
					   # test mode values
					   method="$1"
					   [ "$method" != "mean" -a "$method" != "median" ] && errMsg "--- METHOD=$method IS NOT A VALID VALUE ---"
					   ;;
				-f)    # get filter
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FILTER SPECIFICATION ---"
					   checkMinus "$1"
					   filter=`expr "$1" : '\([.0-9]*\)'`
					   [ "$filter" = "" ] && errMsg "--- FILTER=$filter MUST BE A NON-NEGATIVE FLOAT ---"
					   test=`echo "$filter <= 0" | bc`
					   [ $test -eq 1 ] && errMsg "--- FILTER=$filter MUST BE A POSITIVE FLOAT ---"
					   ;;
				-s)    # get subsection
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SUBSECTION SPECIFICATION ---"
					   checkMinus "$1"
					   subsection=`expr "$1" : '\([x+0-9]*\)'`
					   [ "$subsection" = "" ] && errMsg "--- SUBSECTION=$subsection MUST BE INTEGERS IN THE FORM OF WIDTHxHEIGHT+XOFF+YOFF ---"
					   ;;
				-n)    # get nstd
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NSTD SPECIFICATION ---"
					   checkMinus "$1"
					   nstd=`expr "$1" : '\([.0-9]*\)'`
					   [ "$nstd" = "" ] && errMsg "--- NSTD=$nstd MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
					   test1=`echo "$nstd < 0" | bc`
					   test2=`echo "$nstd > 100" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- NSTD=$nstd MUST BE A FLOAT BETWEEN 0 AND 100 ---"
					   ;;
				-u)    # get unsharp
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID UNSHARP SPECIFICATION ---"
					   checkMinus "$1"
					   unsharp=`expr "$1" : '\([.0-9]*\)'`
					   [ "$unsharp" = "" ] && errMsg "--- UNSHARP=$unsharp MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-g)    # get gain
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GAIN SPECIFICATION ---"
					   checkMinus "$1"
					   gain=`expr "$1" : '\([.0-9]*\)'`
					   [ "$gain" = "" ] && errMsg "--- GAIN=$gain MUST BE A NON-NEGATIVE FLOAT ---"
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


# setup temporary images and auto delete upon exit
tmpI1="$dir/denoise_I_$$.mpc"
tmpI2="$dir/denoise_I_$$.cache"
tmpM1="$dir/denoise_M_$$.mpc"
tmpM2="$dir/denoise_M_$$.cache"
tmpS1="$dir/denoise_S_$$.mpc"
tmpS2="$dir/denoise_S_$$.cache"
tmpG1="$dir/denoise_G_$$.mpc"
tmpG2="$dir/denoise_G_$$.cache"
trap "rm -f $tmpI1 $tmpI2 $tmpM1 $tmpM2 $tmpS1 $tmpS2 $tmpG1 $tmpG2;" 0
trap "rm -f $tmpI1 $tmpI2 $tmpM1 $tmpM2 $tmpS1 $tmpS2 $tmpG1 $tmpG2; exit 1" 1 2 3 15
trap "rm -f $tmpI1 $tmpI2 $tmpM1 $tmpM2 $tmpS1 $tmpS2 $tmpG1 $tmpG2; exit 1" ERR

# read the input image and filter image into the temp files and test validity.
convert -quiet "$infile" +repage "$tmpI1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"

# get im version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# set up for biasing
if [ "$im_version" -ge "07000000" ]; then
	biasing="-define convolve:bias=50%"
else
	biasing="-bias 50%"
fi


# estimate noise std from nearly constant region
if [ "$nstd" = "" -a "$subsection" = "" ]; then
	nstd=`convert $tmpI1 -define convolve:scale='!' $biasing \
		-morphology Convolve Laplacian:3 -solarize 50% -level 0x50% -negate \
		-format "%[fx:2*(mean/standard_deviation)*sqrt(pi/2)]" info:`
elif [ "$nstd" = "" -a "$subsection" != "" ]; then	
	nstd=`convert $tmpI1[$subsection] -format "%[fx:100*standard_deviation]" info:`
fi

echo ""
echo "nstd=$nstd"
echo ""



#correct for change in median at IM 6.6.8-6 from radius to widthxheight
#and from -median to -statistic median
if [ "$im_version" -ge "06060806" -a "$method" = "median" ]; then
	filter=`convert xc: -format "%[fx:2*$filter+1]" info:`
	proc="-statistic median"
else
	proc="-median"
fi

# set up for gamma or evaluate pow
# don't want gamma to change meta value of gamma from 0.4545, so use -evaluate pow
if [ "$im_version" -lt "06040109" ]; then
	sqrt="-gamma 2"
else
	sqrt="-evaluate pow 0.5"
fi

#create mean (or median) and std
if [ "$method" = "mean" ]; then
	convert $tmpI1 -blur 0x$filter $tmpM1
elif [ "$method" = "median" ]; then
	convert $tmpI1 $proc $filter $tmpM1
fi
convert $tmpI1 \( -clone 0 -blur 0x$filter \) \
	\( -clone 0 -clone 0 -compose multiply -composite -blur 0x$filter \) \
	\( -clone 1 -clone 1 -compose multiply -composite \) \
	-delete 0,1 +swap -compose minus -composite $sqrt $tmpS1

# create gain image
convert $tmpS1 \( +clone -evaluate subtract ${nstd}% \) \
	-compose divide -composite -evaluate max 0 $tmpG1


# set up for unsharp masking
if [ "$unsharp" = "0" ]; then
	unsharpen=""
else
	unsharpen="-unsharp 0x${unsharp}+${gain}"
fi

# denoise
: '
# slower
convert $tmpI1 $tmpM1 $tmpG1 \
	-fx "(u[0]-u[1])*u[2]+u[1]" $unsharpen "$outfile"
'

if [ "$im_version" -lt "06080005" ]; then
	proc2="-monitor -fx 'u[0]-u[1]+u[2]' +monitor $unsharpen "
else
	proc2="-poly '1,1 -1,1 1,1' $unsharpen "
fi

# faster
eval 'convert $tmpI1 $tmpM1 $tmpG1 \
	\( -clone 0 -clone 2 -compose multiply -composite \) \
	\( -clone 1 -clone 2 -compose multiply -composite \) \
	-delete 0,2 -swap 0,1 -swap 1,2 \
	'"$proc2"' "$outfile"'

exit 0
