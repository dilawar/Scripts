#!/bin/bash
#
# Developed by Fred Weinhaus 7/15/2011 .......... 1/10/2015
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
# USAGE: dualtonemap [-c] [-g gamma] [-m mode] [-b bias] [-t thresh] [-s svals] [-h hvals] [-r ramp] infile outfile
# USAGE: dualtonemap [-help]
#
# OPTIONS:
#
# -c                set colorspace of input image from sRGB to RGB before 
#                   processing
# -g     gamma      apply non-linear gamma processing to input image before 
#                   processing. Alternate to -c option. Nominal value is 
#                   gamma=2. Values are floats>0; default=1.
# -m     mode       mode of operation; choices are: shadows, highlights or
#                   both; default=both
# -b     bias       percent shift in mean value used as threshold between
#                   shadows and highlights; positive or negative floats; 
#                   default is no change from mean of image.
# -t     thresh     threshold value between shadows and highlights; 
#                   0<=float<=100; overrides automatic mode of mean of 
#                   image if provided
# -s     svals      blackpoint,whitepoint,gamma values to use in shadows; 
#                   0<=float<=100; svals=0,100,1 is no change; default=0,50,1
# -h     hvals      blackpoint,whitepoint,gamma values to use in highlights; 
#                   0<=float<=100; hvals=0,100,1 is no change; default=50,100,1
# -r     ramp       ramp of transition between shadows and highlights in 
#                   pixels; integer>=0; default=50
#
###
#
# NAME: DUALTONEMAP 
# 
# PURPOSE: Enhances the shadows and/or highlight regions in an image.
# 
# DESCRIPTION: DUALTONEMAP enhances the shadows and/or highlight regions 
# in an image. This is done by adjusting the -level blackpoint,whitepoint% 
# in the shadows and/or highlight regions. This is similar to Photoshop's 
# Shadows/Highlights function.
# 
# 
# OPTIONS: 
# 
# -c ... Set colorspace of input image from sRGB to RGB before processing. 
# This provides an initial non-linear processing. The default=no.
# 
# -g gamma ... GAMMA applies a non-linear gamma processing to input image before 
# processing. Alternate to -c option. Do not use both. Values are floats>0. 
# Nominal value is gamma=2. The default=1 (no non-linear gamma processing).
# 
# -m mode ... MODE of operation that specifies to adjust shadows or highlights 
# or both. Choices are shadows (or s), highlights (or h) or both (or b). The
# default=both
#
# -b bias ... BIAS is the percent shift of the mean value of the input that is 
# is used as the nominal threshold value between shadows and highlights. 
# Values are positive or negative floats. The default=0 indicates no change 
# from the global mean value of all channels of the input image.
# 
# -t thresh ... THRESH is the user specified threshold value. When used, it 
# overrides the automatic value from the (mean + bias) value. Values are 
# floats between 0 and 100. The default is to use automatic value from the 
# (mean + bias).
# 
# -s svals ... SVALS=blackpoint,whitepoint,gamma used by -level in the 
# shadows region. Values of each blackpoint and whitepoint are float 
# percents in the range of 0 to 100 and gamma is a float>0. A value of 
# svals=0,100,1 produces no change. The default=0,50,1.
# 
# -h hvals ... HVALS=blackpoint,whitepoint,gamma used by -level in the 
# highlights region. Values of each blackpoint and whitepoint are float 
# percents in the range of 0 to 100 and gamma is a float>0. A value of 
# hvals=0,100,1 produces no change. The default=50,100,1.
# 
# -r ramp ... RAMP is the transition distance in pixels between the shadows and 
# highlights. Values are integers>=0. The default=50.
#
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
mode="both"			# shadows, highlights, both
bias=0				# bias percent shift of mean value for thresholding shadows and highlights
thresh=""			# threshold percent; if provided skip mean and bias
svals="0,50,1"		# blackpoint,whitepoint values to use in shadows;
hvals="50,100,1"	# blackpoint,whitepoint values to use in highlights;
ramp=50				# ramp the transition in mask between shadows and highligths
cmode="no"			# set colorspace to sRGB
gamma=1				# nominal value is 2

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
elif [ $# -gt 17 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
	while [ $# -gt 0 ]
		do
			# get parameter values
			case "$1" in
		     -help)    # help information
					   echo ""
					   usage2
					   exit 0
					   ;;
				-c)    # set colormode to sRGB
					   cmode="yes" ;;
				-g)    # get gamma
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GAMMA SPECIFICATION ---"
					   checkMinus "$1"
					   gamma=`expr "$1" : '\([.0-9]*\)'`
					   [ "$gamma" = "" ] && errMsg "--- GAMMA=$gamma MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
					   test1=`echo "$gamma <= 0" | bc`
					   [ $test1 -eq 1 ] && errMsg "--- GAMMA=$gamma MUST BE A FLOAT GREATER THAN 0 ---"
					   ;;
				-m)    # get  mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$mode" in 
					   		shadows|s) mode=shadows;;
					   		highlights|h) mode=highlights;;
					   		both|b) mode=both;;
					   		*) errMsg "--- MODE=$mode IS AN INVALID VALUE ---" 
					   	esac
					   ;;
				-b)    # get bias
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID BIAS SPECIFICATION ---"
					   #checkMinus "$1"
					   bias=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$bias" = "" ] && errMsg "--- BIAS=$bias MUST BE A POSITIVE OR NEGATIVE FLOAT ---"
 					   ;;
				-t)    # get thresh
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID THRESH SPECIFICATION ---"
					   checkMinus "$1"
					   thresh=`expr "$1" : '\([.0-9]*\)'`
					   [ "$thresh" = "" ] && errMsg "--- THRESH=$thresh MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
					   test1=`echo "$thresh < 0" | bc`
					   test2=`echo "$thresh > 100" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- THRESH=$thresh MUST BE A FLOAT BETWEEN 0 AND 100 ---"
					   ;;
				-s)    # get svals
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SVALS SPECIFICATION ---"
					   checkMinus "$1"
					   svals=`expr "$1" : '\([,.0-9]*\)'`
					   [ "$svals" = "" ] && errMsg "--- SVALS=$svals MUST BE A COMMA SEPARATED TRIPLET OF NON-NEGATIVE FLOATS ---"
					   ;;
				-h)    # get hvals
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID HVALS SPECIFICATION ---"
					   checkMinus "$1"
					   hvals=`expr "$1" : '\([,.0-9]*\)'`
					   [ "$hvals" = "" ] && errMsg "--- HVALS=$hvals MUST BE A COMMA SEPARATED TRIPLET OF NON-NEGATIVE FLOATS ---"
					   ;;
				-r)    # get ramp
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RAMP SPECIFICATION ---"
					   checkMinus "$1"
					   ramp=`expr "$1" : '\([0-9]*\)'`
					   [ "$ramp" = "" ] && errMsg "--- RAMP=$ramp MUST BE A NON-NEGATIVE INTEGER ---"
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

# create temp files
tmpA1="$dir/dualtonemap_1_$$.mpc"
tmpB1="$dir/dualtonemap_1_$$.cache"
tmpA2="$dir/dualtonemap_2_$$.mpc"
tmpB2="$dir/dualtonemap_2_$$.cache"
tmpA3="$dir/dualtonemap_3_$$.mpc"
tmpB3="$dir/dualtonemap_3_$$.cache"
tmpA4="$dir/dualtonemap_4_$$.mpc"
tmpB4="$dir/dualtonemap_4_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4; exit 1" ERR

computeMean()
	{
	img="$1"
	if [ "$im_version" -ge "06040011" ]
		then 
		mean=`convert $img -format "%[fx:mean]" info:`
	else
		data=`convert $img -verbose info:`
		mean=`echo "$data" | sed -n 's/^.*[Mm]ean:.*[(]\([0-9.]*\).*$/\1/p ' | head -1`
	fi
	meanpct=`convert xc: -format "%[fx:100*$mean]" info:`
	}

# get IM version
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
# The following was determined from various version tests using dualtonemap.
# with IM 6.7.4.10, 6.7.6.10, 6.7.8.7
# Note: added $setcspace2 to handle case of -c
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
if [ "$im_version" -gt "06070707" ]; then
	setcspace2="-set colorspace RGB"
else
	setcspace2=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
	setcspace2=""
fi

# setup cmode
if [ "$cmode" = "yes" -a "$gamma" != "1" ]; then
	errMsg "--- CANNOT SPECIFY -c And -g OPTIONS TOGETHER ---"
fi

# colorspace swapped at IM 6.7.5.5, but not properly fixed until 6.7.6.6
# before swap verbose info reported colorspace=RGB after colorspace=sRGB
if [ "$cmode" = "yes" -a "$im_version" -ge "06070606" ]; then
	preproc1="-set colorspace RGB -colorspace sRGB"
elif [ "$cmode" = "yes" -a "$im_version" -lt "06070606" ]; then
	preproc1="-set colorspace sRGB -colorspace RGB"
else
	preproc1="$setcspace"
fi


# setup gamma
if [ "$gamma" != "1" ]; then
	preproc2="-gamma $gamma"
else
	preproc2=""
fi

if [ "$preproc1" != "" ]; then
	preproc=$preproc1
elif [ "$preproc2" != "" ]; then
	preproc=$preproc2
fi

#echo "preproc=$preproc"

# test input image
convert -quiet "$infile" +repage $preproc -auto-level "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"


# get blackpoints and whitepoints
sbp=`echo "$svals" | cut -d, -f1`
swp=`echo "$svals" | cut -d, -f2`
sg=`echo "$svals" | cut -d, -f3`
hbp=`echo "$hvals" | cut -d, -f1`
hwp=`echo "$hvals" | cut -d, -f2`
hg=`echo "$hvals" | cut -d, -f3`
[ "$sg" = "" ] && sg=1
[ "$hg" = "" ] && hg=1
#echo "sbp=$sbp; swp=$swp; sg=$sg; hbp=$hbp; hwp=$hwp; hg=$hg"
test=`convert xc: -format "%[fx:(0<=$sbp<=100)?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- INVALID SHADOW BLACKPOINT ---"
test=`convert xc: -format "%[fx:(0<=$swp<=100)?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- INVALID SHADOW WHITEPOINT ---"
if [ "$mode" = "both" ]; then
	test=`convert xc: -format "%[fx:($sbp<$swp)?1:0]" info:`
	[ $test -eq 0 ] && errMsg "--- SHADOW WHITEPOINT MUST BE LARGER THAN SHADOW BLACKPOINT ---"
fi
test=`convert xc: -format "%[fx:(0<=$hbp<=100)?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- INVALID HIGHLIGHT BLACKPOINT ---"
test=`convert xc: -format "%[fx:(0<=$hwp<=100)?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- INVALID HIGHLIGHT WHITEPOINT ---"
if [ "$mode" = "both" ]; then
	test=`convert xc: -format "%[fx:($sbp<$swp)?1:0]" info:`
	[ $test -eq 0 ] && errMsg "--- HIGHLIGHT WHITEPOINT MUST BE LARGER THAN HIGHLIGHT BLACKPOINT ---"
fi
test=`convert xc: -format "%[fx:$sg>0?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- SHADOW GAMMA MUST BE A POSITIVE FLOAT ---"
test=`convert xc: -format "%[fx:$hg>0?1:0]" info:`
[ $test -eq 0 ] && errMsg "--- HIGHLIGHT GAMMA MUST BE A POSITIVE FLOAT ---"

# find threshold
if [ "$thresh" = "" ]; then
	computeMean $tmpA1
	thresh=`convert $tmpA1 -format "%[fx:100*($mean+$bias/100)]" info:`
	echo ""
	echo "mean=$meanpct%"
	echo "threshold=$thresh%"
	echo ""
fi

# set up for blurring transition
if [ "$ramp" = "0" ]; then
	blurring=""
else
	blurring="-blur ${ramp}x65000"
fi

# create mask to separate image at midpoint
convert $tmpA1 $setcspace2 -threshold ${thresh}% $blurring $tmpA2


# process image
if [ "$mode" = "both" ]; then
	convert $tmpA1 -level ${sbp},${swp},${sg}% $tmpA3
	convert $tmpA1 -level ${hbp},${hwp},${hg}% $tmpA4
	convert $tmpA3 $tmpA4 $tmpA2 -compose over -composite "$outfile"
elif [ "$mode" = "shadows" ]; then
	convert $tmpA1 -level ${sbp},${swp},${sg}% $tmpA3
	convert $tmpA3 $tmpA1 $tmpA2 $setcspace2 -compose over -composite "$outfile"
elif [ "$mode" = "highlights" ]; then
	convert $tmpA1 -level ${hbp},${hwp},${hg}% $tmpA4
	convert $tmpA1 $tmpA4 $tmpA2 -compose over -composite "$outfile"
fi

exit 0



