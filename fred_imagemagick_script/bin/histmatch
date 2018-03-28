#!/bin/bash
#
# Developed by Fred Weinhaus 6/25/2009 .......... revised 4/25/2015
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
# USAGE: histmatch [-c colormode] [-L] infile1 infile2 outfile
# USAGE: histmatch [-h or -help]
#
# OPTIONS:
#
# -c      colormode       colorspace/channel to use to compute the image 
#                         histograms; choices are: gray, intensity, luminance,
#                         lightness, brightness, average and rgb; default=gray
# -L                      save the lookup table image; if saved, it will 
#                         be named histmatch_lut.png
# 
# infile2 will be modified to match infile1
# 
###
#
# NAME: HISTMATCH 
# 
# PURPOSE: To modify one image to try to match its histogram and optionally
# saturation to that of another image.
# 
# DESCRIPTION: HISTMATCH modifies one image to try to match its histogram 
# and optionally saturation to that of another image. The second 
# image will be modified to match the first image. Saturation changes 
# only apply to color images, not grayscale ones. The choice of colormode 
# determines what colorspace or channel will be used to compute the 
# histograms of the two images. A non-linear transformation is then 
# computed from the cumulative histograms of the two images. This 
# non-linear transformation is then applied to each channel of the second image 
# using -clut to generate the output. If colormode=rgb,then each 
# channel will be processed independently.
#
# IMPORTANT: This histogram transformation can only do so much. Once information 
# is lost, it generally cannot be adequately recovered. This would be the case,
# for example, if the second image has too narrow a histogram, such from too 
# little contrast or mostly one color. The histogram can be spread out to 
# increase the dynamic range, but any one bin cannot be broken into multiple 
# bins. Likewise, if the second image has too high a contrast, the histogram 
# will be very wide and much of the data will be contained in the very high 
# and very low bins and little data in the middle bins. Again any one bin 
# cannot be broken into multiple bins, but small bins can be combined.
# 
# OPTIONS: 
# 
# -c colormode ... COLORMODE is the colorspace/channel to use to compute
# the histograms of the two images. The choices are: gray, intensity, luminance, 
# lightness, brightness, average, and rgb. Values of gray and intensity are 
# equivalent. If colormode=rgb, then each channel of the image will be processed 
# independently. The default is gray.
# 
# Gray or Intensity uses statistics from -colorspace Gray.
# Luminance uses statistics from -colorspace Rec709Luma.
# Lightness uses statistics from the lightness channel of -colorspace HSL.
# Brightness uses statistics from the brightness channel of -colorspace HSB.
# Average uses statistics from the first channel of -colorspace OHTA.
# RGB uses statistics independently from each channel of either sRGB or RGB.
# See definitions at: 
# http://www.imagemagick.org/script/command-line-options.php#colorspace
# 
# Note: generally there are only slight differences between the various 
# non-rgb colormode results. Colormode=rgb can cause color balance shifts.
# and odd colors may creep in.
# 
# -s satmode ... SATMODE is the choice of saturation colorspace to use 
# when modifying the saturation. Choices are: HSB and HSL. The default 
# is to make no change in saturation. If used, I recommend HSB. This 
# argument is only applicable when both images are color (not grayscale).
# 
# -L ... Save the lookup table image. If saved, it will be named 
# histmatch_lut.png
# 
# REQUIRES: IM version 6.4.5-1 or higher if modifying saturation 
# due to the use of -set option:modulate:colorspace. Otherwise, requires 
# IM version 6.3.9-2 due to the use of -format "%[colorspace]". Also, 
# requires NetPBM PGM. See http://netpbm.sourceforge.net/
# 
# NOTE: Thanks to Anthony Thyssen for the suggested changes from shell 
# calculations for the histograms to the use of AWK. This has produced 
# a 10x performance speed up.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
colormode="gray"	# various choices: gray, luminance ... and rgb
maxval=65535		# maxval for NetPBM
savelut="no"        # save the lut

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

#function to get totpixels in image and set normalization factor
totPix()
	{
	img="$1"
	totpix=`convert $img -ping -format "%[fx:w*h]" info:`
	}

# function to get cumulative histogram
cumHistogram()
	{
	img="$1"
	totpix="$2"
	# convert: generates unsorted IM text histogram (using first channel)
	# awk: generates cumulative histogram
	ccountArr=(`convert $img -depth 8 -format "%c" -define histogram:unique-colors=true histogram:info:- |\
		tr -cs '0-9\012' ' ' |\
		awk -v totpix="$totpix" '# AWK to generate a cumulative histogram
			{ bin[int($2)] += $1 }
			END { for (i=0;i<256;i++) {cum += bin[i]/totpix; print cum; } } '`)
#	echo ${ccountArr[*]}
#	echo ${#ccountArr[*]}
	}

genLutImage()
	{
	img1="$1"
	img2="$2"

	# process single channel images
		totPix "$img1"
		totpix1=$totpix
		cumHistogram "$img1" "$totpix1"
		ccountArr1=(${ccountArr[*]})
		totPix "$img2"
		totpix2=$totpix
		cumHistogram "$img2" "$totpix2"
		ccountArr2=(${ccountArr[*]})
		# for each possible bin graylevel (0 to 255) of cc2 starting at 0
		# get count from cumulate histogram cc2 at that bin, then
		# increment along bin graylevels of histogram cc1 until that count exceeds that of cc2
		# find the bin graylevel in cc1 and use that as the output value of the lut transformation 
		# where the cc2 bin graylevel is the input value of the lut transformation
		# repeat for the next cc2 bin, but starting at graylevel where left off from previous.
		# as cumulative histograms never decrease, you don't have to start at graylevel 0 each time
		lutlist=$(for ((i=0; i<256; i++)); do
			echo "$i ${ccountArr1[$i]} ${ccountArr2[$i]}"
			done |\
			awk -v maxval="$maxval" '# AWK to generate transformation lut
				BEGIN { i=0; } { cc1[$1]=$2; cc2[$1]=$3; } 
				END { for ( j=0;j<256;j++ ) 
					{ while ( i != 255 && cc1[i] <= cc2[j] ) 
						{ i++ } lut = maxval*i/255; print int(lut+0.5); } }')
		echo "P2 256 1 $maxval $lutlist" | convert - -scale 256x1\! $tmpL
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
				-c)    # get  colormode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COLORMODE SPECIFICATION ---"
					   checkMinus "$1"
					   colormode=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$colormode" in 
					   		gray) ;;
					   		intensity) ;;
					   		luminance) ;;
					   		lightness) ;;
					   		brightness) ;;
					   		average) ;;
					   		rgb) ;;
					   		*) errMsg "--- COLORMODE=$colormode IS AN INVALID VALUE ---" 
					   	esac
					   ;;
				-L)    # get  savelut
					   savelut="yes"
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
	infile1="$1"
	infile2="$2"
	outfile="$3"
fi

# test that infile1 provided
[ "$infile1" = "" ] && errMsg "NO INPUT FILE 1 SPECIFIED"

# test that infile2 provided
[ "$infile2" = "" ] && errMsg "NO INPUT FILE 2 SPECIFIED"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"


# test for minimum IM version required
# IM 6.4.5.1 or higher to conform to -set option:modulate:colorspace or
# IM 6.3.9.2 or higher to conform to -format "%[colorspace]"
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`
[ "$im_version" -lt "06040501" -a "$satmode" != "" ] && errMsg "--- REQUIRES IM VERSION 6.4.5-1 OR HIGHER ---"
#[ "$im_version" -lt "06030902" -a "$satmode" = "" ] && errMsg "--- REQUIRES IM VERSION 6.3.9-2 OR HIGHER ---"


# set up temporary files
tmpA1="$dir/histmatch_1_$$.mpc"
tmpB1="$dir/histmatch_1_$$.cache"
tmpA2="$dir/histmatch_2_$$.mpc"
tmpB2="$dir/histmatch_2_$$.cache"
tmpIA1="$dir/histmatch_I1_$$.mpc"
tmpIB1="$dir/histmatch_I1_$$.cache"
tmpIA2="$dir/histmatch_I2_$$.mpc"
tmpIB2="$dir/histmatch_I2_$$.cache"
tmpRA1="$dir/histmatch_R1_$$.mpc"
tmpRB1="$dir/histmatch_R1_$$.cache"
tmpRA2="$dir/histmatch_R2_$$.mpc"
tmpRB2="$dir/histmatch_R2_$$.cache"
tmpGA1="$dir/histmatch_G1_$$.mpc"
tmpGB1="$dir/histmatch_G1_$$.cache"
tmpGA2="$dir/histmatch_G2_$$.mpc"
tmpGB2="$dir/histmatch_G2_$$.cache"
tmpBA1="$dir/histmatch_B1_$$.mpc"
tmpBB1="$dir/histmatch_B1_$$.cache"
tmpBA2="$dir/histmatch_B2_$$.mpc"
tmpBB2="$dir/histmatch_B2_$$.cache"
tmpSA1="$dir/histmatch_S1_$$.mpc"
tmpSB1="$dir/histmatch_S1_$$.cache"
tmpSA2="$dir/histmatch_S2_$$.mpc"
tmpSB2="$dir/histmatch_S2_$$.cache"
tmpOA2="$dir/histmatch_O2_$$.mpc"
tmpOB2="$dir/histmatch_O2_$$.cache"
tmpL="$dir/histmatch_L_$$.mpc"
tmpLC="$dir/histmatch_L_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpIA1 $tmpIB1 $tmpIA2 $tmpIB2 $tmpRA1 $tmpRB1 $tmpRA2 $tmpRB2 $tmpGA1 $tmpGB1 $tmpGA2 $tmpGB2 $tmpBA1 $tmpBB1 $tmpBA2 $tmpBB2 $tmpSA1 $tmpSB1 $tmpSA2 $tmpSB2 $tmpOA2 $tmpOB2 $tmpL $tmpLC;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpIA1 $tmpIB1 $tmpIA2 $tmpIB2 $tmpRA1 $tmpRB1 $tmpRA2 $tmpRB2 $tmpGA1 $tmpGB1 $tmpGA2 $tmpGB2 $tmpBA1 $tmpBB1 $tmpBA2 $tmpBB2 $tmpSA1 $tmpSB1 $tmpSA2 $tmpSB2 $tmpOA2 $tmpOB2 $tmpL $tmpLC; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpIA1 $tmpIB1 $tmpIA2 $tmpIB2 $tmpRA1 $tmpRB1 $tmpRA2 $tmpRB2 $tmpGA1 $tmpGB1 $tmpGA2 $tmpGB2 $tmpBA1 $tmpBB1 $tmpBA2 $tmpBB2 $tmpSA1 $tmpSB1 $tmpSA2 $tmpSB2 $tmpOA2 $tmpOB2 $tmpL $tmpLC; exit 1" ERR

# read the input images into the temp files and test validity.
convert -quiet "$infile1" -strip +repage "$tmpA1" ||
	errMsg "--- FILE $infile1 DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"

convert -quiet "$infile2" -strip +repage "$tmpA2" ||
	errMsg "--- FILE $infil2 DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"


# NOT CURRENTLY USED
: <<COMMENT1
# extract alpha channel if exists
alpha=`convert $tmpA2 -format "%A" info:`
if [ "$alpha" = "True" ]; then
	convert $tmpA2 -alpha extract $tmpOA2
fi
#echo "alpha=$alpha"

# test if infile2 is PseudoClass and RGB or Gray and if matte
type2=`convert $tmpA2 -format '%r' info:`
if [ "$type2" = "PseudoClassRGB" ]; then
	reduction="-type Palette"
elif [ "$type2" = "PseudoClassRGBMatte" ]; then
	reduction="-type Palette $tmpOA2 -compose copy_opacity -composite"
elif [ "$type2" = "PseudoClassGray" ]; then
	reduction="-type Grayscale"
elif [ "$type2" = "PseudoClassGrayMatte" ]; then
	reduction="-type Grayscale $tmpOA2 -compose copy_opacity -composite"
else
	reduction=""
fi
#echo "type2=$type2; reduction=$reduction"
COMMENT1

: <<COMMENT2
# test if infiles are grayscale
gray1="no"
gray2="no"
grayscale1=`convert $tmpA1 -format "%[colorspace]" info:`
typegray1=`convert $tmpA1 -format '%r' info: | grep 'Gray'`
grayscale2=`convert $tmpA2 -format "%[colorspace]" info:`
typegray2=`convert $tmpA2 -format '%r' info: | grep 'Gray'`
[ "$grayscale1" = "Gray" -o "$typegray1" != "" ] && gray1="yes"
[ "$grayscale2" = "Gray" -o "$typegray2" != "" ] && gray2="yes"
#echo "gray1=$gray1; gray2=$gray2"
COMMENT2

# initialize gray1 and gray2
gray1="no"
gray2="no"

# get colorspace and type for infile1
# colorspace swapped at IM 6.7.5.5, but not properly fixed until 6.7.6.6
# before swap verbose info reported colorspace=RGB after colorspace=sRGB
# not all formats report grayscale for colorspace (gif, tiff, jpg do not), but type will be grayscale
colorspace=`identify -ping -verbose $tmpA1 | sed -n 's/^.*Colorspace: \([^ ]*\).*$/\1/p'`
type=`identify -ping -verbose $tmpA1 | sed -n 's/^.*Type: \([^ ]*\).*$/\1/p'`
if [ "$colorspace" != "RGB" -a "$colorspace" != "sRGB" -a "$colorspace" != "Gray" ]; then
	errMsg "--- FILE $infile MUST BE RGB, sRGB or GRAY ---"
elif [ "$type1" = "Grayscale" ]; then
	gray1="yes"
fi
if [ "$gray1" = "yes" -a "$satmode" != "" ]; then
	errMsg "--- FILE $infile1 CANNOT CONVERT TO HSL/HSB SINCE IT IS GRAYSCALE ---"
fi


# get colorspace and type for infile2
# colorspace swapped at IM 6.7.5.5, but not properly fixed until 6.7.6.6
# before swap verbose info reported colorspace=RGB after colorspace=sRGB
# not all formats report grayscale for colorspace (gif, tiff, jpg do not), but type will be grayscale
colorspace=`identify -ping -verbose $tmpA2 | sed -n 's/^.*Colorspace: \([^ ]*\).*$/\1/p'`
type=`identify -ping -verbose $tmpA2 | sed -n 's/^.*Type: \([^ ]*\).*$/\1/p'`
if [ "$colorspace" != "RGB" -a "$colorspace" != "sRGB" -a "$colorspace" != "Gray" ]; then
	errMsg "--- FILE $infile MUST BE RGB, sRGB or GRAY ---"
elif [ "$type2" = "Grayscale" ]; then
	gray2="yes"
fi
if [ "$gray2" = "yes" -a "$satmode" != "" ]; then
	errMsg "--- FILE $infile2 CANNOT CONVERT TO HSL/HSB SINCE IT IS GRAYSCALE ---"
fi

# colorspace RGB and sRGB swapped between 6.7.5.5 and 6.7.6.7 
# though probably not resolved until the latter
# then -colorspace gray changed to linear between 6.7.6.7 and 6.7.8.2 
# then -separate converted to linear gray channels between 6.7.6.7 and 6.7.8.2,
# though probably not resolved until the latter
# so -colorspace HSL/HSB -separate and -colorspace gray became linear
# but we need to use -set colorspace RGB before using them at appropriate times
# so that results stay as in original script
# The following was determined from various version tests using redist.
# Note: bug in IM 6.7.6.6 HSL/HSB bad, 6.7.7.0 HSL/HSB/RGB bad, 6.7.7.8 & 6.7.7.9 HSL/HSB bad, 6.7.8.1 HSB very bad
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	cspace="RGB"
else
	cspace="sRGB"
fi
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
	cspace="sRGB"
fi


# convert infile1 to colormode if not already gray
# use -set colorspace RGB so that converting to gray will be non-linear as in original script
if [ "$gray1" = "no" ]; then
	if [ "$colormode" = "intensity" -o "$colormode" = "gray" ]; then
		convert $tmpA1 $setcspace -colorspace Gray $tmpIA1
	elif [ "$colormode" = "luminance" ]; then
		convert $tmpA1 $setcspace -colorspace Rec709Luma $tmpIA1
	elif [ "$colormode" = "lightness" ]; then
		convert $tmpA1 $setcspace -colorspace HSL -channel B -separate $tmpIA1
	elif [ "$colormode" = "brightness" ]; then
		convert $tmpA1 $setcspace  -colorspace HSB -channel B -separate $tmpIA1
	elif [ "$colormode" = "average" ]; then
		convert $tmpA1 $setcspace  -colorspace OHTA -channel R -separate $tmpIA1
	elif [ "$colormode" = "rgb" ]; then
		convert $tmpA1 $setcspace -channel R -separate $tmpRA1
		convert $tmpA1 $setcspace -channel G -separate $tmpGA1
		convert $tmpA1 $setcspace -channel B -separate $tmpBA1
	fi
fi


# convert infile2 to colormode
if [ "$colormode" = "intensity" -o "$colormode" = "gray" ]; then
	convert $tmpA2 $setcspace -colorspace Gray $tmpIA2
elif [ "$colormode" = "luminance" ]; then
	convert $tmpA2 $setcspace -colorspace Rec709Luma $tmpIA2
elif [ "$colormode" = "lightness" ]; then
	convert $tmpA2 $setcspace -colorspace HSL -channel B -separate $tmpIA2
elif [ "$colormode" = "brightness" ]; then
	convert $tmpA2 $setcspace  -colorspace HSB -channel B -separate $tmpIA2
elif [ "$colormode" = "average" ]; then
	convert $tmpA2 $setcspace  -colorspace OHTA -channel R -separate $tmpIA2
elif [ "$colormode" = "rgb" ]; then
	convert $tmpA2 $setcspace -channel R -separate $tmpRA2
	convert $tmpA2 $setcspace -channel G -separate $tmpGA2
	convert $tmpA2 $setcspace -channel B -separate $tmpBA2
fi


# process images
if [ "$gray1" = "yes" -a "$gray2" = "yes" ]; then
	genLutImage "$tmpA1" "$tmpA2"
	convert $tmpA2 $tmpL -clut "$outfile"
elif [ "$gray1" = "no" -a "$gray2" = "yes" -a "$colormode" != "rgb" ]; then
	genLutImage "$tmpIA1" "$tmpA2"
	convert $tmpA2 $tmpL -clut "$outfile"
elif [ "$gray1" = "yes" -a "$gray2" = "no" -a "$colormode" != "rgb" ]; then
	genLutImage "$tmpA1" "$tmpIA2"
	convert $tmpA2 $tmpL -clut "$outfile"
elif [ "$colormode" != "rgb" ]; then
	# colormode=gray etc
	genLutImage "$tmpIA1" "$tmpIA2"
	convert $tmpA2 $tmpL -clut "$outfile"
else
	# colormode=rgb
	echo "Processing Red Channel"
	genLutImage "$tmpRA1" "$tmpRA2"
	convert $tmpL $tmpRA2
	echo "Processing Green Channel"
	genLutImage "$tmpGA1" "$tmpGA2"
	convert $tmpL $tmpGA2
	echo "Processing Blue Channel"
	genLutImage "$tmpBA1" "$tmpBA2"
	convert $tmpL $tmpBA2
	convert $tmpRA2 $tmpGA2 $tmpBA2 -combine $tmpL
	convert $tmpA2 $tmpL -clut "$outfile"
fi

[ "$savelut" = "yes" ] && convert $tmpL histmatch_lut.png

exit 0