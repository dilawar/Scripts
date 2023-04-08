#!/bin/bash
#
# Developed by Fred Weinhaus 4/7/2010 .......... revised 4/25/2015
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
# USAGE: emboss [-m method] [-a azimuth] [-e elevation] [-d depth] [-i intensity] [-c compose] [-g gray] infile outfile
# USAGE: emboss [-h or -help]
# 
# OPTIONS:
# 
# -m      method            method of embossing; choices are 1 or 2;
#                           1 simulates the GIMP approach;
#                           2 simulates the Photoshop approach; default=1
# -a      azimuth           azimuth angle for light source; 0<=integer<=360; 
#                           counterclockwise from positive x axis (East);
#                           default=135 (NorthWest)
# -e      elevation         elevation angle for light source; 0<=integer<=90;
#                           upwards from x-y plane; default=45; only for method=1
# -d      depth             depth of emboss effect; integer>0; default=1
# -i      intensity         intensity of emboss effect; -50<=integer<=50; 
#                           default=0; only for method=2
# -c      compose           compose method to use to composite emboss result 
#                           with input image. The default is no composite.
# -g      gray              desired base graylevel for method=1; 0<float<1;
#                           default is no adjustment from normal shading
# 
###
# 
# NAME: EMBOSS 
# 
# PURPOSE: To apply an emboss effect to an image.
# 
# DESCRIPTION: EMBOSS applies an emboss effect to an image. Two methods are 
# available. The first simulates the GIMP approach. The second simulates the
# Photoshop approach.
# 
# 
# ARGUMENTS: 
# 
# -m method ... METHOD of embossing. The choices are 1 or 2. Method 1 simulates 
# the GIMP approach and uses parameters of azimuth, elevation and depth. The 
# second simulates the Photoshop approach and uses parameters of azimuth, 
# depth and intensity. The default=1.
# 
# -a azimuth ... AZIMUTH is the angle in degrees in the x-y plane measured 
# counterclockwise from EAST to the light source. Values are integers in the 
# range 0<=azimuth<=360. The default=135 (NorthWest).
# 
# -e elevation ... ELEVATION is the angle in degrees upwards from the x-y plane 
# to the light source. Values are integers in the range 0<=elevation<=90. 
# The default=45. Note this parameter is only used with method=1
# 
# -d depth ... DEPTH controls the depth effect in the embossing. For method=1, 
# it adjust the contrast of the emboss effect. For method=2, it adjust the 
# separation in pixels of the dark and light parts. Values are integers>0. 
# The default=1
# 
# -i intensity ... INTENSITY controls the contrast of the emboss effect in 
# method=2. Values are integers in the range of -50<=intensity<=50. Positive 
# values increase the contrast and negative values decrease the contrast. 
# The default=0
# 
# -c compose ... COMPOSE method to use to composite emboss result with input 
# image. Valid compose methods include: bumpmap, multiply, color_burn, 
# color_dodge, linear_burn, linear_dodge, linear_light, hard_light, soft_light,
# pegtop_light, pin_light, vivid_light, overlay. The default is no composite.
# 
# -g gray ... GRAY is the desired base graylevel for method=1. Values are 
# 0<float<1. The default is no adjustment from the normal shading base level 
# which gets brighter as the elevation angle increases according to the 
# sin(elevation). Nominal value for elevation=45 is gray=.707
# 
# REQUIREMENTS: IM 6.5.3-4 in order to support -set option:compose:args in 
# method 2 and IM 6.4.8-8 in order to support -function polynomial in method 1.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
method=1			# 1=GIMP; 2=PS
azimuth=135			# azimuth angle for light source
elevation=45		# elevation angle for light source; method=1 only
depth=1 			# depth factor
intensity=0			# intensity change percent -50<=integer<=50; method=2 only
compose=""          # compose method
gray=""				# nominal graylevel for method=1

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
				-m)    # get method
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID METHOD SPECIFICATION ---"
					   checkMinus "$1"
					   method=`expr "$1" : '\([0-9]*\)'`
					   [ "$method" = "" ] && errMsg "--- METHOD=$method MUST BE A NON-NEGATIVE INTEGER ---"
					   [ $method -ne 1 -a $method -ne 2 ] && errMsg "--- METHOD=$method MUST BE EITHER 1 OR 2 ---"
					   ;;
				-a)    # get azimuth
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AZUMUTH SPECIFICATION ---"
					   checkMinus "$1"
					   azimuth=`expr "$1" : '\([0-9]*\)'`
					   [ "$azimuth" = "" ] && errMsg "--- AZUMUTH=$azimuth MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$azimuth < 0" | bc`
					   test2=`echo "$azimuth > 360" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- AZUMUTH=$azimuth MUST BE AN INTEGER BETWEEN 0 AND 360 ---"
					   ;;
				-e)    # get elevation
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID ELEVATION SPECIFICATION ---"
					   checkMinus "$1"
					   elevation=`expr "$1" : '\([0-9]*\)'`
					   [ "$elevation" = "" ] && errMsg "--- ELEVATION=$elevation MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$elevation < 0" | bc`
					   test2=`echo "$elevation > 90" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- ELEVATION=$elevation MUST BE AN INTEGER BETWEEN 0 AND 90 ---"
					   ;;
				-d)    # get  depth
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DEPTH SPECIFICATION ---"
					   checkMinus "$1"
					   depth=`expr "$1" : '\([0-9]*\)'`
					   [ "$depth" = "" ] && errMsg "--- DEPTH=$depth MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test=`echo "$depth < 1" | bc`
					   [ $test -eq 1 ] && errMsg "--- DEPTH=$depth MUST BE A POSITIVE INTEGER ---"
					   ;;
				-i)    # get intensity
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID INTENSITY SPECIFICATION ---"
					   #checkMinus "$1"
					   intensity=`expr "$1" : '\([-0-9]*\)'`
					   [ "$intensity" = "" ] && errMsg "--- INTENSITY=$intensity MUST BE AN INTEGER VALUE ---"
					   test1=`echo "$intensity < -50" | bc`
					   test2=`echo "$intensity > 50" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- INTENSITY=$intensity MUST BE AN INTEGER BETWEEN -50 AND 50 ---"
					   ;;
				-c)    # get  compose
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COMPOSE SPECIFICATION ---"
					   checkMinus "$1"
					   compose=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$compose" in 
					   		overlay) ;;
					   		multiply) ;;
					   		hard_light) ;;
					   		soft_light) ;;
					   		pegtop_light) ;;
					   		pin_light) ;;
					   		linear_light) ;;
					   		vivid_light) ;;
					   		linear_dodge) ;;
					   		linear_burn) ;;
					   		color_dodge) ;;
					   		color_burn) ;;
					   		bumpmap) ;;
					   		*) errMsg "--- COMPOSE=$compose IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-g)    # get gray
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GRAY SPECIFICATION ---"
					   checkMinus "$1"
					   gray=`expr "$1" : '\([.0-9]*\)'`
					   [ "$gray" = "" ] && errMsg "--- GRAY=$gray MUST BE A NON-NEGATIVE INTEGER ---"
					   test1=`echo "$gray <= 0" | bc`
					   test2=`echo "$gray >= 1" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- GRAY=$gray MUST BE A FLOAT GREATER THAN 0 AND LESS THAN 1 ---"
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
tmpA1="$dir/emboss_1_$$.mpc"
tmpB1="$dir/emboss_1_$$.cache"
tmpA2="$dir/emboss_2_$$.mpc"
tmpB2="$dir/emboss_2_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2; exit 1" ERR

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
# The following was determined from various version tests using emboss.
# with IM 6.6.0.10, 6.7.2.0, 6.7.4.10, 6.7.6.8, 6.7.6.9, 6.7.6.10, 6.7.8.7
# Note: added $setcspace when reading input
# Note: bugs for method=2 sometime after IM 6.7.4.0 through 6.7.6.8, which I cannot seem to fix
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
fi


# read the input image and filter image into the temp files and test validity.
convert -quiet "$infile" +repage $setcspace "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"

# set up for gray adjustment
if [ "$gray" = "" ]; then
	grayadjust=""
else
	newgray=`convert xc: -format "%[fx:100*($gray-sin(pi*$elevation/180))]" info:`
	grayadjust="-evaluate add $newgray%"
fi
if [ $method -eq 1 ]; then
	[ "$im_version" -lt "06040808" ] && errMsg "--- IM VERSION IS NOT COMPATIBLE ---"
	convert $tmpA1 -colorspace gray -shade ${azimuth}x${elevation} $grayadjust $tmpA2
	mean=`convert $tmpA2 -format "%[fx:mean]" info:`
	aa=$depth
	bb=`convert xc: -format "%[fx:$mean*(1-$depth)]" info:`
	convert $tmpA2 -function polynomial "$aa,$bb" $tmpA2
	
elif [ $method -eq 2 ]; then
	xdist1=`convert xc: -format "%[fx:cos(pi*(180-$azimuth)/180)*$depth/2]" info:`
	ydist1=`convert xc: -format "%[fx:sin(pi*(180-$azimuth)/180)*$depth/2]" info:`
	xdist2=`convert xc: -format "%[fx:-$xdist1]" info:`
	ydist2=`convert xc: -format "%[fx:-$ydist1]" info:`
	intensity1=`convert xc: -format "%[fx:100-abs($intensity)]" info:`
	test=`convert xc: -format "%[fx:$intensity<0?1:0]" info:`
	if [ $test -eq 0 ]; then
		levelize="-level $intensity1%"
	else
		levelize="+level $intensity1%"
	fi
	
	[ "$im_version" -lt "06050403" ] && errMsg "--- IM VERSION IS NOT COMPATIBLE ---"
	convert \( $tmpA1 -colorspace gray \) \( -clone 0 -negate \) \
		\( -clone 0 -distort SRT "0,0 1 0 $xdist2,$ydist2" \) \
		\( -clone 1 -distort SRT "0,0 1 0 $xdist1,$ydist1" \) \
		-delete 0,1 -define compose:args=50 -compose blend -composite \
		$levelize $tmpA2
fi

if [ "$compose" = "" ]; then
	convert $tmpA2 "$outfile"
else
	convert $tmpA1 $tmpA2 -compose $compose -composite "$outfile"
fi
exit 0


