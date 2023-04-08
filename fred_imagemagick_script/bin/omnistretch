#!/bin/bash
#
# Developed by Fred Weinhaus 8/18/2007 .......... revised 4/25/2015
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
# define usage function
function usage
	{
	echo "USAGE: omnistretch [option1] [option2] [option3] [option4] infile outfile"
	echo ""
	echo "Option1 (choice of one only):"
	echo "  -h or -help                 get help"
	echo "  -st                         get image statistics; requires input_file"
	echo "  -m or -model                processing (color) model (HSL, HSB, RGB); requires infile and outfile; default HSL if none specified"
	echo ""
	echo "Option2 (choice of one only):"
	echo "  -b    low,[high[,gamma]]    process w/ brightness 0-100 for low,high; gamma 0-10"
	echo "  -c    low,[high[,gamma]]    process w/ histogram counts >=0 for low,high; gamma 0-10"
	echo "  -pc   low,[high[,gamma]]    process w/ % of max histogram counts 0-100 for low,high; gamma 0-10"
	echo "  -cc   low,[high[,gamma]]    process w/ cumulative histogram counts >=0 for low,high; gamma 0-10"
	echo "  -cd   low,[high[,gamma]]    process w/ cumulative histogram count differences >=0 for low,high; gamma 0-10"
	echo "  -ab   gamma_method          auto process w/ default brightness values of min,max,gamma"
	echo "  -ac   gamma_method          auto process w/ default histogram counts of 30,30,gamma"
	echo "  -apc  gamma_method          auto process w/ default % of max histogram counts of 10,10,gamma"
	echo "  -acc  gamma_method          auto process w/ default % cumulative histogram counts of .6,.6,gamma"
	echo "  -acd  gamma_method          auto process w/ default % cumulative histogram count differences of .06,.06,gamma"
	echo ""
	echo "Option3:"
	echo "  -s   sgamma                 process w/ saturation; sgamma 0-10"
	echo ""
	echo "Option4:"
	echo "  -g                          display graph of intensity mapping function"
	echo "                              but must be used in conjunction with option 1 or 2,"
	echo "                              but not -h or -help or -st"
	}
#
# define description function
function descr
	{
	echo "NAME: OMNISTRETCH "
	echo ""
	echo "PURPOSE: To enhance the dynamic range of brightness values and "
	echo "optionally saturation values in an image. "
	echo ""
	echo "DESCRIPTION: OMNISTRETCH is designed to enhance the dynamic "
	echo "range of brightness values and optionally saturation values "
	echo "in an image. It has various means of linearly doing this as "
	echo "described below. If a gamma value is also supplied, then a "
	echo "non-linear change is affected on top of the linear change. "
	echo "It works by computing a grayscale histogram, which is then "
	echo "used to compute a low and high value to map to black and "
	echo "white, respectively in the image as a whole or on either the "
	echo "Lightness or Brightness channels, depending upon which "
	echo "colorspace model is selected. The script uses the IM -level "
	echo "function to do the processing. If a saturation change is "
	echo "desired, then the IM -gamma function will be applied to "
	echo "the saturation channel. No saturation processing is allowed "
	echo "if the image is grayscale. The graph is normally just "
	echo "viewed, but a default parameter in the program can be set to "
	echo "allow it to be saved as outfilename_graph.gif. To end the "
	echo "script, close/quit the graph image. "
	echo ""
	echo "Argument:     ---  displays help information. "
	echo ""
	echo "Argument: -h  ---  displays help information. "
	echo ""
	echo "Argument: -g  ---  displays graph of intensity mapping function. "
	echo ""
	echo "Argument: -st  ---  displays image statistics, including:  "
	echo "minimum, maximum, mean, standard deviation and histogram "
	echo "in a range of brightness values scaled to 0-100. An "
	echo "input_file is required with no output_file specified. "
	echo ""
	echo "Argument: -m  ---  defines the processing color model to use for the "
	echo "processing of a color image. If HSL or HSB are specified, then the "
	echo "the image is first transformed into that color space and the "
	echo "corresponding channels are separated. Either the L or B channels "
	echo "will then be used to compute a histogram, which is then used "
	echo "to determine the low and high values to map to black and white, "
	echo "within that channel. The channels are then put back together and "
	echo "output in RGB format. If RGB is selected, the image is first "
	echo "converted to RGB format and used to generate a grayscale image. "
	echo "The grayscale image is then used to compute a histogram. The "
	echo "histogram is then used to determine the low and high values which "
	echo "are then stretched in the RGB image to black and white, respectively. "
	echo "The default for a color image is HSL. If a grayscale image is the "
	echo "input file, then the processing color model is ignored. Note that "
	echo " results for HSB tend to be undersaturated and for RGB tend to be "
	echo " oversaturated. "
	echo ""
	echo "Argument: -b  ---  defines the method using brightness values "
	echo "for low and high in the range of 0-100 to be linearly stretch "
	echo "to full black (0) and full white (100). If high is not provided, "
	echo "high=100-low. A value of 0,[100,[1]] leaves the image unchanged. "
	echo "Optionally, a gamma value may be included in the range "
	echo "of 0 upward; but, nominally 0-10, where 1 is no change, "
	echo "<1 is less contrast and >1 is more contrast. Gamma is a "
	echo "non-linear adjustment. "
	echo ""
	echo "Argument: -c  ---  defines the method using histogram counts "
	echo "for low and high. The method finds the first occurrences of "
	echo "the specified count (or higher) from each end of the histogram "
	echo "to locate the corresponding low and high brightness values to "
	echo "linearly stretch to full black (0) and full white (100). If only "
	echo "one count is specified, it will be used for both low and high. "
	echo "A value of 0,[0,[1]] leaves the image unchanged. Optionally, a "
	echo "gamma value may be included in the range of >=0; but, nominally "
	echo "0-10, where 1 (default) is no change, <1 is less contrast and >1 "
	echo "is more contrast. Gamma is a non-linear adjustment. "
	echo ""
	echo "Argument: -pc  ---  defines the method using percent of maximum "
	echo "histogram counts for low and high. The method finds the first "
	echo "occurrences of the specified percent of maximum histogram count "
	echo "(or higher) from each end of the histogram to locate the "
	echo "corresponding low and high brightness values to linearly stretch "
	echo "to full black (0) and full white (100). If only one count is "
	echo "specified, it will be used for both low and high. Counts for low "
	echo "and high are expressed as percent of the maximum count in the "
	echo "histogram, but the % symbol is not specified. A value of "
	echo "0,[0,[1]] leaves the image unchanged. Optionally, a gamma value "
	echo "may be included in the range of >=0; but, nominally 0-10, where "
	echo "1 (default) is no change, <1 is less contrast and >1 is more "
	echo "contrast. Gamma is a non-linear adjustment. "
	echo ""
	echo "Argument: -cc  ---  defines the method using cumulative histogram "
	echo "counts in percent of total count for low and high. The method "
	echo "finds the first occurrences of the specified cumulative count "
	echo "(or higher) from each end of the histogram to locate the "
	echo "corresponding low and high brightness values to linearly stretch "
	echo "to full black (0) and full white (100). If only one cumulative "
	echo "count is specified, it will be used for both low and high. "
	echo "A value of 0,[0,[1]] indicates no change in the image. Cumulative "
	echo "counts for low and high are expressed as percent of total pixels "
	echo "in the image, but the % symbol is not specified. Optionally, "
	echo "a gamma value may be included in the range of >=0; but, "
	echo "nominally 0-10, where 1 (default) is no change, <1 is less "
	echo "saturation and >1 is more saturation. Gamma is a non-linear "
	echo "adjustment. "
	echo ""
	echo "Argument: -cd  ---  defines the method using cumulative percent "
	echo "histogram count differences between successive histogram values "
	echo "equal to or exceeding low and high. The method finds the first "
	echo "occurrences of the specified cumulative count differences "
	echo "(or higher) from each end of the histogram to locate the "
	echo "corresponding low and high brightness values to linearly stretch "
	echo "to full black (0) and full white (100). If only one cumulative "
	echo "count difference is specified, it will be used for both low and high. "
	echo "A value of 0,[0,[1]] indicates no change in the image. Cumulative "
	echo "count differences for low and high are expressed as percent of total "
	echo "pixels in the image, but the % symbol is not specified. This method "
	echo "is basically looking for where the slope of the histogram, starting "
	echo "from each end, increases above a threshold given by low and high. "
	echo "Optionally, a gamma value may be included in the range of >=0; but, "
	echo "nominally 0-10, where 1 (default) is no change, <1 is less "
	echo "saturation and >1 is more saturation. Gamma is a non-linear "
	echo "adjustment. "
	echo ""
	echo "Argument: -ab  ---  automatic version of -b using the image's "
	echo "actual minimum and maximum values for low and high. gamma_method=1 "
	echo "sets gamma=1. gamma_method=2 determines gamma from the computed low "
	echo "and high. gamma_method=3 determines gamma from the image's standard "
	echo "deviation. "
	echo ""
	echo "Argument: -ac  ---  automatic version of -c using low and high "
	echo "counts of 30. gamma_method=1 sets gamma=1. gamma_method=2 determines "
	echo "gamma from computed low and high. gamma_method=3 determines gamma from "
	echo "the image's standard deviation. "
	echo ""
	echo "Argument: -apc  ---  automatic version of -pc using low and high "
	echo "percent of maximum counts of 10. gamma_method=1 sets gamma=1. "
	echo "gamma_method=2 determines gamma from computed low and high. "
	echo "gamma_method=3 determines gamma from the image's standard deviation. "
	echo ""
	echo "Argument: -acc  ---  automatic version of -cc using low and high "
	echo "cumulative counts of 0.6. gamma_method=1 sets gamma=1. gamma_method=2 "
	echo "determines gamma from computed low and high. gamma_method=3 determines "
	echo "gamma from the image's standard deviation "
	echo ""
	echo "Argument: -acd  ---  automatic version of -cd using low and high "
	echo "cumulative count differences of 0.06. gamma_method=1 sets gamma=1. "
	echo "gamma_method=2 determines gamma from computed low and high. "
	echo "gamma_method=3 determines gamma from the image's standard deviation "
	echo ""
	echo "OBSERVATION: All these algorithms tend to work better on image's which "
	echo "have long, low histogram tails or do not span the full range of 0-100. "
	echo "Typically, this means low contrast images. "
	echo ""
	echo "REMARK: Feel free to change the default values below for the "
	echo "automatic methods: -ac, -apc and -acc, if you find that those "
	echo "values work better for your class of imagery. Also the methods for "
	echo "automatically determining gamma are not well tested. Method 2 is "
	echo "limited to histograms for which low > 0 and/or high < 100. "
	echo "Method 3 seems to work for both low and high contrast images."
	echo ""
	echo "CAVEAT: No guarantee that this script will work on all platforms, "
	echo "nor that trapping of inconsistent parameters is complete and "
	echo "foolproof. Use At Your Own Risk. "
	echo ""
	}
#
# set default values for automatic processing methods: -ab, -ac, -apc, -acc, -acd
# defaults for -ab set from images min and max values
ac_low=30
ac_high=30
apc_low=10
apc_high=10
acc_low=0.6
acc_high=0.6
acd_low=0.06
acd_high=0.06
#
# set the default controlling constants for gamma_method 2 and 3
gm2=1    		 # gamma=1 + (gm2 * (computed-low + computed-high - 100) / 100)
gm3a=66; gm3b=1  # gamma=1 + (gm3b * (gm3a - std) / 100)
#
# set flag if graph is permanent (graph=save) or temporary (graph=view)
graph="view"
#
# set directory for temporary files
dir="."    # suggestions are dir="." or dir="/tmp"
#
# set the default colorspace model for a color image
colormodel=HSL
#
# function to report error messages
function errMsg
	{
	echo ""
	echo $1
	echo ""
	}
#
# test for correct number of arguments
if [ $# -eq 0 ]
	then
		p1=-h
		# help information
		echo ""
		usage
		echo ""
		descr
		exit 0
elif [ $# -eq 3 -o $# -gt 9 ]
	then
		errMsg "--- INCORRECT NUMBER OF PARAMETERS ---"
		usage
		exit 1
fi
#
# function to test for minus at start of value of second part of option 1 or 2
function checkMinus
	{
	test=`echo $val | grep -c '^-.*$'`   # returns 1 if match; 0 otherwise
    if [ $test -eq 1 ]
		then
			errMsg "$errorMsg"
			usage
			exit 1
		else
			if [ $pflag -eq 1 ]
				then
				v1=$val
			fi
    fi
	}
#
# get parameter values and do computations
while [ $# -gt 0 ]
	do
		# get parameter values
		case "$1" in
	   -h|-help)    # help information
				   echo ""
				   usage
				   descr
				   exit 0  ;;
			-g)    # display graph
				   display_graph="yes" ;;
		   -st)    # image statistics
				   p1=-st  ;;
	 -m|-model)    # colorspace model
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   pflag=3
				   colormodel=$1
				   if [ "$colormodel" != "HSL" -a "$colormodel" != "HSB" -a "$colormodel" != "RGB" ]
				   		then
				   		errMsg "--- MODEL=$colormodel IS NOT A VALID VALUE ---"
				   		usage
				   		exit 1
				   	fi
				   errorMsg="--- INCORRECT MODEL PARAMETER SPECIFICATION ---"
				   # save any existing val
				   oldval=$val
				   val=$colormodel
				   checkMinus
				   # reset val to old val
				   val=$oldval  ;;
			-b)    # brightness values
				   p1=-b
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT BRIGHTNESS PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-c)    # histogram counts 
				   p1=-c
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT HISTOGRAM COUNT PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-pc)   # percent of maximum histogram count
				   p1=-pc
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT PERCENT OF MAXIMUM HISTOGRAM COUNT PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-cc)   # cumulative histogram counts
				   p1=-cc
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT CUMULATIVE HISTOGRAM COUNT PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-cd)   # cumulative histogram count differences
				   p1=-cd
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT CUMULATIVE HISTOGRAM COUNT DIFFERENCE PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-s)    # saturation gamma
				   p2=-s
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=2
				   sgamma=$1
				   errorMsg="--- INCORRECT SATURATION PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-ab)   # automatic brighntess values
				   p1=-ab
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT GAMMA METHOD PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-ac)   # automatic histogram counts
				   p1=-ac
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT GAMMA METHOD PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-apc)  # automatic percent histogram counts
				   p1=-apc
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT GAMMA METHOD PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-acc)  # automatic cumulative histogram counts
				   p1=-acc
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT GAMMA METHOD PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			-acd)  # automatic cumulative histogram counts
				   p1=-acd
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign
				   pflag=1
				   val=$1
				   errorMsg="--- INCORRECT GAMMA METHOD PARAMETER SPECIFICATION ---"
				   checkMinus  ;;
			 -)    # STDIN and end of arguments
				   break
				   ;;
			-*)    # any other - argument
				   errorMsg="--- UNKNOWN OPTION ---"
				   usage
				   exit 1 ;;
			*)     # end of arguments which begin with minus sign
				   break ;;
		esac
		shift   # next option
done
#
# get infile and outfile
infile="$1"
if [ "$p1" != "-st" -a "$p1" != "" ]
	then
	outfile="$2"
fi
#
# test if output file provide for appropriate case
if [ "$p1" != "-h" -a "$p1" != "-st" -a "$outfile" = "" ]
	then
		errMsg "--- NO OUTPUT FILE WAS SPECIFIED ---"
		usage
		exit 1
fi	
#
# setup temporary images and auto delete upon exit
# use mpc/cache to hold input image temporarily in memory
#tmpA="$dir/omnistretch_$$.mpc"
#tmpB="$dir/omnistretch_$$.cache"
# use miff instead as mpc/cache produces too many values/counts for the same bin in the IM histogram that need to be combined
tmpA="$dir/omnistretch_$$.miff"
tmp00="$dir/omnistretch_00_$$.png"
tmp0="$dir/omnistretch_0_$$.png"
tmp1="$dir/omnistretch_1_$$.png"
tmp2="$dir/omnistretch_2_$$.png"
tmp1p="$dir/omnistretch_1_proc_$$.png"
tmp2p="$dir/omnistretch_2_proc_$$.png"
# get outfile name before suffix
outname=`echo "$outfile" | sed -n 's/^\([^.]*\)[.][^.]*$/\1/ p'`
gg="_graph"
tmp3="$dir/$outname$gg.gif"
if [ "$graph" = "view" ] 
	then 
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p $tmp3;" 0
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p $tmp3; exit 1" 1 2 3 15
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p $tmp3; exit 1" ERR
elif [ "$graph" = "save" ]
	then
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p;" 0
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p; exit 1" 1 2 3 15
	trap "rm -f $tmpA $tmpB $tmp00 $tmp0 $tmp1 $tmp2 $tmp1p $tmp2p; exit 1" ERR
else
	errMsg "--- NOT A VALID GRAPH DISPLAY OPTION ---"
fi
#
# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"
# test that outfile provided
[  "$p1" != "-st" -a "$p1" != "" -a "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"
#

# get im_version
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
# The following was determined from various version tests using redist.
# Note: bug in IM 6.7.6.6 HSL/HSB bad, 6.7.7.0 HSL/HSB/RGB bad, 6.7.7.8 & 6.7.7.9 HSL/HSB bad, 6.7.8.1 HSB very bad
if [ "$im_version" -lt "06070607" -o "$im_version" -gt "06070707" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi
if [ "$im_version" -lt "06070606" -o "$im_version" -gt "06070707" ]; then
	cspace="RGB"
else
	cspace="sRGB"
fi
# no need for setcspace for grayscale or channels after 6.8.5.4
if [ "$im_version" -gt "06080504" ]; then
	setcspace=""
	cspace="sRGB"
fi



if convert -quiet "$infile" +repage "$tmpA"
	then
		# get colorspace
		colorspace=`identify -verbose $tmpA | sed -n 's/^.*Colorspace: \([^ ]*\).*$/\1/p'`
		type=`identify -ping -verbose $tmpA | sed -n 's/^.*Type: \([^ ]*\).*$/\1/p'`
		if [ "$type" != "Grayscale" -a "$colorspace" != "Gray" -a "$colormodel" != "RGB" ]
			then
			 	convert $tmpA $setcspace -colorspace $colormodel -channel R -separate $tmp0
			 	convert $tmpA $setcspace -colorspace $colormodel -channel G -separate $tmp1
			 	convert $tmpA $setcspace -colorspace $colormodel -channel B -separate $tmp2
		elif [ "$colorspace" != "Gray" -a "$colormodel" = "RGB" ]
			then
			 	convert $tmpA $setcspace -colorspace Gray $tmp2
			else
				convert $tmpA $tmp2
		fi
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
		usage
		exit 1
fi
#
#function to get width, height and total pixels
function imagesize
	{
	width=`identify -format %w $tmpA`
	height=`identify -format %h $tmpA`
	totpix=`echo "$width * $height" | bc`
	}
#
# function to get min, max, mean, std from Brightness channel (or Graylevel image)
function imagestats
	{
	data=`convert $tmp2 -verbose info:`
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
#
# function to get histogram values and counts as arrays
function histogram
	{
	# get lists of values and counts
	# note that IM histograms are not well sorted (and have multiple bins with counts for the same values)
	value=`convert $tmp2 -format %c -depth 8 -define histogram:unique-colors=true histogram:info: | sort -k 2 -b | sed -n 's/^ *[0-9]*: [(]\([0-9 ]*\).*$/\1/ p'`
	count=`convert $tmp2 -format %c -depth 8 -define histogram:unique-colors=true histogram:info: | sort -k 2 -b | sed -n 's/^ *\([0-9]*\): [(].*$/\1/ p'`
	#
	# put value and count into arrays
	valueArr=($value)
	countArr=($count)
	#
	# check if both arrays are the same size
	if [ ${#valueArr[*]} -ne ${#countArr[*]} ]
		then
			errMsg "--- ARRAY SIZES DO NOT MATCH ---"
			exit 1
	fi
	#
	# process arrays to merge multiple bins at same value
	i=0
	jold=-1
	k=-1
	totcount=0
	while [ $i -lt ${#valueArr[*]} ]
		do
		j=${valueArr[$i]}
		# test if previous bin same as current one. if so, the combine
		if [ $j -eq $jold ]
			then
			mvalueArr[$k]=$j
			mcountArr[$k]=`expr ${countArr[$i]} + ${mcountArr[$k]}`
		else
			k=`expr $k + 1`
			mvalueArr[$k]=$j
			mcountArr[$k]=${countArr[$i]}
		fi
		i=`expr $i + 1`
		jold=$j
	done
	#
	# reset histogram values to range 0-100 and build index array
	index=0
	while [ $index -lt ${#mvalueArr[*]} ]
		do
			mvalueArr[$index]=`echo "scale=1; ${mvalueArr[$index]} * 100 / 255" | bc`
			index=`expr $index + 1`
	done
	# get maximum count in histogram and total count in histogram
	index=0
	maxcount=0
	totcount=0
	while [ $index -lt ${#mcountArr[*]} ]
		do
			if [ ${mcountArr[$index]} -gt $maxcount ]
				then
					maxcount=${mcountArr[$index]}
			fi
			totcount=`expr $totcount + ${mcountArr[$index]}`
			index=`expr $index + 1`
	done
	}
	#
# function to get percent of maximum counts and cumulative percentages in histogram from each end
function cumHistograms
	{
	# do first half of histogram from low end to midpt
	index=0
	cum1=0
	cum2=0
	cum2_prev=0
	midpt=`expr ${#mcountArr[*]} / 2`
	numpts=${#mcountArr[*]}
	while [ $index -lt $midpt ]
		do
			if [ "$p1" = "-cc" -o "$p1" = "-acc" -o "$p1" = "-st" ]
				then
				# cumulative percent count
				cum1=`expr ${mcountArr[$index]} + $cum1`
				ccountArr[$index]=$cum1
			fi
			if [ "$p1" = "-cd" -o "$p1" = "-acd" -o "$p1" = "-st" ]
				then
				# cumulative percent count differences
				cum2=`expr ${mcountArr[$index]} + $cum2`
				cdiff=`expr $cum2 - $cum2_prev`
				dcountArr[$index]=$cdiff
			fi
			cum2_prev=$cum2
			index=`expr $index + 1`
	done
	# do second half of histogram from high end to midpt
	index=`expr ${#mcountArr[*]} - 1`
	cum1=0
	cum2=0
	cum2_prev=0
	while [ $index -ge $midpt ]
		do
			if [ "$p1" = "-cc" -o "$p1" = "-acc" -o "$p1" = "-st" ]
				then
				# cumulative percent count
				cum1=`expr ${mcountArr[$index]} + $cum1`
				ccountArr[$index]=$cum1
			fi
			if [ "$p1" = "-cd" -o "$p1" = "-acd" -o "$p1" = "-st" ]
				then
				# cumulative percent count differences
				cum2=`expr ${mcountArr[$index]} + $cum2`
				cdiff=`expr $cum2 - $cum2_prev`
				dcountArr[$index]=$cdiff
			fi
			cum2_prev=$cum2
			index=`expr $index - 1`
	done
	}
#
# function to check specified values for consistency - (must be numeric)
function parmCount
	{
	low=`echo $v1 | sed -n 's/^\([0-9.-]*\).*$/\1/ p'`
	high=`echo $v1 | sed -n 's/^[0-9.-]*,\([0-9.-]*\).*$/\1/ p'`
	gamma=`echo $v1 | sed -n 's/^[0-9.-]*,[0-9.-]*,\([0-9.-]*\)$/\1/ p'`
	if [ "$low" = "" -a "$high" = "" -a "$gamma" = "" ]
	then
		errMsg "--- NO VALUES WERE PROVIDED ---"
		usage
		exit 1
	elif [ "$low" != "" -a "$high" != "" -a "$gamma" != "" ]
	then
		parmcount=3
	elif [ "$low" != "" -a "$high" != "" -a "$gamma" = "" ]
	then
		parmcount=2
		gamma=1
	elif [ "$low" != "" -a "$high" = "" -a "$p1" != "-b" ]
	then
		parmcount=1
		high=$low
		gamma=1
	elif [ "$low" != "" -a "$high" = "" -a "$p1" = "-b" ]
	then
		parmcount=1
		gamma=1
		high=`echo "scale=1; (100 - $low) / 1" | bc`
	else
		parmcount=0
		errMsg "--- AN INCONSISTENT SET OF VALUES HAS BEEN PROVIDED ---"
		usage
		exit 1
	fi
	}
#
# function to convert processed values into common format
function parmValue
	{
	if [ $parmcount -le 3 -a $parmcount -ge 1 ]
	then
		parms=$low%,$high%,$gamma
	else
		errMsg "--- AN INCONSISTENT SET OF VALUES HAS BEEN PROVIDED ---"
		usage
		exit 1
	fi
	}
# function to get hmin and hmax values from each end of the histogram respectively using low and high
function histvals
	{
	# note: convert percent low high values to absolute counts to avoid precision issues on comparisons
	case "$p1" in
		-c)     # histogram counts 
				useArr=(${mcountArr[*]})  ;;
		-pc)    # percent of maximum histogram counts 
				low=`echo "scale=0; $maxcount * $low / 100" | bc`
				high=`echo "scale=0; $maxcount * $high / 100" | bc`
				useArr=(${mcountArr[*]})  ;;
		-cc)    # percent cumulative histogram counts
				low=`echo "scale=0; $totcount * $low / 100" | bc`
				high=`echo "scale=0; $totcount * $high / 100" | bc`
				useArr=(${ccountArr[*]})  ;;
		-cd)    # percent cumulative histogram count differences
				low=`echo "scale=0; $totcount * $low / 100" | bc`
				high=`echo "scale=0; $totcount * $high / 100" | bc`
				useArr=(${dcountArr[*]})  ;;
		-ac)     # histogram counts 
				useArr=(${mcountArr[*]})  ;;
		-apc)    # percent of maximum histogram counts 
				low=`echo "scale=0; $maxcount * $low / 100" | bc`
				high=`echo "scale=0; $maxcount * $high / 100" | bc`
				useArr=(${mcountArr[*]})  ;;
		-acc)    # percent cumulative histogram counts
				low=`echo "scale=0; $totcount * $low / 100" | bc`
				high=`echo "scale=0; $totcount * $high / 100" | bc`
				useArr=(${ccountArr[*]})  ;;
		-acd)    # percent cumulative histogram count differences
				low=`echo "scale=0; $totcount * $low / 100" | bc`
				high=`echo "scale=0; $totcount * $high / 100" | bc`
				useArr=(${dcountArr[*]})  ;;
	esac
	# get hmin value from low end of histogram where count is >= low
	index=0
	while [ $index -lt ${#mvalueArr[*]} ]
		do
			if [ ${useArr[$index]} -ge $low ]
				then
					break
			fi
			index=`expr $index + 1`
	done
	hmin=${mvalueArr[$index]}
	# get hmax value from high end of histogram where count is >= high
	index=`expr ${#mvalueArr[*]} - 1`
	while [ $index -gt 0 ]
		do
			if [ ${useArr[$index]} -ge $high ]
				then
					break
			fi
			index=`expr $index - 1`
	done
	hmax=${mvalueArr[$index]}
	test=`echo "$hmin >= $hmax" | bc`         # test=1 if true, otherwise 0""
    if [ $test -eq 1 ]
		then
			errMsg "--- NO HISTOGRAM VALUES WERE FOUND ---"
			exit 1
	fi
	}
#
#
# function to test input low and high values
function checkLowHigh
	{
	errorflag=no
	case "$p1" in
		-b)     # brightness values
				if [ `echo "$low < 0 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-c)     # histogram counts 
				if [ `echo "$low < 0 || $high < 0 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-pc)    # percent of maximum histogram counts 
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-cc)    # percent cumulative histogram counts
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-cd)    # percent cumulative histogram count differences
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-s)     # saturation gamma
				if [ `echo "$sgamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-ab)    # automatic brighntess values
				if [ `echo "$low < 0 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-ac)    # automatic histogram counts
				if [ `echo "$low < 0 || $high < 0 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-apc)   # automatic percent of maximum histogram counts
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-acc)   # automatic percent cumulative histogram counts
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
		-acc)   # automatic percent cumulative histogram count differences
				if [ `echo "$low < 0 || $high < 0 || $low > 100 || $high > 100 || $gamma < 0" | bc` -eq 1 ]
					then
						errorflag=yes
				fi  ;;
	esac
	if [ "$errorflag" = "yes" ]
		then
			errMsg "--- ONE OR MORE LOW,HIGH,GAMMA VALUES EXCEED THE ALLOWED RANGE ---"
			exit 1
	fi
	}
# function to check whether low >= high
function compareLowHigh
	{
	test=`echo "$low > $high" | bc`         # test=1 if true, otherwise 0""
    if [ $test -eq 1 ]
		then
			errMsg "--- THE PROCESSED LOW VALUE IS GREATER THAN THE PROCESSED HIGH VALUE ---"
			exit 1
	fi
	}
#
# function to compute gamma for automatic methods
function computeGamma
	{
	case "$v1" in
		1)	gamma=1  ;;
		2)	gamma=`echo "scale=2; 1 + ($gm2 * ($low + $high - 100) / 100)" | bc`  ;;
		3)	# imagestats already computed for p1=-ab
			if [ "$p1" = "-ac" -o "$p1" = "-apc" -o "$p1" = "-acc" ]
				then
				imagestats
			fi
			gamma=`echo "scale=2; 1 + ($gm3b * ($gm3a - $std) / 100)" | bc`  ;;
	esac
	}
#
# do method specific calculations
echo ""
echo "Please Wait - It May Take Some Time To Process The Image"
echo ""
#
# get parameter values and do computations
case "$p1" in
	-st)    # image statistics
			echo ""
			echo "Image Statistics:"
			echo ""
			echo "Colorspace = $colorspace"
			echo ""
			imagesize
			echo "Width = $width"
			echo "Height = $height"
			echo "Total Pixels = $totpix"
			echo ""
			imagestats
			echo "Min (0-100) = $min"
			echo "Max (0-100) = $max"
			echo "Mean (0-100) = $mean"
			echo "Std (0-...) = $std"
			echo ""
			echo "Histogram: (Please Wait)"
			histogram
			cumHistograms
			echo ""
			echo "Total Values=$numpts"
			echo "Mid Point=$midpt"
			echo "Max Count = $maxcount"
			echo "Tot Count = $totcount"
			echo ""
			echo "B=brightness 0-100(0-255);  C=count;  PC=percent of maximum count;  CC=cumulative count percent;  CD=cumulative count percent difference"
			echo ""
			index=0
			while [ $index -lt ${#mvalueArr[*]} ]
				do
				index1=`expr $index + 1`
				B255=`echo "scale=0; 255 * ${mvalueArr[$index]} / 100" | bc`
				pcountArr[$index]=`echo "scale=1; 100 * ${mcountArr[$index]} / $maxcount" | bc`
 				ccountArr[$index]=`echo "scale=2; 100 * ${ccountArr[$index]} / $totcount" | bc`
 				dcountArr[$index]=`echo "scale=2; 100 * ${dcountArr[$index]} / $totcount" | bc`
         		printf %-13s $index1  B=${mvalueArr[$index]}\($B255\)  C=${mcountArr[$index]}  PC=${pcountArr[$index]}  CC=${ccountArr[$index]}  CD=${dcountArr[$index]}
          		echo ""
				index=`expr $index + 1`
			done
			echo ""  
			exit 0  ;;
	-b)     # brightness values
			parmCount
			checkLowHigh
		    compareLowHigh
			parmValue  ;;
	-c)     # histogram counts 
			parmCount
			checkLowHigh
		    histogram
			histvals
			low=$hmin
			high=$hmax
		    compareLowHigh
			parmValue  ;;
	-pc)    # percent of maximum histogram counts 
			parmCount
			checkLowHigh
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
		    compareLowHigh
			parmValue  ;;
	-cc)    # cumulative histogram counts
			parmCount
			checkLowHigh
		    imagesize
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
		    compareLowHigh
			parmValue  ;;
	-cd)    # cumulative histogram counts
			parmCount
			checkLowHigh
		    imagesize
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
		    compareLowHigh
			parmValue  ;;
	-ab)    # automatic brighntess values
			imagestats
			low=$min
			high=$max
			computeGamma
			v1=$low,$high,$gamma
			parmCount
		    compareLowHigh
			parmValue  ;;
	-ac)    # automatic histogram counts
			low=$ac_low
			high=$ac_high
		    histogram
			histvals
			low=$hmin
			high=$hmax
			computeGamma
			v1=$low,$high,$gamma
			parmCount
		    compareLowHigh
			parmValue  ;;
	-apc)   # automatic percent of maximum histogram counts
			low=$apc_low
			high=$apc_high
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
			computeGamma
			v1=$low,$high,$gamma
			parmCount
		    compareLowHigh
			parmValue  ;;
	-acc)   # automatic cumulative histogram counts
			low=$acc_low
			high=$acc_high
		    imagesize
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
			computeGamma
			v1=$low,$high,$gamma
			parmCount
		    compareLowHigh
			parmValue  ;;
	-acd)   # automatic cumulative histogram counts
			low=$acd_low
			high=$acd_high
		    imagesize
		    histogram
		    cumHistograms
			histvals
			low=$hmin
			high=$hmax
			computeGamma
			v1=$low,$high,$gamma
			parmCount
		    compareLowHigh
			parmValue  ;;
esac
#
# display new end points
low255=`echo "scale=0; 255 * $low / 100" | bc`
high255=`echo "scale=0; 255 * $high / 100" | bc`
echo "Histogram Grayscale Value Mapped To Black = $low ($low255 in range 0 - 255)"
echo "Histogram Grayscale Value Mapped To White = $high ($high255 in range 0 - 255)"


# process the brightness and saturation bands using the low,high,gamma parameters extracted from any of the methods
# then recombine the color bands, if appropriate
 if [ "$type" != "Grayscale" -a "$colorspace" != "Gray" -a "$colormodel" != "RGB" -a "$p2" = "-s" ]
	then
		# process L or B
		# use -colorspace RGB at end rather than sRGB as HSB/HSL channels were made non-linear via -set colorspace RGB
		convert $tmp2 -level $parms $tmp2p
		# process S
		convert $tmp1 -gamma $sgamma $tmp1p
		# combine HSL or HSB channels
		convert $tmp0 -colorspace $colormodel \
			$tmp0 -compose CopyRed -composite \
			$tmp1p -compose CopyGreen -composite \
			$tmp2p -compose CopyBlue -composite \
			-colorspace $cspace $tmp00
 elif [ "$type" != "Grayscale" -a "$colorspace" != "Gray" -a "$colormodel" != "RGB" -a "$p2" != "-s" ]
	then
		# process L or B
		# use -colorspace RGB at end rather than sRGB as HSB/HSL channels were made non-linear via -set colorspace RGB
		convert $tmp2 -level $parms $tmp2p
		# combine HSL or HSB channels
		convert $tmp0 -colorspace $colormodel \
			$tmp0 -compose CopyRed -composite \
			$tmp1 -compose CopyGreen -composite \
			$tmp2p -compose CopyBlue -composite \
			-colorspace $cspace $tmp00
 elif [ "$type" != "Grayscale" -a "$colorspace" != "Gray" -a "$colormodel" = "RGB" -a "$p2" = "-s" ]
	then
		# process RGB together
		# use -colorspace RGB at end rather than sRGB as HSB/HSL channels were made non-linear via -set colorspace RGB
		convert $tmpA -level $parms $tmp00
		# separate into HSL or HSB
		convert $tmp00 $setcspace -colorspace $colormodel -channel R -separate $tmp0
		convert $tmp00 $setcspace -colorspace $colormodel -channel G -separate $tmp1
		convert $tmp00 $setcspace -colorspace $colormodel -channel B -separate $tmp2
		# process S
		convert $tmp1 -gamma $sgamma $tmp1p
		# combine HSL or HSB channels
		convert $tmp00 -colorspace $colormodel \
			$tmp0 -compose CopyRed -composite \
			$tmp1p -compose CopyGreen -composite \
			$tmp2 -compose CopyBlue -composite \
			-colorspace $cspace $tmp00
 elif [ "$type" != "Grayscale" -a "$colorspace" != "Gray" -a "$colormodel" = "RGB" -a "$p2" != "-s" ]
	then
		# process RGB together
		convert $tmpA -level $parms $tmp00
 else
		# process Grayscale
		convert $tmp2 -level $parms $tmp2p
		convert $tmp2p $tmp00
fi
convert $tmp00 "$outfile"
# display graph if option -g
if [ "$display_graph" = "yes" -a `echo "$gamma == 1" | bc` -eq 1 ]
	then
	xscale=100
	yscale=100
	pairArray=(0,0 $low,0 $high,100, 100,100)
	numpairs=${#pairArray[*]}
	i=0
	while [ $i -lt $numpairs ]
		do
		xArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f1`
		yArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f2`
		xArray[$i]=`echo "scale=1; 100 * ${xArray[$i]} / $xscale" | bc`
		yArray[$i]=`echo "scale=1; 100 * ${yArray[$i]} / $yscale" | bc`
		i=`expr $i + 1`
	done
	i=0
	while [ $i -lt $numpairs ]
		do
		pairArray[$i]=${xArray[$i]},${yArray[$i]}
		i=`expr $i + 1`
	done
	points=${pairArray[*]}
echo "Break Points = $points"
	convert -size 150x150 xc: -fill white -stroke black -draw "rectangle 40,10 141,112" $tmp3
	convert $tmp3 \( -size 100x101 xc: -stroke red -fill white -draw "polyline $points" -flip \) -compose over -geometry 100x101+41+11 -composite $tmp3
	convert $tmp3 -font Arial -pointsize 10 -draw "text 30,122 '0' text 20,17 '100' text 20,17 '100' text 40,60 '_' text 27,65 '50' text 90,112 '|' text 85,125 '50' text 70,140 'i n p u t'" $tmp3
	convert -respect-parenthesis $tmp3 \( -background white -fill black -font Arial -pointsize 10 -gravity center label:'o \nu \nt \np \nu \nt ' -trim \) -geometry +10+20 -compose over -composite $tmp3
	display $tmp3
elif [ "$display_graph" = "yes" -a `echo "$gamma >= 0" | bc` -eq 1 ]
	then
	xscale=100
	yscale=100
	pairArray=(0,0 $low,0 $high,100, 100,100)
	numpairs=${#pairArray[*]}
	i=0
	while [ $i -lt $numpairs ]
		do
		xArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f1`
		yArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f2`
		xArray[$i]=`echo "scale=0; 100 * ${xArray[$i]} / $xscale" | bc`
		yArray[$i]=`echo "scale=0; 100 * ${yArray[$i]} / $yscale" | bc`
		i=`expr $i + 1`
	done
echo "Break Points = ${pairArray[*]}"
	# convert to lut
	xarrlen=${#xArray[*]}
	yarrlen=${#yArray[*]}
	numsegs=`expr $xarrlen - 1`
	#
	# create LUT segments and composite LUT
	m=0
	xx=x
	ht=1
	len=`expr $xscale + 1`
	bsize=$len$xx$ht
	convert -size $bsize xc:black $tmp1
	while [ $m -lt $numsegs ]
		do
			n=`expr $m + 1`
			bpx1=${xArray[$m]}
			bpy1=`echo "scale=3; ${yArray[$m]} / $yscale" | bc`
			bpx2=${xArray[$n]}
			bpy2=`echo "scale=3; ${yArray[$n]} / $yscale" | bc`
			delx=`expr $bpx2 - $bpx1 + 1`
			convert -size 10x$delx gradient: -rotate 90 -fx "$bpy1*(1-u) + $bpy2*u" -alpha off $tmp2
			if [ $bpx1 -eq 0 ]
				then
					xstart=$bpx1
			else
				xstart=$bpx1
	#			xstart=`expr $bpx1 - 1`
			fi
			convert $tmp1 $tmp2 -geometry +$xstart+0 -compose Over -composite $tmp1
			m=`expr $m + 1`
	done
	convert $tmp1 -gamma $gamma -scale 100x101! $tmp1
	convert -size 100x101 xc: -stroke red -draw 'line 0,50 99,50' $tmp2
	convert $tmp1 $tmp2 -resize 400% miff:- | composite - -displace 0x200  miff:- | convert - -resize 25% $tmp1
	convert -size 150x150 xc: -fill white -stroke black -draw "rectangle 40,10 141,112" $tmp2
	convert $tmp2 $tmp1 -compose over -geometry 100x101+41+11 -composite $tmp3
	convert $tmp3 -font Arial -pointsize 10 -draw "text 30,122 '0' text 20,17 '100' text 20,17 '100' text 40,60 '_' text 27,65 '50' text 90,112 '|' text 85,125 '50' text 70,140 'i n p u t'" $tmp3
	convert -respect-parenthesis $tmp3 \( -background white -fill black -font Arial -pointsize 10 -gravity center label:'o \nu \nt \np \nu \nt ' -trim \) -geometry +10+20 -compose over -composite $tmp3
	display $tmp3
fi
exit 0