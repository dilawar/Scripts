#!/bin/bash
#
# Developed by Fred Weinhaus 12/16/2013 .......... revised 4/25/2015
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
# USAGE: crossprocess [-r ramt] [-g gamt] [-b bamt ] [-B bright] [-C contrast]
# infile outfile
# USAGE: crossprocess [-h or -help]
# 
# OPTIONS:
# 
# -r    ramt        red sigmoidal-contrast amount; -100<=integer<=100; 
#                   default=0 (no change)
# -g    gamt        green sigmoidal-contrast amount; -100<=integer<=100; 
#                   default=0 (no change)
# -b    bamt        blue sigmoidal-contrast amount; -100<=integer<=100; 
#                   default=0 (no change)
# -B    bright      post process brightness adjust; -100<=integer<=100; 
#                   default=0 (no change)
# -C    contrast    post process contrast adjust; -100<=integer<=100; 
#                   default=0 (no change)
# 
###
# 
# NAME: CROSSPROCESS 
# 
# PURPOSE: To apply a color crossprocessing effect to an image.
# 
# DESCRIPTION: CROSSPROCESS applies a color crossprocessing effect to an image 
# by adjusting the sigmoidal-contrast of each of the r,g,b channels of the 
# image. 
# 
# 
# ARGUMENTS: 
# 
# -r ramt ... RAMT is the red sigmoidal-contrast amount. Values are integers 
# between -100 and 100. The default=0 (no change)
# 
# -g gamt ... GAMT is the green sigmoidal-contrast amount. Values are integers 
# between -100 and 100. The default=0 (no change)
#
# -b bamt ... BAMT is the blue sigmoidal-contrast amount. Values are integers 
# between -100 and 100. The default=0 (no change)
#
# -B bright ... BRIGHT is a post process brightness adjust. Values are 
# integers between -100 and 100. The default=0 (no change)
# 
# -C contrast ... CONTRAST is a post process contrast adjust. Values are 
# integers between -100 and 100. The default=0 (no change)
#
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
ramt=0			# red contrast amount; -100<=integer<=100
gamt=0			# green contrast amount; -100<=integer<=100
bamt=0			# blue contrast amount; -100<=integer<=100
bright=0		# post process brightness adjust; -100<=integer<=100
contrast=0		# post process contrast adjust; -100<=integer<=100
div=5			# fixed; scaling factor for contrast amounts

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
elif [ $# -gt 12 ]
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
				-r)    # get ramt
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID RAMT SPECIFICATION ---"
					   #checkMinus "$1"
					   ramt=`expr "$1" : '\([-0-9]*\)'`
					   [ "$ramt" = "" ] && errMsg "--- RAMT=$ramt MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$ramt < -100" | bc`
					   testB=`echo "$ramt > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- RAMT=$ramt MUST BE AN INTEGER BETWEEN -100 AND 100 ---"
					   ;;
				-g)    # get gamt
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID GAMT SPECIFICATION ---"
					   #checkMinus "$1"
					   gamt=`expr "$1" : '\([-0-9]*\)'`
					   [ "$gamt" = "" ] && errMsg "--- GAMT=$gamt MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$gamt < -100" | bc`
					   testB=`echo "$gamt > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- GAMT=$gamt MUST BE AN INTEGER BETWEEN -100 AND 100 ---"
					   ;;
				-b)    # get bamt
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID BAMT SPECIFICATION ---"
					   #checkMinus "$1"
					   bamt=`expr "$1" : '\([-0-9]*\)'`
					   [ "$bamt" = "" ] && errMsg "--- BAMT=$bamt MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$bamt < -100" | bc`
					   testB=`echo "$bamt > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- BAMT=$bamt MUST BE AN INTEGER BETWEEN -100 AND 100 ---"
					   ;;
				-B)    # get bright
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID BRIGHT SPECIFICATION ---"
					   #checkMinus "$1"
					   bright=`expr "$1" : '\([-0-9]*\)'`
					   [ "$bright" = "" ] && errMsg "--- BRIGHT=$bright MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$bright < -100" | bc`
					   testB=`echo "$bright > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- BRIGHT=$bright MUST BE AN INTEGER BETWEEN -100 AND 100 ---"
					   ;;
				-C)    # get contrast
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   #errorMsg="--- INVALID CONTRAST SPECIFICATION ---"
					   #checkMinus "$1"
					   contrast=`expr "$1" : '\([-0-9]*\)'`
					   [ "$contrast" = "" ] && errMsg "--- CONTRAST=$contrast MUST BE A NON-NEGATIVE INTEGER ---"
					   testA=`echo "$contrast < -100" | bc`
					   testB=`echo "$contrast > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- CONTRAST=$contrast MUST BE AN INTEGER BETWEEN -100 AND 100 ---"
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
tmpA1="$dir/crossprocess_1_$$.mpc"
tmpB1="$dir/crossprocess_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1;" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpA2; exit 1" ERR

# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	echo "--- 1 FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"


# process red
if [ "$ramt" != "0" ]; then
	test=`convert xc: -format "%[fx:sign($ramt)]" info:`
	ramt=`convert xc: -format "%[fx:abs($ramt)/$div]" info:`
	if [ $test -eq -1 ]; then
		convert $tmpA1 -channel R +sigmoidal-contrast $ramt,50% +channel $tmpA1
	else
		convert $tmpA1 -channel R -sigmoidal-contrast $ramt,50% +channel $tmpA1
	fi
fi

# process green
if [ "$gamt" != "0" ]; then
	test=`convert xc: -format "%[fx:sign($gamt)]" info:`
	gamt=`convert xc: -format "%[fx:abs($gamt)/$div]" info:`
	if [ $test -eq -1 ]; then
		convert $tmpA1 -channel G +sigmoidal-contrast $gamt,50% +channel $tmpA1
	else
		convert $tmpA1 -channel G -sigmoidal-contrast $gamt,50% +channel $tmpA1
	fi
fi

# process blue
if [ "$bamt" != "0" ]; then
	test=`convert xc: -format "%[fx:sign($bamt)]" info:`
	bamt=`convert xc: -format "%[fx:abs($bamt)/$div]" info:`
	if [ $test -eq -1 ]; then
		convert $tmpA1 -channel B +sigmoidal-contrast $bamt,50% +channel $tmpA1
	else
		convert $tmpA1 -channel B -sigmoidal-contrast $bamt,50% +channel $tmpA1
	fi
fi

convert $tmpA1 -brightness-contrast ${bright},${contrast} "$outfile"

exit 0
