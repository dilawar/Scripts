#!/bin/bash
#
# Developed by Fred Weinhaus 12/28/2015 .......... revised 12/28/2015
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
# USAGE: furrowed [-a amount] [-s smoothing] [-g gain ] [-b brightness ] 
# [-S saturation ] infile outfile
# USAGE: furrowed [-h or -help]
# 
# OPTIONS:
# 
# -a     amount         amount of furrowing; float>=0; default=2
# -s     smoothing      smoothing; float>=0; must be less than amount; 
#                       default=0;                       
# -g     gain           gain; float>=0; default=10
# -b     brightness     brightness; integer>=0; default=100 (no change)
# -S     saturation     saturation; integer>=0; default=30
# 
###
# 
# NAME: FURROWED 
# 
# PURPOSE: To apply a furrowed-looking effect to an image creating a "gritty" look.
# 
# DESCRIPTION: FURROWED applies a furrowed-looking effect to an image to 
# enhance wrinkles creating a "gritty" look.
# 
# ARGUMENTS: 
# 
# -a amount ... AMOUNT of furrowing. Values are floats>=0. Amount must be 
# larger than smoothing. The default=2.
# 
# -s smoothing ... SMOOTHING. Values are floats floats>=0; Smoothing must be 
# less than amount. The default=0 (no smoothing).
#                        
# -g gain ... GAIN. Values are floats>=0. The default=10.
# 
# -b brightness ... BRIGHTNESS. Values are integers>=0. The default=100 
# (no change).
# 
# -s saturation ... SATURATION. Values are integer>=0. A value of 100 is no 
# change. The default=30.
# 
# REQUIREMENTS: IM 6.6.1.9 due to the use of -morphology DoG
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
amount=2			# float>=0
smoothing=0			# float>=0
gain=10				# float>=0
brightness=100      # integer>=0
saturation=50		# integer>=0

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
				-a)    # get  amount
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT SPECIFICATION ---"
					   checkMinus "$1"
					   amount=`expr "$1" : '\([.0-9]*\)'`
					   [ "$amount" = "" ] && errMsg "--- AMOUNT=$amount MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   ;;
				-s)    # get  smoothing
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SMOOTHING SPECIFICATION ---"
					   checkMinus "$1"
					   smoothing=`expr "$1" : '\([.0-9]*\)'`
					   [ "$smoothing" = "" ] && errMsg "--- SMOOTHING=$smoothing MUST BE A NON-NEGATIVE FLOAT VALUE ---"
					   ;;
				-g)    # get  gain
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GAIN SPECIFICATION ---"
					   checkMinus "$1"
					   gain=`expr "$1" : '\([.0-9]*\)'`
					   [ "$gain" = "" ] && errMsg "--- GAIN=$gain MUST BE A NON-NEGATIVE FLOAT VALUE (with no sign) ---"
					   ;;
				-b)    # get brightness
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BRIGHTNESS SPECIFICATION ---"
					   checkMinus "$1"
					   brightness=`expr "$1" : '\([0-9]*\)'`
					   [ "$brightness" = "" ] && errMsg "--- BRIGHTNESS=$brightness MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   ;;
				-S)    # get saturation
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SATURATION SPECIFICATION ---"
					   checkMinus "$1"
					   saturation=`expr "$1" : '\([0-9]*\)'`
					   [ "$saturation" = "" ] && errMsg "--- SATURATION=$saturation MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
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
tmpA1="$dir/furrowed_1_$$.mpc"
tmpB1="$dir/furrowed_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1;" 0
trap "rm -f $tmpA1 $tmpB1; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1; exit 1" ERR

# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- 1 FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"


# process image
convert $tmpA1 -auto-level \
	\( -clone 0 -colorspace gray \) \
	\( -clone 1 -bias 50% -define convolve:scale=$gain! -morphology Convolve DoG:0,${smoothing},${amount} \) \
	+swap -compose overlay -composite \
	-modulate ${brightness},${saturation},100 \
	"$outfile"


exit 0

