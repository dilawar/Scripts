#!/bin/bash
#
# Developed by Fred Weinhaus 9/30/2012 .......... revised 4/25/2015
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
# USAGE: filmgrain [-n noise] [-a amount1] [-A amount2] [-d density1] 
# [-D density2] [-s seed1] [-S seed2] [-c compose1] [-C compose2] 
# [-b blur] [-m monotone] infile outfile
# 
# USAGE: filmgrain [-h or -help]
# 
# OPTIONS:
# 
# -n     noise        type of noise; any valid IM noise, except random; 
#                     default=gaussian
# -a     amount1      amount of the first layer of noise; integer>=0; 
#                     default=100
# -A     amount2      amount of the second layer of noise; integer>=0; 
#                     default=50
# -d     density1     density of the first layer of noise; 0<=integer<=100 
#                     default=100
# -D     density2     density of the second layer of noise; 0<=integer<=100 
#                     default=100
# -s     seed1        seed value for the first layer of noise; integer>=0; 
#                     default=100
# -S     seed2        seed value for the second layer of noise; integer>=0; 
#                     default=200
# -c     compose1     compose method for the first layer of noise; choices are:
#                     overlay, softlight, hardlight, linearlight, vividlight; 
#                     default=overlay
# -C     compose2     compose method for the second layer of noise; choices are:
#                     overlay, softlight, hardlight, linearlight, vividlight; 
#                     default=overlay
# -b     blur         blur amount to soften noise; float>=0; default=0.5
# -m     monotone     use monotone (grayscale) noise rather than color noise;
#                     choices are yes or no.
# 
###
# 
# NAME: FILMGRAIN 
# 
# PURPOSE: To apply film grain noise to an image.
# 
# DESCRIPTION: FILMGRAIN applies film grain noise to an image. One or two 
# layers of noise may be applied. The amount and density of each layer of 
# noise may be controlled separately as well as the compose methods.
# 
# 
# ARGUMENTS: 
# 
# -n noise ... NOISE is the type of noise; any valid IM noise, except random 
# is allowed. The default=gaussian. Most of the noise types look rather 
# similar on first glance with the appropriate amounts. Isolated noise can be 
# simulated by reducing the density on the other noise types. Actual film grain 
# is closestly approximated by the gaussian noise.
# 
# -a amount1 ... AMOUNT1 is the amount of the first layer of noise. Value are
# integers>=0. The default=100.
# 
# -A amount2 ... AMOUNT2 is the amount of the second layer of noise. Value are
# integers>=0. The default=50.
# 
# -d density1 ... DENSITY1 is the density (spacing) of the first layer of 
# noise. Values are 0<=integers<=100. The default=100.
# 
# -D densityD ... DENSITY2 is the density (spacing) of the second layer of 
# noise. Values are 0<=integers<=100. The default=100.
# 
# -s seed1 ... SEED1 is the seed value for the first layer of noise. Values are 
# integers>=0. The default=100.
# 
# -S seed2 ... SEED2 is the seed value for the second layer of noise. Values are 
# integers>=0. The default=200.
# 
# -c compose1 ... COMPOSE1 is the compose method for the first layer of noise. 
# Choices are: overlay, softlight, hardlight, linearlight and vividlight. 
# The default=overlay.
# 
# -C compose2 ... COMPOSE2 is the compose method for the second layer of noise. 
# Choices are: overlay, softlight, hardlight, linearlight and vividlight. 
# The default=overlay.
# 
# -b blur ... BLUR amount to soften noise. Values are floats>=0. The 
# default=0.5.
# 
# -m monotone ... MONOTONE indicates to use monotone (grayscale) noise rather 
# than color noise. Choices are: yes or no.
# 
# REFERENCES:
# http://en.wikipedia.org/wiki/Image_noise#Film_grain
# http://www.ephotozine.com/article/photoshop-tutorial--add-film-grain-to-images-13443
# http://www.photoshopessentials.com/photo-effects/film-grain/
# http://photoshoptutorials.ws/photoshop-tutorials/photo-effects/natural-film-grain/?singlepage=1
# http://www.digiretus.com/tippek/cikkiro.php?SORSZAM=79
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
noise="gaussian"
amount1=100						# first layer of noise; integer>=0
amount2=50						# second layer of noise; integer>=0
density1=100					# density for first layer of noise; 0<=integer<=100
density2=100					# density for second layer of noise; 0<=integer<=100
seed1=100						# seed for for first layer of noise; integer>=0
seed2=200						# seed for for second layer of noise; integer>=0
compose1="overlay"				# compose method for first layer of noise; overlay, softlight, hardlight, linearlight, vividlight
compose2="overlay"				# compose method for second layer of noise; overlay, softlight, hardlight, linearlight, vividlight
blur=0.5						# noise blur to soften; float>=0
monotone="yes"					# yes or no; no mean colored noise, yes means grayscale noise

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
elif [ $# -gt 24 ]
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
				-n)    # get  noise
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NOISE SPECIFICATION ---"
					   checkMinus "$1"
					   noise=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$noise" in 
					   		gaussian) ;;
					   		impulse) ;;
					   		laplacian) ;;
					   		multiplicative) ;;
					   		poisson) ;;
					   		uniform) ;;
					   		*) errMsg "--- NOISE=$noise IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-a)    # get amount1
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT1 SPECIFICATION ---"
					   checkMinus "$1"
					   amount1=`expr "$1" : '\([0-9]*\)'`
					   [ "$amount1" = "" ] && errMsg "--- AMOUNT1=$amount1 MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-A)    # get amount2
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT2 SPECIFICATION ---"
					   checkMinus "$1"
					   amount2=`expr "$1" : '\([0-9]*\)'`
					   [ "$amount2" = "" ] && errMsg "--- AMOUNT2=$amount2 MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-d)    # get density1
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DENSITY1 SPECIFICATION ---"
					   checkMinus "$1"
					   density1=`expr "$1" : '\([0-9]*\)'`
					   [ "$density1" = "" ] && errMsg "--- DENSITY1=$density1 MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$density1 < 0" | bc`
					   test2=`echo "$density1 > 100" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- DENSITY1=$density1 MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-D)    # get density2
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DENSITY2 SPECIFICATION ---"
					   checkMinus "$1"
					   density2=`expr "$1" : '\([0-9]*\)'`
					   [ "$density2" = "" ] && errMsg "--- DENSITY2=$density2 MUST BE A NON-NEGATIVE INTEGER VALUE (with no sign) ---"
					   test1=`echo "$density2 < 0" | bc`
					   test2=`echo "$density2 > 100" | bc`
					   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- DENSITY2=$density2 MUST BE AN INTEGER BETWEEN 0 AND 100 ---"
					   ;;
				-s)    # get seed1
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SEED1 SPECIFICATION ---"
					   checkMinus "$1"
					   seed1=`expr "$1" : '\([0-9]*\)'`
					   [ "$seed1" = "" ] && errMsg "--- SEED1=$seed1 MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-S)    # get seed2
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SEED2 SPECIFICATION ---"
					   checkMinus "$1"
					   seed2=`expr "$1" : '\([0-9]*\)'`
					   [ "$seed2" = "" ] && errMsg "--- SEED2=$seed2 MUST BE A NON-NEGATIVE INTEGER ---"
					   ;;
				-c)    # get  compose1
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COMPOSE1 SPECIFICATION ---"
					   checkMinus "$1"
					   compose1=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$compose1" in 
					   		overlay) ;;
					   		hard_light|hard-light|hardlight) ;;
					   		soft_light|soft-light|softlight) ;;
					   		linear_light|linear-light|linearlight) ;;
					   		vivid_light|vivid-light|vividlight) ;;
					   		*) errMsg "--- COMPOSE1=$compose1 IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-C)    # get  compose2
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COMPOSE2 SPECIFICATION ---"
					   checkMinus "$1"
					   compose2=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$compose2" in 
					   		overlay) ;;
					   		hard_light|hard-light|hardlight) ;;
					   		soft_light|soft-light|softlight) ;;
					   		linear_light|linear-light|linearlight) ;;
					   		vivid_light|vivid-light|vividlight) ;;
					   		*) errMsg "--- COMPOSE2=$compose2 IS AN INVALID VALUE ---"  ;;
					   esac
					   ;;
				-b)    # get blur
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BLUR SPECIFICATION ---"
					   checkMinus "$1"
					   blur=`expr "$1" : '\([.0-9]*\)'`
					   [ "$blur" = "" ] && errMsg "--- BLUR=$grbluray MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-m)    # get  monotone
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MONOTONE SPECIFICATION ---"
					   checkMinus "$1"
					   monotone=`echo "$1" | tr '[A-Z]' '[a-z]'`
					   case "$monotone" in 
					   		yes) ;;
					   		no) ;;
					   		*) errMsg "--- MONOTONE=$monotone IS AN INVALID VALUE ---"  ;;
					   esac
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


# set directory for temporary files
# tmpdir="/tmp"
tmpdir="."

dir="$tmpdir/FILMGRAIN.$$"

mkdir "$dir" || errMsg "--- FAILED TO CREATE TEMPORARY FILE DIRECTORY ---"
trap "rm -rf $dir;" 0
trap "rm -rf $dir; exit 1" 1 2 3 15
trap "rm -rf $dir; exit 1" ERR


# test input image
convert -quiet "$infile" +repage  "$dir/tmpI.mpc" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE  ---"

amt1=`convert xc: -format "%[fx:$amount1/100]" info:`
amt2=`convert xc: -format "%[fx:$amount2/100]" info:`


# create midgray image
convert $dir/tmpI.mpc -fill "gray(50%)" -colorize 100% $dir/tmpG.mpc

# set up for monotone (grayscale) noise
if [ "$monotone" = "yes" ]; then
	monoproc="-set colorspace RGB -channel G -separate +channel"
else
	monoproc=""
fi


# set up for density
if [ "$density1" = "100" ]; then
	densityproc1=""
else
	fuzzval1=`convert xc: -format "%[fx:(100-$density1)/2]" info:`
	densityproc1="-fuzz $fuzzval1% -fill gray(50%) -opaque gray(50%)"
fi
if [ "$density2" = "100" ]; then
	densityproc2=""
else
	fuzzval2=`convert xc: -format "%[fx:(100-$density2)/2]" info:`
	densityproc2="-fuzz $fuzzval2% -fill gray(50%) -opaque gray(50%)"
fi



if [ "$amount2" = "0" ]; then
	convert $dir/tmpI.mpc \
	\( $dir/tmpG.mpc -attenuate $amt1 -seed $seed1 +noise $noise $monoproc $densityproc1 -blur 0x$blur \) \
	\( -clone 0 -clone 1 -compose $compose1 -composite \) \
	-delete 0,1 "$outfile"

elif [ "$amount2" != "0" ]; then
	convert $dir/tmpI.mpc \
		\( $dir/tmpG.mpc -attenuate $amt1 -seed $seed1 +noise $noise $monoproc $densityproc1 -blur 0x$blur \) \
		\( -clone 0 -clone 1 -compose $compose1 -composite \) \
		\( $dir/tmpG.mpc -attenuate $amt2 -seed $seed2 +noise $noise $monoproc $densityproc2 -blur 0x$blur \) \
		\( -clone 2 -clone 3 -compose $compose2 -composite \) \
		-delete 0-3 "$outfile"

fi	

exit 0


