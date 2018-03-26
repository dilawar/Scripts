#!/bin/bash
#
# Developed by Fred Weinhaus 11/2/2013 .......... revised 5/28/2015
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
# USAGE: splice [-t top] [-b bottom] [-g gap] [-v vertices] [-m maxoffset] 
# [-s seed] [-a amount] [-d distance] [-o opacity] [-c color] infile outfile
# USAGE: splice [-h or -help]
#
# OPTIONS:
#
# -t    top           top section height; percent or pixels; 
#                     0<integer<image height (or 100%); default=30%
# -b    bottom        bottom section height; percent or pixels; 
#                     0<integer<image height (or 100%); default=30%
# -g    gap           gap in pixels between top and bottom sections; 
#                     integer>=0; default=10
# -v    vertices      number of vertices between sides (excluding end points); 
#                     integer>=0; default=8
# -m    maxoffset     maximum vertex pixel offset in the vertical direction; 
#                     integer>=0; default=15
# -s    seed          random number seed for vertex vertical offsets; 
#                     integer>=0; default=300
# -a    amount        amount of blur for shadow; integer>=0; default=4
# -d    distance      shadow distance in pixels; integer>=0; default=4
# -o    opacity       shadow opacity; 0<=integer<=100; default=100; if set to 
#                     zero, then no shadow will be added
# -c    color         background color; any valid IM color is allowed; 
#                     default=white
# 
###
#
# NAME: SPLICE 
# 
# PURPOSE: To apply a jagged cut and splice effect to an image.
# 
# DESCRIPTION: SPLICE applies a jagged cut and splice effect to an image. A 
# shadow effect is optional, but applied by default.
# 
# OPTIONS: 
# 
# -t top ... TOP is the top section height in percent or pixels. Values are 
# 0<integer<image height (or 100%). The default=30%. Note that the top and  
# bottom sections typically add up to 100% or less of the original image.
# 
# -b bottom ... BOTTOM is the bottom section height in percent or pixels.  
# Values are 0<integer<image height (or 100%). The default=30%. Note that the  
# top and bottom sections typically add up to 100% or less of the original image.
# 
# -g gap ... GAP is the gap in pixels between the top and bottom sections. 
# Values are integer>=0. The default=10.
# 
# -v vertices ... VERTICES are the number of vertices between left and right 
# sides of the image (excluding start and end points). Values are integer>=0. 
# The default=8.
# 
# -m maxoffset ... MAXOFFSET is the maximum vertex pixel offset in the 
# vertical direction. Values are integer>=0. The default=15.
# 
# -s seed ... SEED is the random number seed for the vertex vertical offsets.  
# Values are integer>=0. The default=300.
# 
# -a amount ... AMOUNT is the amount of blur for the shadow. Value are 
# integer>=0. The default=4.
#
# -d distance ... DISTANCE is the shadow distance in pixels. Values are 
# integer>=0. The default=4.
# 
# -o opacity ... OPACITY is the shadow opacity. Values are 0<=integer<=100. 
# The default=100. If opacity is set to zero, then no shadow will be added.
# 
# -c color ... COLOR is the background color. Any valid IM color is allowed. 
# The default=white.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
top="30%"		# top height (percent or pixels)
bottom="30%"	# bottom height (percent or pixels)
gap=10			# gap in pixels
vertices=8		# number of vertices between sides
maxoffset=15	# max vertex offset in vertical direction
seed="300"		# random seed
amount=4		# amount of blur for shadow
distance=4		# shadow distance
opacity=100		# shadow opacity
color=white		# background color


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
elif [ $# -gt 22 ]
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
				-t)    # get top
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID TOP SPECIFICATION ---"
					   checkMinus "$1"
					   top=`expr "$1" : '\([\%0-9]*\)'`
					   [ "$top" = "" ] && errMsg "TOP=$top MUST BE A NON-NEGATIVE INTEGER WITH OR WITHOUT A %"
					   ;;
				-b)    # get bottom
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BOTTOM SPECIFICATION ---"
					   checkMinus "$1"
					   bottom=`expr "$1" : '\([\%0-9]*\)'`
					   [ "$bottom" = "" ] && errMsg "BOTTOM=$bottom MUST BE A NON-NEGATIVE INTEGER WITH OR WITHOUT A %"
					   ;;
				-g)    # get gap
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GAP SPECIFICATION ---"
					   checkMinus "$1"
					   gap=`expr "$1" : '\([0-9]*\)'`
					   [ "$gap" = "" ] && errMsg "GAP=$gap MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-v)    # get vertices
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID VERTICES SPECIFICATION ---"
					   checkMinus "$1"
					   vertices=`expr "$1" : '\([0-9]*\)'`
					   [ "$vertices" = "" ] && errMsg "VERTICES=$vertices MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-m)    # get maxoffset
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MAXOFFSET SPECIFICATION ---"
					   checkMinus "$1"
					   maxoffset=`expr "$1" : '\([0-9]*\)'`
					   [ "$maxoffset" = "" ] && errMsg "MAXOFFSET=$maxoffset MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-s)    # get seed
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SEED SPECIFICATION ---"
					   checkMinus "$1"
					   seed=`expr "$1" : '\([0-9]*\)'`
					   [ "$seed" = "" ] && errMsg "SEED=$seed MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-a)    # get amount
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID AMOUNT SPECIFICATION ---"
					   checkMinus "$1"
					   amount=`expr "$1" : '\([0-9]*\)'`
					   [ "$amount" = "" ] && errMsg "AMOUNT=$amount MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-d)    # get distance
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DISTANCE SPECIFICATION ---"
					   checkMinus "$1"
					   distance=`expr "$1" : '\([0-9]*\)'`
					   [ "$distance" = "" ] && errMsg "DISTANCE=$distance MUST BE A NON-NEGATIVE INTEGER"
					   ;;
				-o)    # get opacity
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID OPACITY SPECIFICATION ---"
					   checkMinus "$1"
					   opacity=`expr "$1" : '\([0-9]*\)'`
					   [ "$opacity" = "" ] && errMsg "OPACITY=$opacity MUST BE A NON-NEGATIVE INTEGER"
		   			   testA=`echo "$opacity < 0" | bc`
		   			   testB=`echo "$opacity > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- OPACITY=$opacity MUST BE AN INTEGER BETWEEEN 0 AND 100 --- "
					   ;;
				-c)    # get color
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   color="$1"
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
tmpA1="$dir/splice_1_$$.mpc"
tmpB1="$dir/splice_1_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2; exit 1" ERR


# read the input image into the temporary cached image and test if valid
convert -quiet "$infile" +repage "$tmpA1" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO size  ---"

# get width and height of image and horizontal vertex increments
ww=`convert $tmpA1 -format "%w" info:`
hh=`convert $tmpA1 -format "%h" info:`
wl=$((ww-1))
hl=$((hh-1))
num=$((vertices+2))
inc=`convert xc: -format "%[fx:$wl/($num-1)]" info:`	

# get top and bottom location of splice
testtop=`echo $top | grep "\%"`
testbottom=`echo $bottom | grep "\%"`
if [ "$testtop" = "" ]; then
	yt=$top
else
	top=`echo "$top" | sed 's/\%//g'`
	yt=`convert xc: -format "%[fx:round($hh*$top/100)]" info:`
fi
if [ "$testtop" = "" ]; then
	yb=$((hh-bottom))
else
	bottom=`echo "$bottom" | sed 's/\%//g'`
	yb=`convert xc: -format "%[fx:round($hh*(100-$bottom)/100)]" info:`
fi
#echo "yt=$yt; yb=$yb"

# trap for too small or too large sections
[ $top -le 0 ] && errMsg = "--- TOP SECTION SIZE IS INVALID ---"
[ $bottom -le 0 ] && errMsg = "--- BOTTOM SECTION SIZE IS INVALID ---"
[ $((yt+maxoffset)) -gt $hh ] && errMsg = "--- TOP SECTION IS TOO BIG ---"
[ $(($hh-yb+maxoffset)) -gt $hh ] && errMsg = "--- BOTTOM SECTION IS TOO BIG ---"


# compute x array for top and bottom of gap and vertical offsets and largest positive (mxoff) and largest negative (mnoff) offsets
mxoff=0
mnoff=0
last=$((num-1))
for ((i=0; i<num; i++)); do
	[ "$seed" = "" ] && seeding="" || seeding="-seed $seed"
	x_array[i]=`convert xc: -format "%[fx:round($i*$inc)]" info:`
	offset_array[i]=`convert xc: $seeding -format "%[fx:pow(-1,$i)*round($maxoffset*random())]" info:`
	off=${offset_array[$i]}
	[ $i -ne 0 -a $i -ne $last -a $off -gt $mxoff ] && mxoff=$off
	[ $i -ne 0 -a $i -ne $last -a $off -lt $mnoff ] && mnoff=$off
	#echo "i=$i; off=$off; mxoff=$mxoff; mnoff=$mnoff;"
	seed=$((seed+314*(i+1)))
done
#echo "max=$mxoff; min=$mnoff;"

# reset offset to 0 for sides of image
offset_array[0]=0
offset_array[$num-1]=0
#echo "${x_array[*]}"
#echo "${offset_array[*]}"


# compute y array for top and bottom of gap
for ((i=0; i<num; i++)); do
	yt_array[i]=`convert xc: -format "%[fx:round($yt+${offset_array[$i]})]" info:`
	yb_array[i]=`convert xc: -format "%[fx:round($yb+${offset_array[$i]})]" info:`
done
#echo "${yt_array[*]}"
#echo "${yb_array[*]}"

# combine vertices into point list
points_top=""
points_bottom=""
for ((i=0; i<num; i++)); do
	points_top="$points_top ${x_array[$i]},${yt_array[$i]}"
	points_bottom="$points_bottom ${x_array[$i]},${yb_array[$i]}"
done

# add points around corners
points_top="$points_top $wl,0 0,0"
points_bottom="$points_bottom $wl,$hl 0,$hl"
#echo "$points_top"
#echo "$points_bottom"


# compute crop parameters
# note mnoff is negative
ht=$((yt+1+mxoff))
hb=$((hh-(yb+mnoff)))



# compute repage offset (shift) for top of bottom section relative to top of top section considering max offset
shift=$((ht+gap-(mxoff-mnoff)))
pageoffset=`printf "+0+%d" $shift`
#echo "yt=$yt; yb=$yb; ht=$ht; hb=$hb; mxoff=$mxoff; mnoff=$mnoff; gap=$gap; shift=$shift;"


# create mask, put into alpha channel, trim for top
# create mask, put into alpha channel, trim for bottom
# append
# create shadow
# merge shadow with spliced image
if [ "$opacity" = "0" ]; then
	convert $tmpA1 \
		\( -clone 0 -fill black -colorize 100 -fill white -draw "polygon $points_top" \) \
		\( -clone 0 -clone 1 -alpha off -compose copy_opacity -composite -compose over \
			-gravity north -crop ${ww}x${ht}+0+0 +repage \) -delete 1 \
		\( -clone 0 -fill black -colorize 100 -fill white -draw "polygon $points_bottom" \) \
		\( -clone 0 -clone 2 -alpha off -compose copy_opacity -composite -compose over \
			-gravity south -crop ${ww}x${hb}+0+0 +repage -repage $pageoffset \) -delete 2 \
		-delete 0 -background $color -layers merge "$outfile"
else
	convert $tmpA1 \
		\( -clone 0 -fill black -colorize 100 -fill white -draw "polygon $points_top" \) \
		\( -clone 0 -clone 1 -alpha off -compose copy_opacity -composite -compose over \
			-gravity north -crop ${ww}x${ht}+0+0 +repage \) -delete 1 \
		\( -clone 0 -fill black -colorize 100 -fill white -draw "polygon $points_bottom" \) \
		\( -clone 0 -clone 2 -alpha off -compose copy_opacity -composite -compose over \
			-gravity south -crop ${ww}x${hb}+0+0 +repage -repage $pageoffset \) -delete 2 \
		-delete 0 -background none -layers merge \
		\( -clone 0 -background black -shadow ${opacity}x${amount}+${distance}+${distance} \) \
		+swap -background "$color" -layers merge "$outfile"
fi	

exit 0
