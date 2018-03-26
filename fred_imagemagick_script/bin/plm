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
# USAGE: plm [-s xscale,yscale] [-g] "x1,y1 x2,y2 ..." infile outfile
# USAGE: plm [-s xscale,yscale] [-g] -f point_file infile outfile
# USAGE: plm [-h or -help]
#
# OPTIONS:
# 
# "x1,y1 x2,y2 ..."      break point values of piece-wise linear transformation
#                        enclosed in quotes; minimum of one (x,y) break point pair;
#                        x corresponds to the graylevel in the input image;
#                        y corresponds to the graylevel in the outut image;
#                        x,y non-negative floats enclosed in quotes;  
#                        list must be specified just prior to lutfile
# -f  point_file         text file containing list of break points;
#                        one x,y pair per line
# -s  xscale,yscale      range of values for x,y breakpoint pairs from 0 to xscale,yscale;
#                        positive integers; default=100,100
# -g                     enables the creation of a graph of the piece-wise linear
#                        transformation, which is then displayed automatically.
#                        There is a default setting below that can be changed 
#                        to enable this to be save to a file named outfile_graph.gif
#                         
###
#
# NAME: PLM
#
# PURPOSE: To generate a piece-wise linear mapping grayscale look up table and
# applies it to an image (infile) to generate a new image (outfile)
#
# DESCRIPTION: PLM takes the supplied break point pairs and generates a one 
# dimensional piece-wise linear grayscale look up table image that is used 
# to change grayscale values in the channels of the input image. At least one point 
# pair must be supplied either as an argument or in a text file with one pair per 
# line supplied. The x value is the position in the look up table which is 
# associated with the grayscale value of the desired channels of the input image 
# and the y value is the associated grayscale value for the corresponding channels 
# in the resulting output image.
# 
# OPTIONS: 
# 
# "x1,y1 x2,y2" ... List of x,y break-points for the piece-wise linear transformation.
# The x,y values are non-negative break-point pairs for the piece-wise linear 
# transformation. There is a minimum one point pair that must be supplied. 
# The x and y values are associated with the input and output 
# grayscale values of the look up table, where x,y values can range from 0 to 
# xscale,yscale. xscale determines the length of the 1D lookup table 
# (length=xscale + 1); that is, the range of x (input) values and yscale determines  
# the range of y (output) values. Both xscale and yscale must be positive integers 
# and their defaults are 100. If the first pair is not 0,0 or the last pair is not 
# xscale,yscale then those points will be added automatically to the list of point 
# pairs. IMPORTANT: the list of break-point pairs must be specified just prior to 
# infile outfile.
# 
# -f point_file ... point-file is a text file containing the list of break points,
# one x,y pair per line.
# 
# -s  xscale,yscale ...    xscale,yscale is the range of values for the x,y breakpoint 
# pairs which go from 0 to xscale,yscale; positive integers. The default is 100,100.
# Larger values for xscale and yscale can be used for more accuracy. For example 
# with xscale=yscale=100, x and y values are specified between 0 and 100 and the 
# length of the output image is 101 pixels. For xscale=yscale=255, x and y values 
# are specified # between 0 and 255 and the length of the output image is 256. 
# Equal values for xscale and yscale are not required. 
# 
# -g ... enables the creation of a graph of the piece-wise linear transformation, 
# which is then displayed automatically. There is a default setting below that can 
# be changed to enable this to be save to a file named lutfile_graph.gif. The graph 
# will be scaled to size 100x100. IMPORTANT: To end the script, close/quit the graph 
# image.
# 
# CAVEAT: No guarantee that this script will work on all platforms, nor that 
# trapping of inconsistent parameters is complete and foolproof. Use At Your 
# Own Risk.
# 
######
# 

# set default value for scale and height of lut
xscale=100
yscale=100
height=10
# maxpoints=40	# no longer a limit
decimal=1		# decimal precision for re-listing points and graphing

# set flag if graph is permanent (graph=save) or temporary (graph=view)
graph="view"

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

# function to report error messages, usage and exit
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

# function to test if valid positive integer point pair
testFloatPair()
	{
	v1=`echo $1 | cut -d, -f1`
	v2=`echo $1 | cut -d, -f2`
	test1=`expr "$v1" : '^[.0-9][.0-9]*$'`
	test2=`expr "$v2" : '^[.0-9][.0-9]*$'`
	[ $test1 -eq 0 -o $test2 -eq 0 ] && errMsg "$1 IS NOT A VALID POINT PAIR"
	[ `echo "scale=5; $v1>$xscale" | bc` -eq 1 ] && errMsg "--- XVALUE $v1 EXCEEDS ALLOWED RANGE ---"
	[ `echo "scale=5; $v2>$yscale" | bc` -eq 1 ] && errMsg "--- YVALUE $v1 EXCEEDS ALLOWED RANGE ---"
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
					   exit 0  ;;
				-g)    # display graph
					   display_graph="yes" ;;
				-f)    # text file with point pairs
					   opt=-f
					   shift  # to get the next parameter - point_file
					   # test if parameter starts with minus sign 
					   errorMsg="--- INCORRECT POINT_FILE SPECIFICATION ---"
					   checkMinus "$1"
					   point_file=$1
					   #test if point_file is a valid file
					   [ -f $point_file -a -r $point_file -a -s $point_file ] || errMsg "--- POINT FILE $point_file DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"  ;;
				-s)    # scaling parameter
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INCORRECT XSCALE,YSCALE SPECIFICATION ---"
					   checkMinus "$1"
					   # separate xscale and yscale and test for validity
					   test=`expr "$1" : '^[0-9][0-9]*,[0-9][0-9]*$'`
					   [ $test -eq 0 ] && errMsg "--- XSCALE AND/OR YSCALE ARE NOT INTEGERS ---"
					   xscale=`echo "$1" | cut -d, -f1`
					   yscale=`echo "$1" | cut -d, -f2`  ;;
				 -)    # STDIN and end of arguments
					   break
					   ;;
				-*)    # any other - argument
					   errMsg "--- UNKNOWN OPTION ---"  ;;
				*)     # end of arguments
					   break ;;
			esac
			shift   # next option
	done
fi

# extract and test point pair values
if [ "$opt" = "-f" ]
	then
	# get infile and outfile as the last arguments left
    infile="$1"
    outfile="$2"
	# put the file with line breaks into parm
	parms=`cat $point_file`
	# strip the line breaks (works ONLY if $parm is NOT put into quotes "$parm")
	parm=`echo $parms | grep '.*'`
	# first pattern below replaces all occurrences of commas and spaces with a space => 1 2 3 4 5 6
	# second pattern below replaces the first occurrence of a space with a comma => 1,2[ 3 4][ 5 6] - ignore [], they are for emphasis only
	# third pattern below looks for all space number space number pairs and replaces them with a space followed by number1,number2 => 1,2 3,4 5,6
	set - `echo "$parms" | sed 's/[, ][, ]*/ /g; s/ /,/; s/ \([^ ]*\) \([^ ]*\)/ \1,\2/g'`
	# test for valid integers for x and y
	# keep all points from file
	index=0
	plist=""
	while [ $# -gt 0 ]
		do
		testFloatPair $1
		plist="$plist $1"
		shift
		index=`expr $index + 1`
	done
	#remove leading space
	plist=`echo "$plist" | sed -n 's/ [ ]*\(.*\)/\1/p'`
	[ "$plist" = "" ] && errMsg "--- NO POINT PAIRS WERE PROVIDED ---"
#	[ $# -lt 1 -o $# -gt $maxpoints ] && errMsg "--- NUMBER ($#) OF POINT PAIRS MUST BE BETWEEN 1 AND $maxpoints ---"
else
	# get plist, infile and outfile
	parms="$1"
	infile="$2"
	outfile="$3"
	# first pattern below replaces all occurrences of commas and spaces with a space => 1 2 3 4 5 6
	# second pattern below replaces the first occurrence of a space with a comma => 1,2[ 3 4][ 5 6] - ignore [], they are for emphasis only
	# third pattern below looks for all space number space number pairs and replaces them with a space followed by number1,number2 => 1,2 3,4 5,6
	set - `echo "$parms" | sed 's/[, ][, ]*/ /g; s/ /,/; s/ \([^ ]*\) \([^ ]*\)/ \1,\2/g'`
	# test for valid integers for x and y
	# keep all but last argument which is then set to the lutfile
	index=0
	plist=""
	while [ $# -gt 0 ]
		do
		testFloatPair $1
		plist="$plist $1"
		shift
		index=`expr $index + 1`
	done
	#remove leading space from plist
	plist=`echo "$plist" | sed -n 's/ [ ]*\(.*\)/\1/p'`
	[ "$plist" = "" ] && errMsg "--- NO POINT PAIRS WERE PROVIDED ---"
#	[ $# -lt 1 -o $# -gt $maxpoints ] && errMsg "--- NUMBER ($#) OF POINT PAIRS MUST BE BETWEEN 1 AND $maxpoints ---"
fi

# setup temporary images and auto delete upon exit
# use mpc/cache to hold input image temporarily in memory
tmpA="$dir/plm_$$.mpc"
tmpB="$dir/plm_$$.cache"
tmp1="$dir/plm_1_$$.png"
# get outfile name before suffix
outname=`echo "$outfile" | sed -n 's/^\([^.]*\)[.][^.]*$/\1/ p'`
gg="_graph"
tmp2="$dir/$outname$gg.gif"
if [ "$graph" = "view" ] 
	then 
	trap "rm -f $tmpA $tmpB $tmp1; exit 0" 0
	trap "rm -f $tmpA $tmpB $tmp1; exit 1" 1 2 3 15
elif [ "$graph" = "save" ]
	then
	trap "rm -f $tmpA $tmpB; exit 0" 0
	trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
else
	errMsg "--- NOT A VALID GRAPH DISPLAY OPTION ---"
fi

# test that infile provided
[ "$infile" = "" ] && errMsg "--- NO INPUT FILE SPECIFIED ---"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "--- NO OUTPUT FILE SPECIFIED ---"

# test that last point pair not used instead of infile
test=`expr "$infile" : '^[0-9]*,.*$'`
[ $test -gt 0 ] && errMsg "--- INPUT/OUTPUT FILES IMPROPERLY SPECIFIED ---"

# test input image
if convert -quiet "$infile" +repage "$tmpA"
	then
	: 'do nothing special'
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi


# now process the point pairs

# augment plist so that add 0,0 and/or xscale,yscale if plist does not span 0 to xscale
count=`echo "$plist" | wc -w`
if [ $count -eq 1 ]; then
	firstx=`echo "$plist" | sed -n 's/^\([.0-9]*\),.*$/\1/p'`
	lastx=$firstx
	lasty=`echo "$plist" | sed -n 's/^.*,\([.0-9]*\)$/\1/p'`
elif [ $count -gt 1 ]; then	
	firstx=`echo "$plist" | sed -n 's/^\([.0-9]*\),.*$/\1/p'`
	lastx=`echo "$plist" | sed -n 's/^.* \([.0-9]*\),[.0-9]*$/\1/p'`
	lasty=`echo "$plist" | sed -n 's/^.*,\([.0-9]*\)$/\1/p'`
else
	errMsg="--- INCORRECT NUMBER OF POINT PAIRS SUPPLIED ---"
fi
[ `echo "$firstx != 0" | bc` -eq 1 ] && plist="0,0 $plist"
[ `echo "$lastx != $xscale" | bc` -eq 1 ] && plist="$plist $xscale,$yscale"

# augment plist so that forms closed polygon
# note duplicate points do not matter
plistnew="$plist $xscale,0"

# *************
# echo "plistnew=$plistnew"
# *************

# create and apply lut
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`
if [ "$im_version" -ge "06030507" ]
	then 
	proc="-clut"
else
	proc="-fx 'v.p{u*v.w,0}'"
fi
# draw white polygon on black background
# remove first row corresponding to zero as cannot have any white on this row.
# then box average the columns to get the lut and apply it to the image.
ww=`convert xc: -format "%[fx:$xscale+1]" info:`
hh=`convert xc: -format "%[fx:$yscale+1]" info:`
convert $tmpA \
	\( -size ${ww}x${hh} xc:black -fill white \
	-draw "polygon $plistnew" \
	-crop ${ww}x${yscale}+0+1 +repage \
	-scale ${ww}x1! \) \
	$proc "$outfile"

# display graph if option -g
if [ "$display_graph" = "yes" ]
	then
	# create pair array
	pairArray=($plist)
	numpairs=${#pairArray[*]}
	
	# get xArray and yArray
	i=0
	while [ $i -lt $numpairs ]
		do
		xArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f1`
		yArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f2`
		i=`expr $i + 1`
	done
	
	# scale xArray and yArray
	i=0
	while [ $i -lt $numpairs ]
		do
		xArray[$i]=`echo "scale=$decimal; 100 * ${xArray[$i]} / $xscale" | bc`
		yArray[$i]=`echo "scale=$decimal; 100 * ${yArray[$i]} / $yscale" | bc`
		i=`expr $i + 1`
	done
	
	# regenerate scaled pairArray
	i=0
	while [ $i -lt $numpairs ]
		do
		pairArray[$i]=${xArray[$i]},${yArray[$i]}
		i=`expr $i + 1`
	done
		
	# regenerate scaled list
	points=${pairArray[*]}
	
	# create and display graph
echo "Break Points = $points"
	convert -size 150x150 xc: -fill white -stroke black -draw "rectangle 40,10 141,112" $tmp1
	convert $tmp1 \( -size 100x101 xc: -stroke red -fill white -draw "polyline $points" -flip \) -compose over -geometry 100x101+41+11 -composite $tmp1
	convert $tmp1 -font Arial -pointsize 10 -draw "text 30,122 '0' text 20,17 '100' text 20,17 '100' text 40,60 '_' text 27,65 '50' text 90,112 '|' text 85,125 '50' text 70,140 'i n p u t'" $tmp1
	convert -respect-parenthesis $tmp1 \( -background white -fill black -font Arial -pointsize 10 -gravity center label:'o \nu \nt \np \nu \nt ' -trim \) -geometry +10+20 -compose over -composite $tmp1
	display $tmp1
fi
exit 0
