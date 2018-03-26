#!/bin/bash
#
# Developed by Fred Weinhaus 8/18/2007 .......... revised 1/14/2016
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
# USAGE: curves [-s xscale,yscale] [-c channel] [-g graphmode] "x1,y1 x2,y2 ..." infile outfile
# USAGE: curves [-s xscale,yscale] [-c channel] [-g graphmode] -f point_file infile outfile
# USAGE: curves [-h or -help]
#
# OPTIONS:
# 
# "x1,y1 x2,y2 ..."         break point values of piece-wise linear transformation
#                           enclosed in quotes; minimum of one (x,y) break point pair;
#                           x corresponds to the graylevel in the input image;
#                           y corresponds to the graylevel in the outut image;
#                           x,y non-negative floats enclosed in quotes;  
#                           list must be specified just prior to infile
# -c     channel            channel to process; options are: red, green, blue, 
#                           lightness, brightness, luma and luminosity; 
#                           default=all rgb channels
# -f     point_file         text file containing list of break points;
#                           one x,y pair per line
# -s     xscale,yscale      range of values for x,y breakpoint pairs from 0 to xscale,yscale;
#                           positive integers; default=100,100
# -g     graphmode          enables the creation of a graph of the transformation
#                           and optionally the original break point, which is then 
#                           displayed automatically. The choices are: curve (C) or 
#                           points (P), where the latter shows the original user 
#                           specified break points in addition to the curve. The 
#                           default is curve (C). There is a default setting below 
#                           that can be changed to enable this to be save to a file 
#                           named outfile_graph.gif
#                         
###
#
# NAME: CURVES
#
# PURPOSE: To generate a smoothly interpolated, curved mapping look up table 
# from a set of break point and applies it to an image.
#
# DESCRIPTION: CURVES takes the supplied break point pairs and generates a  
# dimensional spline curve that is then used to create a 1-D look up table image. 
# The look up table image is then used to change grayscale values in the channels 
# of the input image. At least one point pair must be supplied either as an argument 
# or in a text file with one pair per line supplied. The x value is the position in the 
# look up table which is associated with the grayscale value of the desired channels of 
# the input image and the y value is the associated grayscale value for the corresponding 
# channels in the resulting output image. A Catmull-Rom spline is used to generate the 
# the curve and does go through the supplied point pairs.
# 
# OPTIONS: 
# 
# "x1,y1 x2,y2" ... List of x,y break-points for the curved
# transformation. The x,y values are non-negative break-point pairs for
# the curved transformation. There is a minimum one point pair that must
# be supplied. The x and y values are associated with the input and output
# grayscale values of the look up table, where x,y values can range from 0
# to xscale,yscale. xscale determines the length of the 1D lookup table
# (length=xscale + 1); that is, the range of x (input) values and yscale
# determines the range of y (output) values. Both xscale and yscale must
# be positive integers and their defaults are 100. If the first pair is
# not 0,0 or the last pair is not xscale,yscale then those points will be
# added automatically to the list of point pairs. IMPORTANT: the list of
# break-point pairs must be specified just prior to infile outfile.
# 
# -f point_file ... point-file is a text file containing the list of break 
# points, one x,y pair per line.
# 
# -s  xscale,yscale ...    xscale,yscale is the range of values for the
# x,y breakpoint pairs which go from 0 to xscale,yscale; positive
# integers. The default is 100,100. Larger values for xscale and yscale
# can be used for more accuracy. For example with xscale=yscale=100, x and
# y values are specified between 0 and 100 and the length of the output
# image is 101 pixels. For xscale=yscale=255, x and y values are specified
# # between 0 and 255 and the length of the output image is 256. Equal
# values for xscale and yscale are not required.
# 
# -c channel ... CHANNEL to process. Options are: red, green, blue, lightness 
# (L from HSL), brightness (B from HSB), luma (Y from YCbCr) and luminosity 
# (L from Lab). The default=all rgb channels.
#
# -g graphmode... enables the creation of a graph of the curved transformation, 
# which is then displayed automatically. The choices are: curve (C) or 
# points (P), where the latter shows the original user specified break points 
# in addition to the curve. The default is no graph if this option is not used. 
# There is also a default setting below that can be changed to enable this to be 
# save to a file named outfile_graph.gif. The graph will be scaled to size 100x100. 
# IMPORTANT: To end the script, close/quit the graph image.
# 
# REQUIREMENTS: IM 6.7.8.2 or higher when using channel=luminosity.
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
channel=""
height=10
precision=1				# decimal precision
pinc=1					# increment in pixels
graphmode=""            # initialization
crad=2					# point circle radius

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

# function to test if valid positive float point pair
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
elif [ $# -gt 9 ]
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
				-g)    # get  graphmode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GRAPHMODE SPECIFICATION ---"
					   checkMinus "$1"
						case "$1" in
						 curve)		graphmode="curve";;
						 C)			graphmode="curve";;
						 c)			graphmode="curve";;
						 points)	graphmode="points";;
						 P)			graphmode="points";;
						 p)			graphmode="points";;
						 *)         errMsg="--- UNKNOWN GRAPHMODE ---"
						esac
					   ;;
				-c)    # get  channel
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID CHANNEL SPECIFICATION ---"
					   checkMinus "$1"
					   channel=`echo "$1" | tr "[:upper:]" "[:lower:]"`
						case "$channel" in
						 red) ;;
						 green) ;;
						 blue) ;;
						 lightness) ;;
						 brightness) ;;
						 luma) ;;
						 luminosity) ;;
						 *) errMsg="--- UNKNOWN CHANNEL ---"
						esac
					   ;;
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
    infile=$1
    outfile=$2
	# put the file with line breaks into parm
	parms=`cat $point_file`
	# strip the line breaks (works ONLY if $parm is NOT put into quotes "$parm")
	parm=`echo $parms | grep '.*'`
	# first pattern below replaces all occurrences of commas and spaces with a space => 1 2 3 4 5 6
	# second pattern below replaces the first occurrence of a space with a comma => 1,2[ 3 4][ 5 6] - ignore [], they are for emphasis only
	# third pattern below looks for all space number space number pairs and replaces them with a space followed by number1,number2 => 1,2 3,4 5,6
	set - `echo "$parms" | sed 's/[, ][, ]*/ /g; s/ /,/; s/ \([^ ]*\) \([^ ]*\)/ \1,\2/g'`
	# test for valid positive floats for x and y
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
else
	# get plist, infile and outfile
	parms="$1"
	infile="$2"
	outfile="$3"
	# first pattern below replaces all occurrences of commas and spaces with a space => 1 2 3 4 5 6
	# second pattern below replaces the first occurrence of a space with a comma => 1,2[ 3 4][ 5 6] - ignore [], they are for emphasis only
	# third pattern below looks for all space number space number pairs and replaces them with a space followed by number1,number2 => 1,2 3,4 5,6
	set - `echo "$parms" | sed 's/[, ][, ]*/ /g; s/ /,/; s/ \([^ ]*\) \([^ ]*\)/ \1,\2/g'`
	# test for valid positive floats for x and y
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
fi

# setup temporary images and auto delete upon exit
# use mpc/cache to hold input image temporarily in memory
tmpA="$dir/curves_$$.mpc"
tmpB="$dir/curves_$$.cache"
tmp1="$dir/curves_1_$$.png"

# get outfile name before suffix
outname=`echo "$outfile" | sed -n 's/^\([^.]*\)[.][^.]*$/\1/ p'`
gg="_graph"
tmp1="$dir/$outname$gg.gif"

# remove temporaries
if [ "$graph" = "view" ] 
	then 
	trap "rm -f $tmpA $tmpB $tmp1;" 0
	trap "rm -f $tmpA $tmpB $tmp1; exit 1" 1 2 3 15
#	trap "rm -f $tmpA $tmpB $tmp1; exit 1" ERR
elif [ "$graph" = "save" ]
	then
	trap "rm -f $tmpA $tmpB;" 0
	trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
#	trap "rm -f $tmpA $tmpB; exit 1" ERR
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


# cubic spline interpolate using parametric format
# x=a0+a1*s+a2*s^2+a3*s^3
# y=b0+b1*s+b2*s^2+b3*s^3
# Q=C¥S; where Q=(x;y)=polynomial coordinate (column) vector
# and C=(a3 a2 a1 a0; b3 b2 b1 b0)=coefficient matrix with a's and b's as rows; 
# and S=(s^3 s^2 s 1)=parametric (row) vector

# spline formulation often represented as: Q=S¥M¥V
# where M=matrix which depends upon the type of spline 
# and V is the control (column) vector (often either a set of knots or derivatives; knots being data points)
# x=(s^3 s^2 s 1)¥M¥Vx where Vx is (column) vector of four components (knots, tangents or contol point)
# y=(s^3 s^2 s 1)¥M¥Vx where Vx is (column) vector of four components (knots, tangents or control points)
# (alternate form is P=Vt¥Mt*Vt; Vt=V transposed so is a row vector; Mt=M transposed)

# see references:
# http://icie.cs.byu.edu/UIBook/12-2DGeometry.pdf
# http://www.cs.cornell.edu/Courses/cs465/2004fa/lectures/15splines/15splines.pdf
# http://evasion.inrialpes.fr/Membres/Francois.Faure/enseignement/M2R-Animation/interpolation.pdf
# http://www.csie.ntu.edu.tw/~cyy/amt/lec03.ppt
# http://www.cg.tuwien.ac.at/courses/Animation/c.ps
# http://cubic.org/docs/hermite.htm
# http://local.wasp.uwa.edu.au/~pbourke/other/interpolation/
# http://wiki.tcl.tk/10530

# catmull-rom vector of knots and matrix
# V=(K0 K1 K2 K3); and interpolate between K1 and K2
# -1  3 -3  1
#  2 -5  4 -1
# -1  0  1  0
#  0  2  0  0


# start processing
echo ""
echo "Please Wait"
echo ""


# augment plist to add 0,0 and/or xscale,yscale if plist does not span 0 to xscale
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


# convert plist into pairArray for processing of spline
pairArray=($plist)
np=${#pairArray[*]}
npm1=`expr $np - 1`
npm2=`expr $np - 2`

# separate x and y components of pairArray
i=0
while [ $i -lt $np ]; do
	xArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f1`
	yArray[$i]=`echo "${pairArray[$i]}" | cut -d, -f2`
	i=`expr $i + 1`
done

# augment pairArray with linearly extended first and last point
pleftx=`echo "scale=6; 2*${xArray[0]}-${xArray[1]}/1" | bc`
plefty=`echo "scale=6; 2*${yArray[0]}-${yArray[1]}/1" | bc`
prightx=`echo "scale=6; 2*${xArray[$npm1]}-${xArray[$npm2]}/1" | bc`
prighty=`echo "scale=6; 2*${yArray[$npm1]}-${yArray[$npm2]}/1" | bc`
newplist="$pleftx,$plefty $plist $prightx,$prighty"
pairArray=($newplist)
np=${#pairArray[*]}
npm1=`expr $np - 1`


# function to apply catmull-rom basis matrix to knots
catrom()
	{
	a3=`convert xc: -format "%[fx:0.5*(-$x0+3*$x1-3*$x2+$x3)]" info:`
	a2=`convert xc: -format "%[fx:0.5*(2*$x0-5*$x1+4*$x2-$x3)]" info:`
	a1=`convert xc: -format "%[fx:0.5*(-$x0+$x2)]" info:`
	a0=$x1
	b3=`convert xc: -format "%[fx:0.5*(-$y0+3*$y1-3*$y2+$y3)]" info:`
	b2=`convert xc: -format "%[fx:0.5*(2*$y0-5*$y1+4*$y2-$y3)]" info:`
	b1=`convert xc: -format "%[fx:0.5*(-$y0+$y2)]" info:`
	b0=$y1
	}

# function to spline interpolate between two x,y pairs
spline_interp()
	{
	pair0="$1"
	pair1="$2"
	pair2="$3"
	pair3="$4"
	x0=`echo "$pair0" | cut -d, -f1`
	y0=`echo "$pair0" | cut -d, -f2`
	x1=`echo "$pair1" | cut -d, -f1`
	y1=`echo "$pair1" | cut -d, -f2`
	x2=`echo "$pair2" | cut -d, -f1`
	y2=`echo "$pair2" | cut -d, -f2`
	x3=`echo "$pair3" | cut -d, -f1`
	y3=`echo "$pair3" | cut -d, -f2`
	catrom
	# range is horizontal distance in pixels between knots
	range=`convert xc: -format "%[fx:($x2-$x1)]" info:`
	numinc=`convert xc: -format "%[fx:floor($range/$pinc)]" info:`
	pinc=`convert xc: -format "%[fx:$range/$numinc]" info:`
	s=0
	# do not include last value if this is not the last sequence as overlaps with first value in next sequence
	if [ "$x3" != "$prightx" ]; then 
		op="<"
	else
		op="<="
	fi
	points1=`awk -v range=$range -v numinc="$numinc" -v pinc="$pinc" -v op="$op" -v a0="$a0" \
	-v a1="$a1" -v a2="$a2" -v a3="$a3" -v b0="$b0" -v b1="$b1" -v b2="$b2" -v b3="$b3" \
	'BEGIN { if ( op=="<=" ) { for (s=0; s <= numinc; s++) \
	{ s1=s*pinc/range; s2=s1*s1; s3=s2*s1; print a3*s3+a2*s2+a1*s1+a0, b3*s3+b2*s2+b1*s1+b0; }; } \
	else { for (s=0; s < numinc; s++) \
	{ s1=s*pinc/range; s2=s1*s1; s3=s2*s1; print a3*s3+a2*s2+a1*s1+a0, b3*s3+b2*s2+b1*s1+b0; }; } }' |\
	while read x y; do
	echo "$x,$y"
	done`
	# insert line break before each new section
	points="$points
$points1"
	}

# process pairArray to create spline
points=""
j=0
k=1
l=2
m=3
while [ $m -le $npm1 ]; do
echo "Processing Segment $k"
	spline_interp ${pairArray[$j]} ${pairArray[$k]} ${pairArray[$l]} ${pairArray[$m]}
	j=$k
	k=$l
	l=$m
	m=`expr $m + 1`
done

#edit control points to add leading 0, due to bug prior to IM 6.4.2-10
points=`echo "$points" | sed 's/[ ][ ]*\./ 0./g'`
points=`echo "$points" | sed 's/,\./,0./g'`
points=`echo "$points" | sed 's/-\./-0./g'`

#remove leading spaces
points=`echo "$points" | sed -n 's/^[ ]*\(.*\)$/\1/p'`

# augment points so that form closed polygon for creating lut
# note duplicate points do not matter
plistnew="0,0 $points $xscale,$yscale $xscale,0"

# *************
#echo "plistnew=$plistnew"
# *************


# create and apply lut

# trap for IM version
config=`convert -list configure`
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`
if [ "$im_version" -ge "06030507" ]
	then 
	proc="-clut"
else
	proc="-fx 'v.p{u*v.w,0}'"
fi

if [ "$im_version" -lt "06070607" ]; then
	cspace="RGB"
else
	cspace="sRGB"
fi
# note results for below will be a bit too bright; alternative if leave off is way too dark
if [ "$im_version" -ge "06070708" -a "$im_version" -le "06080504" ]; then
	setcspace="-set colorspace RGB"
else
	setcspace=""
fi


if [ "$channel" = "red" ]; then
	cproc1="-channel red"
	cproc2="+channel"
elif [ "$channel" = "green" ]; then
	cproc1="-channel green"
	cproc2="+channel"
elif [ "$channel" = "blue" ]; then
	cproc1="-channel blue"
	cproc2="+channel"
elif [ "$channel" = "lightness" ]; then
	cproc1="$setcspace -colorspace HSL -channel blue"
	cproc2="+channel -colorspace $cspace"
elif [ "$channel" = "brightness" ]; then
	cproc1="$setcspace -colorspace HSB -channel blue"
	cproc2="+channel -colorspace $cspace"
elif [ "$channel" = "luma" ]; then
	cproc1="$setcspace -colorspace YCbCr -channel red"
	cproc2="+channel -colorspace $cspace"
elif [ "$channel" = "luminosity" ]; then
	cproc1="$setcspace -colorspace Lab -channel red"
	cproc2="+channel -colorspace $cspace"
else
	cproc1=""
	cproc2=""
fi

# draw white polygon on black background
# remove first row corresponding to zero as cannot have any white on this row.
# then box average the columns to get the lut and apply it to the image.
ww=`convert xc: -format "%[fx:$xscale+1]" info:`
hh=`convert xc: -format "%[fx:$yscale+1]" info:`
convert $tmpA \
	\( -size ${ww}x${hh} xc:black -fill white \
	-draw "polygon $plistnew" -alpha off \
	-crop ${ww}x${yscale}+0+1 +repage \
	-scale ${ww}x1! \) \
	$cproc1 $proc $cproc2 "$outfile"


# display graph if option -g
if [ "$graphmode" != "" ]
	then
echo ""
echo "Break Points = $plist"
echo ""
echo "Curve Points = $points"
echo ""
	convert -size 150x150 xc:white -stroke black -draw "rectangle 40,10 141,112" $tmp1
	if [ "$graphmode" = "curve" ]; then
		convert $tmp1 \( -size 100x101 xc:white \
			-draw "fill none stroke red polyline $points" -flip \) \
			-compose over -geometry 100x101+41+11 -composite $tmp1
	elif [ "$graphmode" = "points" ]; then
		pArray=($plist)
		num=${#pArray[*]}
		circles=$(for point in $plist; do
			x=`echo "$point" | cut -d, -f1`
			y=`echo "$point" | cut -d, -f2`
			xx=`convert xc: -format '%[fx:'"$x"'+'"$crad"']' info:`
			echo "circle $x,$y $xx,$y"
		done)
		convert $tmp1 \( -size 100x101 xc:white \
			-draw "fill none stroke red polyline $points" \
			-draw "fill blue stroke blue $circles" -flip \) \
			-compose over -geometry 100x101+41+11 -composite $tmp1
	fi
	convert $tmp1 -font Arial -pointsize 10 -draw "text 30,122 '0' \
		text 20,17 '100' text 20,17 '100' text 40,60 '_' text 27,65 '50' \
		text 90,112 '|' text 85,125 '50' text 70,140 'i n p u t'" $tmp1
	convert -respect-parenthesis $tmp1 \( -background white -fill black -font Arial \
	-pointsize 10 -gravity center label:'o \nu \nt \np \nu \nt ' -trim \) \
	-geometry +10+20 -compose over -composite $tmp1
	display $tmp1
fi
exit 0
