#!/bin/bash
#
# Developed by Fred Weinhaus 7/30/2008 .......... revised 4/25/2015
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
# USAGE: polar [-m mode] [-c xc,yc] [-r rmn,rmx] [-a amn,amx] [-v vpmode] [-b bgcolor] infile outfile
# USAGE: polar [-h or -help]
# 
# OPTIONS:
# 
# -m      mode           rect2polar or polar2rect; default=rect2polar
# -c      xc,yc          coordinates of the transformation center; 
#                        default is image center
# -r      rmn,rmx        min and max radius; default=0,(half image diagonal);
#                        non-negative floats; rmx can be w, h, d or m for 
#                        width/2, height/2, diagonal/2 or min(width,height)/2 of image
# -a      amn,amx        min and max angle; default=-180,180;
#                        floats between -180 and 180
# -v      vpmode         virtual-pixel mode to use to fill area of output image 
#                        that are outside the input image; default=edge
# -b      bgcolor        background color for virtual-pixel=background
#                        any valid IM color is allowed. The default=black
# 
###
# 
# NAME: POLAR 
# 
# PURPOSE: To apply either a rectangular (Cartesian) to polar transformation 
# or a polar to rectangular (Cartesian) transformation on an image.
# 
# DESCRIPTION: POLAR applies either a rectangular (Cartesian) to polar transformation 
# or a polar to rectangular (Cartesian) transformation on an image. The user 
# has control of the range of radius, angle and center in the transformation.
# 
# 
# ARGUMENTS: 
# 
# -m mode ... MODE is the transformation mode, which can be either rect2polar 
# (rectangular to polar) or polar2rect (polar to rectangular). The former, rect2polar,  
# treats the input as a normal rectangular image with columns representing X 
# and rows representing Y and treats the output as having columns representing angle 
# and rows representing radius. The latter, polar2rect, treats the input as having 
# columns representing angle and rows representing radius and treats the output as 
# having columns representing X and rows representing Y. Note that in a polar image, 
# nominally the left column is angle=amn=-180, the middle column is angle=0 and the 
# right column is angle=amx=180. Similarly, the top row is radius=rmn=0 and the 
# bottom row is radius=rmx=(half image diagonal).
# 
# -c xc,yc ... XC,YC are the pixel coordinates for the transformation center. 
# The default is the image center.
# 
# -r rmn,rmx ... RMN,RMX are the min and max radii. By default, rmn=0 and 
# rmx=(half image diagonal). This provides a transformation that includes 
# the whole image. However, for the rect2polar mode, this will cause the 
# output image to contain areas that are outside the input image. They will 
# be filled as determined by the virtual pixel setting. Values are non-negative 
# floats. Values are generally non-negative floats, but four other choices 
# are allowed: w, h, d and m, where w=width/2, h=height/2, d=diagonal/2 and 
# m=min(width,height)/2.
# 
# -a amn,amx ... AMN,AMX are the min and max angles. By default, amn=-180 and 
# amx=180. Values for amn and amx are floats between -180 and 180.
# 
# -v vpmode ... VPMODE is the virtual-pixel mode. Any valid IM virtual-pixel 
# setting is allowed. The default is edge.
# 
# -b bgcolor ... BGCOLOR is the background color to use with vpmode=background. 
# Any valid IM color may be used. The default is black.
# 
# Note that this script may be slow due to the use of -fx.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
mode="rect2polar"
xc=""
yc=""
rmn=0
rmx="d"
amn=-180
amx=180
vpmode="edge"
bgcolor="black"

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
elif [ $# -gt 14 ]
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
				-m)    # get  mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode="$1"
					   [ "$mode" != "rect2polar" -a "$mode" != "polar2rect" ] && errMsg "--- INVALID MODE VALUE ---"
					   ;;
				-c)    # get coords
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COORDS SPECIFICATION ---"
					   checkMinus "$1"
					   test=`echo "$1" | tr "," " " | wc -w`
					   [ $test -eq 1 -o $test -gt 2 ] && errMsg "--- INCORRECT NUMBER OF COORDINATES SUPPLIED ---"
					   coords=`expr "$1" : '\([.0-9]*,[.0-9]*\)'`
					   [ "$coords" = "" ] && errMsg "--- COORDS=$coords MUST BE A PAIR OF NON-NEGATIVE FLOATS SEPARATED BY A COMMA ---"
					   coords="$1,"
		   			   xc=`echo "$coords" | cut -d, -f1`
		   			   yc=`echo "$coords" | cut -d, -f2`
					   ;;
				-a)    # get amn,amx angles
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID ANGLE SPECIFICATION ---"
					   test=`echo "$1" | tr "," " " | wc -w`
					   [ $test -eq 1 -o $test -gt 2 ] && errMsg "--- INCORRECT NUMBER OF ANGLES SUPPLIED ---"
					   angles=`expr "$1" : '\([-.0-9]*,[-.0-9]*\)'`
					   [ "$angles" = "" ] && errMsg "--- ANGLE=$angles MUST BE A NON-NEGATIVE FLOAT ---"
					   amn=`echo "$angles" | cut -d, -f1`
					   amx=`echo "$angles" | cut -d, -f2`
					   amntestA=`echo "$amn < -180" | bc`
					   amntestB=`echo "$amn > 180" | bc`
					   [ $amntestA -eq 1 -o $amntestB -eq 1 ] && errMsg "--- AMN=$amn MUST BE A FLOAT BETWEEN -180 AND 180 ---"
					   amxtestA=`echo "$amx < -180" | bc`
					   amxtestB=`echo "$amx > 180" | bc`
					   [ $amxtestA -eq 1 -o $amxtestB -eq 1 ] && errMsg "--- AMX=$amx MUST BE A FLOAT BETWEEN -180 AND 180 ---"
					   test=`echo "$amn >= $amx" | bc`
					   [ $test -eq 1 ] && errMsg "--- AMX=$amx MUST BE GREATER THAN AMN=$amn ---"
					  ;;
				-r)    # get rmn,rmx radii
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RADIUS SPECIFICATION ---"
					   checkMinus "$1"
					   test=`echo "$1" | tr "," " " | wc -w`
					   [ $test -eq 1 -o $test -gt 2 ] && errMsg "--- INCORRECT NUMBER OF RADII SUPPLIED ---"
					   radii=`expr "$1" : '\([.0-9]*,[.0-9]*\)'`
					   [ "$radii" = "" ] && radii=`expr "$1" : '\([.0-9]*,[whdm]\)'`
					   [ "$radii" = "" ] && errMsg "--- RADII=$radii MUST BE A PAIR OF NON-NEGATIVE FLOATS SEPARATED BY A COMMA OR USE THE LETTERS W, H, D OR M ---"
					   radii="$1,"
		   			   rmn=`echo "$radii" | cut -d, -f1`
		   			   rmx=`echo "$radii" | cut -d, -f2`
					   ;;
				-v)    # get  vpmode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID VIRTUAL-PIXEL SPECIFICATION ---"
					   checkMinus "$1"
					   vpmode="$1"
					   ;;
				-b)    # get  bgcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
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

tmpA="$dir/polar_$$.mpc"
tmpB="$dir/polar_$$.cache"
trap "rm -f $tmpA $tmpB;" 0
trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB; exit 1" ERR

if convert -quiet "$infile" +repage "$tmpA"
	then
	# set default values
	w2=`identify -format "%[fx:w/2]" $tmpA`
	h2=`identify -format "%[fx:h/2]" $tmpA`
	d2=`identify -format "%[fx:sqrt(w*w+h*h)/2]" $tmpA`
	m2=`identify -format "%[fx:min(w,h)/2]" $tmpA`
	centx=`identify -format "%[fx:(w-1)/2]" $tmpA`
	centy=`identify -format "%[fx:(h-1)/2]" $tmpA`
	[ "$rmx" = "w" ] && rmx=$w2
	[ "$rmx" = "h" ] && rmx=$h2
	[ "$rmx" = "d" ] && rmx=$d2
	[ "$rmx" = "m" ] && rmx=$m2
	[ "$xc" = "" ] && xc=$centx
	[ "$yc" = "" ] && yc=$centy
fi

#echo "mode=$mode: w=$w2; h=$h2; d=$d2; m=$m2; xc=$xc; yc=$yc; rmn=$rmn; rmx=$rmx; amn=$amn; amx=$amx"

# rect to polar general equations (for inverse transformation)
# note reverse of x and y as angle is measured from y axis not x axis
# x=r*sin(angle)
# y=r*cos(angle)

# polar to rect general equations (for inverse transformation)
# note reversal of order of x,y in arctan2 due to angle measured from y axis
# r=sqrt(x^2 + y^2)
# angle=arctan2(x,y)

# get IM Version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`


# set channel if bgcolor=none
if [ "$bgcolor" = "none" ]; then
	channel="-channel rgba -alpha on"
else
	channel=""
fi
	
# set up equations and do processing

# rect2polar
if [ "$mode" = "rect2polar" ]; then
yy="yy=((j-$rmn)*$rmx/(h-1))*cos((($amx-$amn)*pi/180)*(i/(w-1))+($amn*pi/180))+$yc;"
xx="xx=((j-$rmn)*$rmx/(h-1))*sin((($amx-$amn)*pi/180)*(i/(w-1))+($amn*pi/180))+$xc;"

convert $infile -virtual-pixel $vpmode -background $bgcolor -monitor \
	-fx "$xx $yy u.p{xx,yy}" +monitor "$outfile"
fi

# polar2rect
if [ "$mode" = "polar2rect" ]; then
if [ "$im_version" -ge "06030600" ]; then 
	rr="rr=hypot((i-$xc),(j-$yc));"
else
	rr="rr=sqrt((i-$xc)*(i-$xc) + (j-$yc)*(j-$yc));"
fi
aa="aa=atan2((i-$xc),(j-$yc));"
xx="xx=(aa-(pi/180)*$amn)*(w-1)/((pi/180)*($amx-$amn));"
yy="yy=(rr-$rmn)*(h-1)/($rmx-$rmn);"

convert $infile -virtual-pixel $vpmode -background $bgcolor -monitor \
	-fx "$rr $aa $xx $yy u.p{xx,yy}" +monitor "$outfile"
fi

exit 0
