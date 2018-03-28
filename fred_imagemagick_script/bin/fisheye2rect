#!/bin/bash
# 
# Developed by Fred Weinhaus 8/5/2008 .......... revised 4/25/2015
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
# USAGE: fisheye2rect [widthxheight] [-i ifov] [-o ofov] [-t type] [-f format] [-m mode] [-c xc,yc] [-l xl,yl] [-r radius] [-v vpmethod] [-b bgcolor] infile outfile
# USAGE: fisheye2rect -d [-c xc,yc] [-r radius] [-s strokecolor] infile outfile
# USAGE: fisheye2rect [-h or -help]
# 
# OPTIONS:
# 
# widthxheight           size (width and height) of desired output image;
#                        default is size of input image
# -i      ifov           input (fisheye) image field of view in degrees;
#                        float; 0<ifov<360; default=180
# -o      ofov           output (perspective) image diagonal field of view in degrees;
#                        float; 0<ofov<180; default=90
# -t      type           type of fisheye lens; linear, equalarea, 
#                        orthographic, stereographic; default=linear
# -f      format         format of fisheye lens image; circular or fullframe;
#                        default=circular
# -m      mode           nadir (n) or zenith (z) in center of fisheye image;
#                        default=nadir (n)
# -c      xc,yc          coordinates of the center of the fisheye image area; 
#                        float; default=center of fisheye image
# -l      xl,yl          coordinates of the look point in the fisheye image area, 
#                        which will also become the center in the perspective 
#                        view; float; default is 3/4 towards the right side 
#                        and 1/2 down from the top.
# -r      radius         radius of fisheye area in the input image; float;
#                        radius>0; default=min(width,height)/2
# -d                     draw circle on the input fisheye image of specified radius, 
#                        center and color=scolor, which will then become the output
# -s      scolor         scolor is the stroke color for a circle that will be 
#                        drawn on the input fisheye image; default=white
# -v      vpmethod       virtual-pixel method to use to fill area of output image 
#                        that are outside the input image; default=black
# -b      bgcolor        background color for virtual-pixel=background
#                        any valid IM color is allowed. The default=black
# 
###
# 
# NAME: FISHEYE2RECT 
# 
# PURPOSE: To generate a perspective (rectilinear) image from a region of a vertically 
# viewed fisheye image.
# 
# DESCRIPTION: FISHEYE2RECT generates a perspective (rectilinear) image from a 
# region of a vertically viewed fisheye image. The region is identified by a point 
# on the fisheye image and the specified output field of view. The perspective  
# image will in general be tilted (not a view along the horizontal), except for 
# example, when viewing from the center to the radius of a 180 degree fisheye 
# image or from the center to the horizon radius in hyperwide fisheye images.
# 
# 
# ARGUMENTS: 
# 
# widthxheight ... WIDTHxHEIGHT are the desired dimensions for the output
# perspective image. If not specified, then the size of the perspective 
# image will be identical to the size of the fisheye image.
# 
# -i ifov ... IFOV is the input fisheye image field of view in degrees. A 
# value of 180 will correspond to a hemispherical fisheye image within the 
# circular area. Values are floats in the range 0<ifov<360. The default is 
# 180 degrees for a full hemisphere. Note that ifov is not limited to 180 
# degrees to permit the use of hyperwide field of view fisheye systems that 
# may view from below the horizon to above the horizon. These are typical 
# of the one-shot mirrored 360 degree panoramic systems.
# 
# -o ofov ... OFOV is the diagonal output perspective image field of view 
# in degrees. Values are floats in the range 0<ofov<180. The default is 90. 
# The amount of input image in the output perspective image will depend upon  
# the ofov parameter. Note that in comparison, a value of 48.8 degrees 
# corresponds to a diagonal field of view from a 35 mm camera 
# (film size 36mm x 24mm) with a 50mm focal length lens, i.e. a "normal" 
#  view. Similarly, when the image diagonal is equal to the focal length  
# of the camera, the field of view is about 53.1 degrees. 
# 
# -t type ... TYPE is the type of fisheye lens. The choices are: linear 
# (equidistant), equalarea (equisolid), orthographic and stereographic. 
# The default is linear.
# 
# -f format ... FORMAT is the format of the fisheye lens image. The choices are: 
# circular (image fills a circle that spans the minimum of the width or height) 
# or fullframe (image spans a circle that spans the diagonal dimension). The 
# default is circular.
# 
# -m mode ... MODE is either nadir (n) or zenith (z) at the center of the 
# fisheye image. The default is nadir (n). If zenith is selected, the 
# perspective image will undergo a 180 degree rotation.
# 
# -c xc,yc ... XC,YC are the pixel coordinates in the input fisheye  
# image that correspond to the (circular fisheye area) center. The pixel at 
# this coordinate will then become the center of the perspective image. The
# default values are the center of the input fisheye image. Values  
# are non-negative floats. You can use the -d option to validate your 
# choice of center and radius for the fisheye image. See more below.
# 
# -l xl,yl ... XL,YL are the pixel coordinates in the input fisheye  
# image that determine the direction towards which the output perspective 
# view is looking. That is the pixel at this coordinate will then be 
# transormed to the center of the perspective image. The view will  
# always be from the center of the input fisheye image. The default  
# is at xl=0.75*width and yl=0.5*height, i.e. half way down and 3/4  
# towards the right edge. Note that as xl,yl get closer to the  
# center of the fisheye, a horizontal stripe artifact will show at the   
# nadir when it is within the field of view. Recommended locations are 
# at least half way towards the edges (i.e. greater than about 1/2 the  
# radius of the image from center).
# 
# -r radius ... RADIUS is the radius of the fisheye circular area in the 
# input image. Values are floats greater than zero. The default is half  
# the minimum value between the input image width and height.
# 
# -d ... Use of this argument produces an ouput image that is simply the input 
# image with a circle drawn on it to show where the expected fisheye image 
# area is located. You can specify a radius and center point if you want to 
# adjust the transformation to use the precise center and radius that matches 
# the area delimited by the circle. Radius and center default as described 
# above.
# 
# -s scolor ... SCOLOR is the stroke color to use to draw the circle when 
# the -d argument is used. The default is white.
# 
# -v vpmethod ... VPMETHOD is the virtual-pixel method. Any valid IM virtual-pixel 
# setting is allowed. The default is black.
# 
# -b bgcolor ... BGCOLOR is the background color to use with vpmethod=background. 
# Any valid IM color may be used. The default is black.
# 
# See the following references for definitions and mathematical details of each 
# type of fisheye lens: 
# http://en.wikipedia.org/wiki/Fisheye_lens
# http://www.bobatkins.com/photography/technical/field_of_view.html
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
ifov=180					# fisheye field of view (aperture) in degrees
ofov=90						# perspective field of view (aperture) in degrees
xc=""						# center of fisheye area
yc=""						# center of fisheye area
xl=""						# look point in the fisheye image
yl=""						# look point in the fisheye image
rad=""						# radius of fisheye area
type="linear"				# linear, equalarea, orthographic, stereographic
format="circular"			# circular, fullframe
draw="no"					# flag to draw circle on input
sc="red"					# stroke color for drawing circle
dwidth=""					# desired width of output perspective image
dheight=""					# desired height of output perspective image
vpmethod="black"			# virtual-pixel method
bgcolor="black"				# virtual-pixel background color
mode="n"					# nadir (n) or zenith (z)

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
elif [ $# -gt 23 ]
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
				-i)    # get ifov
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID IFOV SPECIFICATION ---"
					   checkMinus "$1"
					   ifov=`expr "$1" : '\([.0-9]*\)'`
					   [ "$ifov" = "" ] && errMsg "--- IFOV=$ifov MUST BE A NON-NEGATIVE FLOAT ---"
					   ifovtestA=`echo "$ifov <= 0" | bc`
					   ifovtestB=`echo "$ifov >= 360" | bc`
					   [ $ifovtestA -eq 1 -o $ifovtestB -eq 1 ] && errMsg "--- IFOV=$ifov MUST BE A FLOAT GREATER THAN 0 AND LESS THAN 360 ---"
					   ;;
				-o)    # get ofov
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID OFOV SPECIFICATION ---"
					   checkMinus "$1"
					   ofov=`expr "$1" : '\([.0-9]*\)'`
					   [ "$ofov" = "" ] && errMsg "--- IFOV=$ofov MUST BE A NON-NEGATIVE FLOAT ---"
					   ofovtestA=`echo "$ofov <= 0" | bc`
					   ofovtestB=`echo "$ofov >= 180" | bc`
					   [ $ofovtestA -eq 1 -o $ofovtestB -eq 1 ] && errMsg "--- OFOV=$ofov MUST BE A FLOAT GREATER THAN 0 AND LESS THAN 180 ---"
					   ;;
				-t)    # get  type
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID TYPE SPECIFICATION ---"
					   checkMinus "$1"
					   type="$1"
					   [ "$type" != "linear" -a "$type" != "equalarea" -a "$type" != "orthographic" -a "$type" != "stereographic" ] && errMsg "--- INVALID TYPE VALUE ---"
					   ;;
				-f)    # get  format
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FORMAT SPECIFICATION ---"
					   checkMinus "$1"
					   format="$1"
					   [ "$format" != "circular" -a "$format" != "fullframe" ] && errMsg "--- INVALID TYPE VALUE ---"
					   ;;
				-m)    # get  mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode="$1"
					   [ "$mode" != "nadir" -a "$mode" != "n" -a "$mode" != "zenith" -a "$mode" != "z" ] && errMsg "--- INVALID MODE VALUE ---"
					   ;;
				-c)    # get center of fisheye coords
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FISHEYE CENTER COORDS SPECIFICATION ---"
					   checkMinus "$1"
					   test=`echo "$1" | tr "," " " | wc -w`
					   [ $test -eq 1 -o $test -gt 2 ] && errMsg "--- INCORRECT NUMBER OF COORDINATES SUPPLIED ---"
					   coords=`expr "$1" : '\([.0-9]*,[.0-9]*\)'`
					   [ "$coords" = "" ] && errMsg "--- CENTE COORDS=$coords MUST BE A PAIR OF NON-NEGATIVE FLOATS SEPARATED BY A COMMA ---"
					   coords="$1,"
		   			   xc=`echo "$coords" | cut -d, -f1`
		   			   yc=`echo "$coords" | cut -d, -f2`
					   ;;
				-l)    # get look point coords
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID LOOK POINT COORDS SPECIFICATION ---"
					   checkMinus "$1"
					   test=`echo "$1" | tr "," " " | wc -w`
					   [ $test -eq 1 -o $test -gt 2 ] && errMsg "--- INCORRECT NUMBER OF COORDINATES SUPPLIED ---"
					   coords=`expr "$1" : '\([.0-9]*,[.0-9]*\)'`
					   [ "$coords" = "" ] && errMsg "--- LOOK COORDS=$coords MUST BE A PAIR OF NON-NEGATIVE FLOATS SEPARATED BY A COMMA ---"
					   coords="$1,"
		   			   xl=`echo "$coords" | cut -d, -f1`
		   			   yl=`echo "$coords" | cut -d, -f2`
					   ;;
				-r)    # get rad
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RADIUS SPECIFICATION ---"
					   checkMinus "$1"
					   rad=`expr "$1" : '\([.0-9]*\)'`
					   [ "$rad" = "" ] && errMsg "--- RADIUS=$rad MUST BE A NON-NEGATIVE FLOAT ---"
					   radtestA=`echo "$rad <= 0" | bc`
					   [ $radtestA -eq 1 ] && errMsg "--- RADIUS=$rad MUST BE A FLOAT GREATER THAN 0 ---"
					   ;;
				-s)    # get  scolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID STROKE COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   sc="$1"
					   ;;
				-d)    # enable draw circle
					   draw="yes"
					   ;;
				-v)    # get  vpmethod
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID VIRTUAL-PIXEL SPECIFICATION ---"
					   checkMinus "$1"
					   vpmethod="$1"
					   ;;
				-b)    # get  bgcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BACKGROUND COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
					   ;;
	 [0-9]*x[0-9]*)    # get  size
					   size="$1"
		   			   dwidth=`echo "$size" | cut -dx -f1`
		   			   dheight=`echo "$size" | cut -dx -f2`
		   			   # dheight = dwidth automatically if second set does not exist
		 			   dwidth=`expr "$dwidth" : '\([0-9]*\)'`
		 			   dheight=`expr "$dheight" : '\([0-9]*\)'`
					   [ "$dwidth" = "" -o "$dheight" = "" ] && errMsg "--- WIDTHxHEIGHT=${dwidth}x${dheight} MUST BE TWO NON-NEGATIVE INTEGERS SEPARATED BY AN 'x' ---"
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

tmpA="$dir/fisheye2rect_$$.mpc"
tmpB="$dir/fisheye2rect_$$.cache"
trap "rm -f $tmpA $tmpB;" 0
trap "rm -f $tmpA $tmpB; exit 1" 1 2 3 15
trap "rm -f $tmpA $tmpB; exit 1" ERR

if convert -quiet "$infile" +repage "$tmpA"
	then
	swidth=`identify -format %w $tmpA`
	sheight=`identify -format %h $tmpA`

	#compute min dimension and diagonal dimension
	smin=`convert xc: -format "%[fx:min($swidth,$sheight)]" info:`
	sdiag=`convert xc: -format "%[fx:sqrt($swidth^2+$sheight^2)]" info:`
	
	# compute input (source) fisheye image dimension depending upon format
	if [ "$format" = "circular" ]
		then
		sdim=$smin
	elif [ "$format" = "fullframe" ]
		then
		sdim=$sdiag
	fi
	[ "$rad" != "" ] && sdim=`convert xc: -format "%[fx:2*$rad]" info:`

	# compute half-widths of input (source) fisheye image
	sw2=`convert xc: -format "%[fx:($swidth-1)/2]" info:`
	sh2=`convert xc: -format "%[fx:($sheight-1)/2]" info:`

	# compute center of input (source) fisheye image
	if [ "$xc" = "" ]; then
		xcs=$sw2
	else
		xcs=$xc
	fi
	if [ "$yc" = "" ]; then
		ycs=$sh2
	else
		ycs=$yc
	fi	

	# compute width, height and diagonal dimension of output (destination) perspective image
	if [ "$dwidth" = "" ]; then
		dwidth=$swidth
	fi
	
	if [ "$dheight" = "" ]; then
		dheight=$sheight
	fi
	ddim=`convert xc: -format "%[fx:sqrt($dwidth^2+$dheight^2)]" info:`

	# compute half-widths of output (destination) perspective image
	dw2=`convert xc: -format "%[fx:($dwidth-1)/2]" info:`
	dh2=`convert xc: -format "%[fx:($dheight-1)/2]" info:`

	# compute center of output (destination) perspective image
	xcd=$dw2
	ycd=$dh2
	
	# compute look point
	[ "$xl" = "" ] && xl=`convert xc: -format "%[fx:0.75*$swidth]" info:`
	[ "$yl" = "" ] && yl=$sh2

	# compute output (perspective) focal length and its inverse from ofov
	# phi=fov/2; r=N/2
	# r/f=tan(phi); f=r/tan(phi); f= (N/2)/tan((fov/2)*(pi/180)) = N/(2*tan(fov*pi/360))
	ofoc=`convert xc: -format "%[fx:$ddim/(2*tan($ofov*pi/360))]" info:`
	ofocinv=`convert xc: -format "%[fx:1/$ofoc]" info:`
	
	# compute perspective look point: thetal and radl -> phil from user location in input fisheye view
	thetal=`convert xc: -format "%[fx:atan2($yl-$ycs,$xl-$xcs)]" info:`
	radl=`convert xc: -format "%[fx:sqrt(($yl-$ycs)^2+($xl-$xcs)^2)]" info:`
		
	else
		errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi

# Pertinent equations:
# note phi=fov/2; fov=field of view (aperture)
# note r=N/2; N=min(width,height)
# perspective: r=f*tan(phi); f=r/tan(phi); f=(N/2)/tan((fov/2)*(pi/180))=N/(2*tan(fov*pi/360))
# linear: r=f*phi; f=r/phi; f=(N/2)/((fov/2)*(pi/180))=N*180/(fov*pi)
# equalarea: r=2*f*sin(phi/2); f=(r/2)/sin(phi/2); f=(N/4)/(sin((fov/4)*(pi/180)))=N/(4*sin(fov*pi/720))
# orthographic: r=f*sin(phi); f=r/sin(phi); f=(N/2)/sin((fov/2)*(pi/180))=N/(2*sin(fov*pi/360))
# stereographic: r=2*f*tan(phi/2); f=(r/2)/tan(phi/2); f=(N/4)/(tan((fov/4)*(pi/180)))=N/(4*tan(fov*pi/720))

im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d;  s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# see Bourke diagram at http://local.wasp.uwa.edu.au/~pbourke/projection/fish2/

# NOTE: phi added as a constant to -fx as of IM 6.7.3.4. So need to change its name if used in -fx

# define transform terms"

# get xd,yd in output perspective image
if [ "$mode" = "nadir" -o "$mode" = "n" ]; then	
	# invert y coordinate as image y increased downward and we need true cartesion with y upward
	xd="xd=(i-$xcd);"
	yd="yd=-(j-$ycd);"
else
	# invert y and 180 rotation; combination is equivalent to invert x
	xd="xd=-(i-$xcd);"
	yd="yd=(j-$ycd);"
fi

# get theta phi relative to center of output perspective
#xd=f*tan(theta)
#yd=f*tan(phi)
# use inverse equations and 
# add thetal to get absolute theta
theta="theta=atan($ofocinv*(xd))+$thetal;"
# phi is now a reserved word in fx since Oct, 2011
# so change phi to phiang
phiang="phiang=atan($ofocinv*(yd));"

# correct for lens type converting (phi+phil) to rr to get absolute radius
if [ "$type" = "linear" ]
	then
	ifoc=`convert xc: -format "%[fx:($sdim*180)/($ifov*pi)]" info:`
	phil=`convert xc: -format "%[fx:$radl/$ifoc]" info:`
	rr="rr=$ifoc*(phiang+$phil);"
elif [ "$type" = "equalarea" ]
	then
	#note ifoc2 rather than ifoc to save computations
	ifoc2=`convert xc: -format "%[fx:$sdim/(2*sin($ifov*pi/720))]" info:`
	rr="rr=$ifoc2*sin((phiang+$phil)/2);"
elif [ "$type" = "orthographic" ]
	then
	ifoc=`convert xc: -format "%[fx:$sdim/(2*sin($ifov*pi/360))]" info:`
	rr="rr=$ifoc*sin(phiang+$phil);"
elif [ "$type" = "stereographic" ]
	then
	#note ifoc2 rather than ifoc to save computations
	ifoc2=`convert xc: -format "%[fx:$sdim/(2*tan($ifov*pi/720))]" info:`
	rr="rr=$ifoc2*tan((phiang+$phil)/2);"
fi

#find xs,ys in input from rr, theta
xs="xs=rr*cos(theta)+$xcs;"
ys="ys=rr*sin(theta)+$ycs;"

if [ "$vpmethod" = "transparent" -o "$bgcolor" = "none" ]
	then
	# create alpha channel
	virtual="-matte -channel RGBA -virtual-pixel $vpmethod -background $bgcolor"
else
	virtual="-virtual-pixel $vpmethod -background $bgcolor"
fi

if false; then
# debugging - show echo results if set to true
echo "swidth=$swidth; sheight=$sheight; sw2=$sw2; sh2=$sh2"
echo "dwidth=$dwidth; dheight=$dheight; dw2=$dw2; dh2=$dh2"
echo "xl=$xl; yl=$yl; xcs=$xcs; ycs=$ycs; thetal=$thetal; radl=$radl phil=$phil"
echo "ofoc=$ofoc; ofocinv=$ofocinv; ifoc=$ifoc; ifoc2=$ifoc2;"
echo "dwidth=$dwidth; dheight=$dheight; vpmethod=$vpmethod bgcolor=$bgcolor"
echo "xd=$xd; yd=$yd;"
echo "theta=$theta; phiang=$phiang; rr=$rr;"
echo "xs=$xs; ys=$ys;"
fi

# process image
if [ "$draw" = "yes" ]
	then
	[ "$rad" = "" ] && rad=`convert xc: -format "%[fx:min($sw2,$sh2)]" info:`
	xr=`convert xc: -format "%[fx:$xcs+$rad]" info:`
	convert $tmpA -fill none -stroke "$sc" -draw "circle $xcs,$ycs $xr,$ycs" "$outfile"
else
	convert \( -size ${dwidth}x${dheight} xc: \) $tmpA \
		-virtual-pixel $vpmethod -background $bgcolor -monitor \
		-fx "$xd $yd $theta $phiang $rr $xs $ys v.p{xs,ys}" \
		"$outfile"
fi
exit 0
