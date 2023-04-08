#!/bin/bash
# 
# Developed by Fred Weinhaus 5/1/2009 .......... revised 4/24/2017
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
# USAGE: cylinderize [-m mode] [-r radius] [-l length] [-w wrap] [-f fcolor] 
# [-a angle] [-p pitch] [-d direction] [-n narrow] [e efactor] [-s scale]  
# [-o offset] [-v vpmethod] [-b bgcolor] [-t] [-R rotate] [-c compose] [-E] 
# [-D dfile] infile [bgfile] outfile
# 
# USAGE: cylinderize [-h or -help]
# 
# OPTIONS:
# 
# -m      mode           mode of orientation for cylinder axis; options are 
#                        horizontal (or h) or vertical (or v); default=vertical
# -r      radius         radius of cylinder; float>0; default is one quarter
#                        of image width or height depending upon mode.
# -l      length         length of cylinder; lenght>0; default=width or height 
#                        depending upon mode and adjusted for the cylinder 
#                        pitch angle.
# -w      wrap           percentage of image to wrap about the cylinder; float; 
#                        10<=wrap<=100; default=50
# -f      fcolor         fill color for portion of cylinder not covered by the 
#                        image; default=none (transparent)
# -a      angle          rotation angle in degrees about cylinder axis; 
#                        best used when wrap=full; float; -360<=angle<=360;
#                        default=0
# -p      pitch          pitch (tilt) angle of cylinder; float; 0<=pitch<90;
#                        default=0
# -d      direction      pitch direction; choices are down (d), up (u), 
#                        right (r), left (l) or both (b). Option both is for 
#                        the case of a straight on view with perspective 
#                        distortion. The default is down or right depending 
#                        upon mode of vertical or horizontal, respectively.                 
# -n      narrow         narrow is the percent ratio of the bottom-to-top or 
#                        right-to-left radii used to simulate perspective 
#                        tapering; floats>=0; default=100 means same size radii
# -e      efactor        exaggeration factor for pitch curvature of the opposite
#                        side of the cylinder adjusted by the pitch argument
#                        to make perspective views more realistic; float>=1; 
#                        default=1 i.e. no exaggeration. Nominal is 2.
# -s      scale          percent scaling of the infile in either the vertical or 
#                        horizontal dimension as appropriate to the mode; 
#                        integer>=100; default=100
# -o      offsets        x and y offset pair in following form: +-Xoff+-Yoff;
#                        where either the + or - sign is required; values are 
#                        integers; default=+0+0
# -v      vpmethod       virtual-pixel method; default=black
# -b      bgcolor        background color for the case when vpmethod=background; 
#                        default=black
# -t                     trim background
# -R      rotation       rotation angle of output before compositing with 
#                        background;float; -360<=rotation<=360; default=0
# -c      compose        compose method when overlaying the input pattern on
#                        the background image; choices are: over (o) and 
#                        multiply (m); the default is over; to apply shading 
#                        from the background image, use multiply
# -E      export         export the displacement map and other needed parameters 
#                        to be able to use a condensed script to repeat the 
#                        processing on the same size input images for faster 
#                        processing.
# -D      dfile          path to and including desired displacement file name; 
#                        used in conjunction with -E (export) option; 
#                        default="displace.png"; do not use a compressed file
#                        format; also use Q16 or higher IM compilation, not Q8
# 
# bgfile is an optional background file onto which the cylinderized infile is 
# to be composited at the offset coordinated relative to the center of the bgfile
# 
# 
###
# 
# NAME: CYLINDERIZE 
# 
# PURPOSE: To apply a cylinder distortion to an image.
# 
# DESCRIPTION: CYLINDERIZE applies a cylinder distortion to an image so 
# that the image is wrapped about the cylinder. The image can be wrapped 
# about any percentage of the cylinder from 10 to 100 percent. If the wrap 
# is less than 100%, then the cylinder will be colored to fill the remaining 
# amount. The cylinder can also be pitched (tilted).
# 
# 
# ARGUMENTS: 
# 
# -m mode ... MODE specifies the orientation for the cylinder axis. The 
# choices are horizontal (or h) or vertical (or v). The default is vertical.
# 
# -r radius ... RADIUS is the radius of the cylinder in pixels. The values are 
# floats>0. The default is one quarter of the image width or height depending 
# upon the mode.
# 
# -l length ... Length is the length of the cylinder along its axis in pixels. 
# The values are floats with length>0. The default is either the width or height 
# depending upon mode and adjusted for the pitch of the cylinder. If a length 
# is provided, then the cylinder length will not be adjusted for pitch, but 
# the ends will still be adjusted for pitch.
# 
# -w wrap ... WRAP is the percentage of the cylinder circumference that is wrapped 
# with the image. Values are floats such that 10<=wrap<=100. Default=50.
# 
# -f fcolor ... FCOLOR is the fill color to put on the remainder of the cylinder 
# that is not covered by the image. Any valid IM color is allowed. The default 
# is none (for transparent).
# 
# -a angle ... ANGLE is the rotation the cylinder about its axis. This is 
# best used when wrap=full. The values are floats with -360<=angle<=360. 
# The default=0.
# 
# -p pitch ... PITCH (tilt) angle of the cylinder. Values are floats with 
# 0<=pitch<90. The default=0.
#
# -d direction ... pitch DIRECTION (sign). The choices are down (d), up (u), 
# right (r), left (l) or both (b). Option both is for the case of a straight 
# on view with perspective distortion pitching either up and down or left and 
# right. The default is down or right depending upon mode of vertical or 
# horizontal, respectively.                   
#
# -n narrow ... NARROW is the percent ratio of the bottom-to-top or 
# right-to-left radii used to simulate perspective tapering. Values are 
# floats>=0. The default=100 means same size radii.
# 
# -e efactor ... EFACTOR is the exaggeration factor for the pitch curvature 
# of the opposite side of the cylinder adjusted by the pitch argument in order 
# to make perspective views more realistic. Values are floats>=1. The default=1 
# i.e. no exaggeration. From my limited tests a value of 2 works well for 
# pitch values up to at least 20 degrees.
# 
# -s scale ... SCALE is the percent scaling of the infile in the vertical or 
# horizontal dimension as appropriate to the mode. Values are integer>=100; 
# The default=100. This can be used to compensate for the distortions 
# introduced by the top and bottom curvatures.
#
# -o offsets ... OFFSETS are the x and y offset pair in the following form: 
# +-Xoff+-Yoff (no spaces) where either the + or - sign is required. Numerical 
# values are integers. The default=+0+0. The offsets are used to position the 
# cylinderized image over the background image relative to its center.
# 
# -v vpmethod ... VPMETHOD is the virtual-pixel method to use. Any valid IM 
# virtual-pixel may be used. The background will be transparent if 
# vpmethod=transparent. The default is black. 
# 
# -b bgcolor ... BGCOLOR is the background color to use when vpmethod=background. 
# Any valid IM color is allowed. The background will be transparent if 
# bgcolor=none and vpmethod=background. The bgcolor must be set to match the 
# virtual pixel color when using the -R rotation option, as well. This is most 
# simply done by using -vpmethod=background and setting the bgcolor. The  
# default vpmethod is black.
# 
# -t ... TRIM the background.
# 
# -R ... ROTATION angle of output before compositing with background. The 
# values are floats with -360<=rotation<=360. The default=0.
#
# -c ... COMPOSE method when overlaying the input pattern on the background 
# image. The choices are: over (o) and multiply (m). The default is over. 
# To apply shading from the background image, use multiply
# 
# -E ... EXPORT the displacement map and other needed parameters for use with 
# a condensed script to repeat the processing on the same size input images 
# for faster processing. The default displacement map will named displace.png.  
# The other arguments will be displayed to the terminal and can be copied and 
# pasted into other scripts. See my script, cylinderwarp.
# 
# -D dfile ... DFILE is the desired location of the displacement file. It is 
# the path to and including the desired name of the displacement file. It is 
# used in conjunction with -E (export) option. The default="displace.png". 
# Do not specify a compressed file format such as jpg. Also you must use a
# Q16 or higher IM compilation and not Q8. A Q8 system will write an 8-bit  
# displacement file, which may cause severe blocky results in the output image.
# 
# NOTE: Thanks to Anthony Thyssen for the concept and basic equation for 
# achieving the tilted cylinder effect. Thanks to Glen Fiebich for the 
# suggestion to allow any amount of wrap and to be able to fill the 
# remainder with color or transparency and still rotate the cylinder. 
# Thanks to Marcos Passos for the suggestion and processing concept 
# to handle face on cylindrical objects with perspective distortion up 
# and down and for his extensive testing.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
# 

# set default values
mode="vertical"			# vertical or horizontal
wrap=50					# percentage of cylinder to wrap with image
fcolor="none"			# fill color for remainder of cylinder
radius=""				# default=1/4 width or height
length=""				# default=width or height
angle=0					# cylinder rotation
pitch=0					# cylinder pitch (tilt)
direction="positive"	# pitch angle direction (sign)
efact=1             	# bottom curvature exaggeration factor
narrow=100          	# tapering due to perspective effect
scale=100				# vertical or horizontal infile scaling
offsets="+0+0"			# offsets for compositing cylinder over bgfile
vpmethod="black"		# virtual pixel method
bgcolor="black"			# background color for vp method=background
trim="no"           	# trim background: yes or no
rotation=0				# rotation of output before compositing
compose="over"         	# compose method: over or multiply
export="no"				# export displacement map and other arguments
coords=""				# initialize coords
dfile="displace.png"	# displacement file

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
elif [ $# -gt 39 ]
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
					   mode=`echo "$mode" | tr "[:upper:]" "[:lower:]"`
					   case "$mode" in
							horizontal) mode="horizontal" ;;
									 h) mode="horizontal" ;;
							  vertical) mode="vertical" ;;
									 v) mode="vertical" ;;
									 *) errMsg "--- MODE=$mode IS NOT A VALID VALUE ---" ;;
					   esac
					   ;;
				-r)    # get radius
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RADIUS SPECIFICATION ---"
					   checkMinus "$1"
					   radius=`expr "$1" : '\([.0-9]*\)'`
					   [ "$radius" = "" ] && errMsg "--- RADIUS=$radius MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$radius <= 0" | bc`
					   [ $testA -eq 1 ] && errMsg "--- RADIUS=$radius MUST BE A FLOAT GREATER THAN 0 ---"
					   ;;
				-l)    # get length
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID LENGTH SPECIFICATION ---"
					   checkMinus "$1"
					   length=`expr "$1" : '\([.0-9]*\)'`
					   [ "$length" = "" ] && errMsg "--- LENGTH=$length MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$length <= 0" | bc`
					   [ $testA -eq 1 ] && errMsg "--- LENGTH=$length MUST BE A FLOAT GREATER THAN 0 ---"
					   ;;
				-w)    # get wrap
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID WRAP SPECIFICATION ---"
					   #checkMinus "$1"
					   wrap=`expr "$1" : '\([.0-9]*\)'`
					   [ "$wrap" = "" ] && errMsg "--- WRAP=$wrap MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$wrap < 10" | bc`
					   testB=`echo "$wrap > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- WRAP=$wrap MUST BE A FLOAT BETWEEN 10 AND 100 ---"
					   ;;
				-f)    # get  fcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FILL COLOR SPECIFICATION ---"
					   checkMinus "$1"
					   fcolor="$1"
					   ;;
				-a)    # get angle
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID ANGLE SPECIFICATION ---"
					   #checkMinus "$1"
					   angle=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$angle" = "" ] && errMsg "--- ANGLE=$angle MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$angle < -360" | bc`
					   testB=`echo "$angle > 360" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- ANGLE=$angle MUST BE A FLOAT BETWEEN -360 AND 360 ---"
					   ;;
				-p)    # get pitch
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID PITCH SPECIFICATION ---"
					   #checkMinus "$1"
					   pitch=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$pitch" = "" ] && errMsg "--- PITCH=$pitch MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$pitch >= 90" | bc`
					   [ $testA -eq 1 ] && errMsg "--- PITCH=$pitch MUST BE A FLOAT BETWEEN 0 AND LESS THAN 90 ---"
					   ;;
				-d)    # get  direction
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DIRECTION SPECIFICATION ---"
					   checkMinus "$1"
					   direction="$1"
					   direction=`echo "$direction" | tr "[:upper:]" "[:lower:]"`
					   case "$direction" in
							down|d) direction="positive" ;;
							right|r) direction="positive" ;;
							up|u) direction="negative" ;;
							left|l) direction="negative" ;;
							both|b) direction="both" ;;
							*) errMsg "--- DIRECTION=$direction IS NOT A VALID VALUE ---" ;;
					   esac
					   ;;
				-e)    # get efact
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID EFACTOR SPECIFICATION ---"
					   checkMinus "$1"
					   efact=`expr "$1" : '\([.0-9]*\)'`
					   [ "$efact" = "" ] && errMsg "--- EFACTOR=$efact MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$efact < 1" | bc`
					   [ $testA -eq 1 ] && errMsg "--- EFACTOR=$efact MUST BE A FLOAT GREATER THAN OR EQUAL TO 1 ---"
					   ;;
				-n)    # get narrow
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NARROW SPECIFICATION ---"
					   checkMinus "$1"
					   narrow=`expr "$1" : '\([.0-9]*\)'`
					   [ "$narrow" = "" ] && errMsg "--- NARROW=$narrow MUST BE A NON-NEGATIVE FLOAT ---"
					   ;;
				-s)    # get scale
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SCALE SPECIFICATION ---"
					   checkMinus "$1"
					   scale=`expr "$1" : '\([0-9]*\)'`
					   [ "$scale" = "" ] && errMsg "--- SCALE=$scale MUST BE A NON-NEGATIVE INTEGER---"
					   testA=`echo "$scale < 100" | bc`
					   [ $testA -eq 1 ] && errMsg "--- SCALE=$scale MUST BE AN INTEGER GREATER THAN OR EQUAL TO 100 ---"
					   ;;
				-o)    # get offsets
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID OFFSETS SPECIFICATION ---"
					   #checkMinus "$1"
					   offsets=`expr "$1" : '\([-+0-9]*\)'`
					   [ "$offsets" = "" ] && errMsg "--- OFFSETS=$offsets MUST BE A PAIR OF INTEGERS WITH + OR - SIGNS ---"
					   ;;
				-v)    # get  vpmethod
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID VIRTUAL-PIXEL METHOD SPECIFICATION ---"
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
				-t)    # get  trim
					   trim="yes"
					   ;;
				-R)    # get rotation
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID ROTATION SPECIFICATION ---"
					   #checkMinus "$1"
					   rotation=`expr "$1" : '\([-.0-9]*\)'`
					   [ "$rotation" = "" ] && errMsg "--- ROTATION=$rotation MUST BE A NON-NEGATIVE FLOAT ---"
					   testA=`echo "$rotation < -360" | bc`
					   testB=`echo "$rotation > 360" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- ROTATION=$rotation MUST BE A FLOAT BETWEEN -360 AND 360 ---"
					   ;;
				-c)    # get compose method
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID COMPOSE SPECIFICATION ---"
					   checkMinus "$1"
					   compose="$1"
					   compose=`echo "$compose" | tr "[:upper:]" "[:lower:]"`
					   case "$compose" in
							over|o) compose="over" ;;
							multiply|m) compose="multiply" ;;
							*) errMsg "--- COMPOSE=$compose IS NOT A VALID VALUE ---" ;;
					   esac
					   ;;
				-E)    # get  export
					   export="yes"
					   ;;
				-D)    # dfile
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DISPLACEMENT FILE SPECIFICATION ---"
					   checkMinus "$1"
					   dfile="$1"
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
	# get infile, bgfile and outfile
	if [ $# -eq 3 ]; then
		infile="$1"
		bgfile="$2"
		outfile="$3"
	elif [ $# -eq 2 ]; then
		infile="$1"
		outfile="$2"
	else
	errMsg "--- NO OUTPUT FILE SPECIFIED ---"
	fi
fi

# test that infile provided
[ "$infile" = "" ] && errMsg "NO INPUT FILE SPECIFIED"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "NO OUTPUT FILE SPECIFIED"

# set up temporary images
tmpA1="$dir/cylinderize_1_$$.mpc"
tmpB1="$dir/cylinderize_1_$$.cache"
tmpA2="$dir/cylinderize_2_$$.mpc"
tmpB2="$dir/cylinderize_2_$$.cache"
tmpA3="$dir/cylinderize_3_$$.mpc"
tmpB3="$dir/cylinderize_3_$$.cache"
tmpA4="$dir/cylinderize_4_$$.mpc"
tmpB4="$dir/cylinderize_4_$$.cache"
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4;" 0
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4; exit 1" 1 2 3 15
trap "rm -f $tmpA1 $tmpB1 $tmpA2 $tmpB2 $tmpA3 $tmpB3 $tmpA4 $tmpB4; exit 1" ERR

# get im_version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# get image dimensions
width=`convert -ping "$infile" -format "%w" info:`
height=`convert -ping "$infile" -format "%h" info:`

# get default radius and length
if [ "$mode" = "vertical" -a "$radius" = "" ]; then
	radius=`convert xc: -format "%[fx:$width/4]" info:`
elif [ "$mode" = "horizontal" -a "$radius" = "" ]; then
	radius=`convert xc: -format "%[fx:$height/4]" info:`
fi
if [ "$mode" = "vertical" -a "$length" = "" ]; then
	length1=`convert xc: -format "%[fx:$height]" info:`
elif [ "$mode" = "horizontal" -a "$length" = "" ]; then
	length1=`convert xc: -format "%[fx:$width]" info:`
else
	length1=$length
fi

if [ "$mode" = "vertical" ]; then
	# need to resize image if width<2*radius
	test=`convert xc: -format "%[fx:($width<2*$radius)?1:0]" info:`
	if [ $test -eq 1 ]; then
		ratio=`convert xc: -format "%[fx:2*$radius/$width]" info:`
		pct=`convert xc: -format "%[fx:100*$ratio]" info:`
		width=`convert xc: -format "%[fx:round(2*$radius)]" info:`
		height=`convert xc: -format "%[fx:$ratio*$height]" info:`
		rproc="-resize $pct%"
	else
		rproc=""
	fi
	pwidth=`convert xc: -format "%[fx:100*$width/$wrap]" info:`
	pheight=$height
elif [ "$mode" = "horizontal" ]; then
	# need to resize image if height<2*radius
	test=`convert xc: -format "%[fx:($height<2*$radius)?1:0]" info:`
	if [ $test -eq 1 ]; then
		ratio=`convert xc: -format "%[fx:2*$radius/$height]" info:`
		pct=`convert xc: -format "%[fx:100*$ratio]" info:`
		height=`convert xc: -format "%[fx:round(2*$radius)]" info:`
		width=`convert xc: -format "%[fx:$ratio*$width]" info:`
		rproc="-resize $pct%"
	else
		rproc=""
	fi
	pwidth=$width
	pheight=`convert xc: -format "%[fx:100*$height/$wrap]" info:`
fi

# set sign of angle for use below in sign of -roll arguments
sign=`convert xc: -format "%[fx:sign($angle)]" info:`
[ $sign -lt 0 ] && sign="-" || sign="+"

if [ "$mode" = "vertical" -a "$angle" != "0" ]; then	
	rollx=`convert xc: -format "%[fx:abs($angle)*$pwidth/360]" info:`
	roll="${sign}${rollx}+0"
	rolling="-roll $roll"
elif [ "$mode" = "horizontal" -a "$angle" != "0" ]; then
	rolly=`convert xc: -format "%[fx:abs($angle)*$pheight/360]" info:`
	roll="+0${sign}${rolly}"
	rolling="-roll $roll"
else
	rolling=""
fi

# set up resize
if [ "$scale" != "100" -a "$mode" = "vertical" ]; then 
	resizing="-resize 100x${scale}%"
elif [ "$scale" != "100" -a "$mode" = "horizontal" ]; then 
	resizing="-resize ${scale}x100%"
else
	resizing=""
fi

# set up for transparency
if [ "$vpmethod" = "transparent" -o "$bgcolor" = "none" ]; then
	channels="-channel rgba -alpha on"
else
	channels="-alpha off"
fi

# set up for image flip for negative pitch
if [ "$direction" = "negative" -a "$mode" = "vertical" ]; then
	flipping="-flip"
elif [ "$direction" = "negative" -a "$mode" = "horizontal" ]; then
	flipping="-flop"
else
	flipping=""
fi

# read the input image into the TMP cached image.
convert -quiet "$infile" $rproc $resizing +repage \
	-gravity center -background ${fcolor} -extent ${pwidth}x${pheight} \
	$rolling $flipping "$tmpA1" || 
	errMsg "--- FILE $infile NOT READABLE OR HAS ZERO SIZE ---"


if [ "$bgfile" != "" ]; then
	convert -quiet -regard-warnings "$bgfile" +repage $tmpA2 || 
		errMsg "--- FILE $bgfile NOT READABLE OR HAS ZERO SIZE ---"
	wbg=`convert $tmpA2 -format "%w" info:`
	hbg=`convert $tmpA2 -format "%h" info:`
fi



# compute center coords
xc=`convert xc: -format "%[fx:($pwidth)/2]" info:`
yc=`convert xc: -format "%[fx:($pheight)/2]" info:`

# get arcsin factor depending upon wrap
factor=`convert xc: -format "%[fx:(1/pi)]" info:`

if [ "$direction" = "both" ]; then
	pitch=-$pitch
fi

# get tilted dimensions
if [ "$length" = "" ]; then
	length1=`convert xc: -format "%[fx:$length1*cos(pi*$pitch/180)]" info:`
fi
radius1=`convert xc: -format "%[fx:$radius*sin(pi*$pitch/180)]" info:`
radius2=`convert xc: -format "%[fx:$efact*$radius1]" info:`
iefact=`convert xc: -format "%[fx:100/$efact]" info:`
if [ "$mode" = "vertical" ]; then
	height1=`convert xc: -format "%[fx:$length1+$radius2]" info:`
	length2=`convert xc: -format "%[fx:100*($length1)/$height]" info:`
	if [ "$direction" = "both" ]; then
		# fix to make full height of image show in result
		height1=$length1
		# fix for when length1 < 2*radius; reduce length2
		fact=1
		test=`convert xc: -format "%[fx:($length1<2*$radius)?1:0]" info:`
		if [ $test -eq 1 ]; then
			fact=`convert xc: -format "%[fx:$length1/(2*$radius)]" info:`
			length2=`convert xc: -format "%[fx:$fact*$length2]" info:`
		fi
		pad=`convert xc: -format "%[fx:-round(0.5*$efact*$height1*sin(pi*$pitch/180)/$fact)]" info:`
		height12=`convert xc: -format "%[fx:$height1+2*$pad]" info:`
		wbg=$((wbg+2*pad))
		hbg=$((hbg+2*pad))
	else
		pad=0
		height12=$height1
	fi
else
	width1=`convert xc: -format "%[fx:$length1+$radius2]" info:`
	if [ "$direction" = "both" ]; then
		# fix to make full width of image show in result
		width1=$length1
		# fix for when length1 < 2*radius; reduce length2
		fact=1
		test=`convert xc: -format "%[fx:($length1<2*$radius)?1:0]" info:`
		if [ $test -eq 1 ]; then
			fact=`convert xc: -format "%[fx:$length1/(2*$radius)]" info:`
			length2=`convert xc: -format "%[fx:$fact*$length2]" info:`
		fi
		pad=`convert xc: -format "%[fx:-round(0.5*$efact*$width1*sin(pi*$pitch/180)/$fact)]" info:`
		width12=`convert xc: -format "%[fx:$width1+2*$pad]" info:`
		wbg=$((wbg+2*pad))
		hbg=$((hbg+2*pad))
	else
		pad=0
		width12=$width1
	fi
fi
if [ "$pad" != "0" -a "$mode" = "vertical" ]; then
	borderize="-bordercolor none -border 0x${pad}"
elif [ "$pad" != "0" -a "$mode" = "horizontal" ]; then
	borderize="-bordercolor none -border ${pad}x0"
else
	borderize=""
fi
#echo "radius=$radius; radius1=$radius1; radius2=$radius2; length1=$length1; pad=$pad; height1=$height1; height12=$height12; borderize=$borderize;"


# set up for background color
if [ "$vpmethod" = "background" ]; then
	backgroundcolor="-background $bgcolor"
elif [ "$vpmethod" = "black" ]; then
	backgroundcolor="-background black"
elif [ "$vpmethod" = "white" ]; then
	backgroundcolor="-background white"
elif [ "$vpmethod" = "gray" ]; then
	backgroundcolor="-background gray"
elif [ "$vpmethod" = "transparent" -o "$vpmethod" = "none" ]; then
	backgroundcolor="-background none"
else
	backgroundcolor=""
fi

# set up for rotation
if [ "$rotation" != "0" ]; then
	rotating="-background $bgcolor -rotate $rotation +repage"
else
	rotating=""
fi

# process image
if [ "$mode" = "vertical" ]; then
	# Slow method using fx
	#	convert $tmpA1 -virtual-pixel $vpmethod -fx \
	#		"xd=(i-$xc)/$radius; $ffx xs=$xc*ffx+$xc; u.p{xs,j}" \
	#		$outfile
	# Equivalent displace map must be relative to i and range from 0 to 1, 
	# but slow fx method above is relative to $xc and 
	# ffx ranges from -1 to 1 (for wrap=half)
	# Thus we must modify the conversion to a displament map
	# from simply 0.5*(ffx-0.5) [which scales ffx from -1,1 to 0,1]
	# to 0.5*(ffx+($xc-i)/$xc)+0.5 [to account for the change from i to $xc]

	# create horizontal cylinder map
	ffx="ffx=$factor*asin(xd);"

	# NOTE: need to clamp at range -1 to 1 for xd, since asin argument must be in that range, 
	# esp for HDRI to avoid NAN, so use xd>1?1:xd<-1?-1:xs
	# NOTE: also need to add clamp after -fx to avoid negative values.
	convert -size ${pwidth}x1 xc: -virtual-pixel black -fx \
		"xd=(i-$xc)/$radius; $ffx xs=0.5*(ffx+($xc-i)/($xc))+0.5; xd>1?1:xd<-1?-1:xs" \
		-clamp -scale ${pwidth}x${height12}! $tmpA3

	# create vertical tilted map
	ffx="ffx=-sqrt(1-(xd)^2);"
	# create equal curvature bottom and top map
	convert -size ${pwidth}x1 xc: -virtual-pixel black -fx \
		"xd=(i-$xc)/$radius; $ffx xs=0.5*(ffx)+0.5; abs(xd)>1?0.5:xs" \
		-scale ${pwidth}x${height12}! $tmpA4
	if [ "$efact" != 1 ]; then
		# exaggerate bottom relative to top (actually reduce top relative to exaggerated radius)
		convert \( -size ${pwidth}x${height12} gradient:black-white +level ${iefact}x100% \) $tmpA4 \
			-compose mathematics -define compose:args="1,0,-0.5,0.5" -composite $tmpA4 
	fi


	if [ "$direction" = "both" ]; then
		wd=`convert -ping $tmpA4 -format "%w" info:`
		ht=`convert -ping $tmpA4 -format "%h" info:`
		#echo "wd=$wd; ht=$ht;"
		convert $tmpA4 \
		\( -clone 0 -negate \) \
		\( -size ${wd}x${ht} gradient: -negate \) \
		-compose over -composite \
		$tmpA4
	fi

	if [ "$export" = "yes" ]; then
	# save distortion map
	# PNG32: causes issues in the dfile such that the result is highly pixelized in large blocks.
	# Seems as though PNG32 writes 8-bit image and PNG writes 16-bit image. I think that is the issue.
	# So add -depth 16 to command to be sure that works and do not use PNG32: since $file may be some other format such as tif.
	#convert $tmpA3 $tmpA4 \( -clone 0 -fill black -colorize 100 \) -colorspace sRGB -combine -alpha set PNG32:$dfile
	convert $tmpA3 $tmpA4 \( -clone 0 -fill black -colorize 100 \) -colorspace sRGB -combine -alpha set -depth 16 $dfile
	fi

	# set up gravity 
	# need to use north if direction positive, or negative (since using -flip for negative case), center if direction both
	if [ "$direction" = "both" ]; then
		gravityval="center"
	else
		gravityval="north"
	fi
#echo "gravityval=$gravityval; borderize=$borderize;"

	# apply displacement
	# convert length1 to percentage of height
	convert $tmpA1 -resize 100x${length2}% \
		$backgroundcolor -gravity $gravityval -extent ${pwidth}x${height1} $borderize $tmpA1

	if [ "$im_version" -lt "06050304" ]; then
		composite $tmpA3 $tmpA1 $tmpA4 $channels -virtual-pixel $vpmethod \
			$backgroundcolor -displace ${xc}x${radius2} $tmpA1
	else
		convert $tmpA1 $tmpA3 $tmpA4 $channels -virtual-pixel $vpmethod $backgroundcolor \
			-define compose:args=${xc}x${radius2} -compose displace -composite $tmpA1
	fi

	if [ "$trim" = "yes" ]; then
		convert $tmpA1 -fill $bgcolor -trim $tmpA1
	else
		convert $tmpA1 -gravity center -crop ${width}x${height12}+0+0 +repage $tmpA1
	fi

# apply narrowing
	if [ "$narrow" != "100" ]; then
		ww=`convert $tmpA1 -ping -format "%[fx:w-1]" info:`
		hh=`convert $tmpA1 -ping -format "%[fx:h-1]" info:`
		offset=`convert xc: -format "%[fx:($ww/2)*(1-$narrow/100)]" info:`
		www=`convert xc: -format "%[fx:$ww-$offset]" info:`
		x1=0
		y1=0
		x2=$ww
		y2=0
		x3=$www
		y3=$hh
		x4=$offset
		y4=$hh
		coords="0,0 $x1,$y1  $ww,0 $x2,$y2  $ww,$hh $x3,$y3  0,$hh $x4,$y4"
		convert $tmpA1 -virtual-pixel $vpmethod $backgroundcolor \
		-distort perspective "$coords" $tmpA1	
	fi


	if [ "$bgfile" != "" ]; then
		convert $tmpA2 \
			\( $tmpA1 $flipping -gravity center -background $bgcolor -extent ${wbg}x${hbg} $rotating \) \
			-gravity center -geometry $offsets -compose $compose -composite $outfile
	else
		convert $tmpA1 $flipping $rotating $outfile
	fi


elif [ "$mode" = "horizontal" ]; then

# create vertical cylinder map
	ffy="ffy=$factor*asin(yd);"

	# NOTE: need to clamp at range -1 to 1 for xd, since asin argument must be in that range, 
	# esp for HDRI to avoid NAN, so use yd>1?1:yd<-1?-1:ys
	# NOTE: also need to add clamp after -fx to avoid negative values.
	convert -size 1x${pheight} xc: -virtual-pixel black -fx \
		"yd=(j-$yc)/$radius; $ffy ys=0.5*(ffy+($yc-j)/($yc))+0.5; yd>1?1:yd<-1?-1:ys" \
		-scale ${width12}x${pheight}! \
		$tmpA3

# create horizontal tilted map
	ffy="ffy=-sqrt(1-(yd)^2);"
	convert -size 1x${pheight} xc: -virtual-pixel black -fx \
		"yd=(j-$yc)/$radius; $ffy ys=0.5*(ffy)+0.5; abs(yd)>1?0.5:ys" \
		-scale ${width12}x${pheight}! \
		$tmpA4
	if [ "$efact" != 1 ]; then
		# exaggerate right relative to left (actually reduce left relative to exaggerated radius)
		convert \( -size ${pheight}x${width12} gradient:black-white -rotate -90 +level ${iefact}x100% \) $tmpA4 \
			-compose mathematics -define compose:args="1,0,-0.5,0.5" -composite $tmpA4 
	fi

	if [ "$direction" = "both" ]; then
		wd=`convert -ping $tmpA4 -format "%w" info:`
		ht=`convert -ping $tmpA4 -format "%h" info:`
		#echo "wd=$wd; ht=$ht;"
		convert $tmpA4 \
		\( -clone 0 -negate \) \
		\( -size ${ht}x${wd} gradient: -rotate -90 -negate \) \
		-compose over -composite \
		$tmpA4
	fi

	if [ "$export" = "yes" ]; then
	# save distortion map
	# PNG32: causes issues in the dfile such that the result is highly pixelized in large blocks.
	# Seems as though PNG32 writes 8-bit image and PNG writes 16-bit image. I think that is the issue.
	# So add -depth 16 to command to be sure that works.
	#convert $tmpA3 $tmpA4 \( -clone 0 -fill black -colorize 100 \) -colorspace sRGB -combine -alpha set PNG32:$dfile
	convert $tmpA4 $tmpA3 \( -clone 0 -fill black -colorize 100 \) -colorspace sRGB -combine -alpha set -depth 16 $dfile
	fi

	# set up gravity 
	# need to use west if direction positive, or negative (since using -flop for negative case), center if direction both
	if [ "$direction" = "both" ]; then
		gravityval="center"
	else
		gravityval="west"
	fi

# apply displacement
	# convert length1 to percentage of height
	length2=`convert xc: -format "%[fx:100*($length1)/$width]" info:`
	convert $tmpA1 -resize ${length2}x100% \
		$backgroundcolor -gravity $gravityval -extent ${width1}x${pheight} $tmpA1

	if [ "$im_version" -lt "06050304" ]; then
		composite $tmpA4 $tmpA1 $tmpA3 $channels -virtual-pixel $vpmethod \
			$backgroundcolor -displace ${radius2}x${yc} $tmpA1
	else
		convert $tmpA1 $tmpA4 $tmpA3 $channels -virtual-pixel $vpmethod $backgroundcolor \
			-define compose:args=${radius2}x${yc} -compose displace -composite $tmpA1
	fi
	
	if [ "$trim" = "yes" ]; then
		convert $tmpA1 -fill $bgcolor -trim $tmpA1
	else
		convert $tmpA1 -gravity center -crop ${width12}x${height}+0+0 +repage $tmpA1
	fi

# apply narrowing
	if [ "$narrow" != "100" ]; then
		ww=`convert $tmpA1 -ping -format "%[fx:w-1]" info:`
		hh=`convert $tmpA1 -ping -format "%[fx:h-1]" info:`
		offset=`convert xc: -format "%[fx:($hh/2)*(1-$narrow/100)]" info:`
		hhh=`convert xc: -format "%[fx:$hh-$offset]" info:`
		x1=0
		y1=0
		x2=$ww
		y2=$offset
		x3=$ww
		y3=$hhh
		x4=0
		y4=$hh
		coords="0,0 $x1,$y1  $ww,0 $x2,$y2  $ww,$hh $x3,$y3  0,$hh $x4,$y4"
		convert $tmpA1 -virtual-pixel $vpmethod $backgroundcolor \
		-distort perspective "$coords" $tmpA1		
	fi

	if [ "$bgfile" != "" ]; then
		convert $tmpA2 \
			\( $tmpA1 $flipping -gravity center -background $bgcolor -extent ${wbg}x${hbg} $rotating \) \
			-gravity center -geometry $offsets -compose $compose -composite "$outfile"
	else
		convert $tmpA1 $flipping $rotating "$outfile"
	fi

fi

if [ "$export" = "yes" ]; then
# show arguments
echo "mode=\"$mode\""
echo "direction=\"$direction\""
echo "trim=\"$trim\""
echo "scale=\"$scale\""
echo "rproc=\"$rproc\""
echo "roll=\"$roll\""
echo "fcolor=\"$fcolor\""
echo "vpmethod=\"$vpmethod\""
echo "bgcolor=\"$bgcolor\""
echo "width=\"$width\""
echo "height=\"$height\""
echo "length2=\"$length2\""
echo "pwidth=\"$pwidth\""
echo "pheight=\"$pheight\""
echo "width1=\"$width1\""
echo "height1=\"$height1\""
echo "width12=\"$width12\""
echo "height12=\"$height12\""
echo "xc=\"$xc\""
echo "yc=\"$yc\""
echo "radius2=\"$radius2\""
echo "coords=\"$coords\""
echo "offsets=\"$offsets\""
echo "rotation=\"$rotation\""
echo "wbg=\"$wbg\""
echo "hbg=\"$hbg\""
echo "pad=\"$pad\""
echo "compose=\"$compose\""
fi

exit 0
