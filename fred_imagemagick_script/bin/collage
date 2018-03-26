#!/bin/bash
# 
# Developed by Fred Weinhaus 6/12/2016 .......... revised 6/12/2016
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
# USAGE: collage [-d dimensions] [-b bgcolor] [-m mode] [-n newseed] 
# [-p pattern] [-s scale] [-g geometry] [-f format] [-r radius] [-t taper] 
# infile [overlayfile] outfile
# USAGE: collage [-h or -help]
# 
# OPTIONS:
# 
# -d     dimensions     WidthxHeight of output; if only one, then output will 
#                       be square; if none, then output will be the size of 
#                       the input; integers>0; default is size of input
# -b     bgcolor        background color; any valid opaque IM color is allowed; 
#                       default is black
# -m     mode           mode is either all (18 variants) or half (9 variants) 
#                       of the input image in the collage; default is all
# -n     newseed        seed value for the randomization of the variants; 
#                       integer>0; default causes every output to be different;
#                       use of any other value will cause the output to be 
#                       repeatable  
# -p     pattern        pattern for the optional overlay image; choices are: 
#                       1, 2, 3 or 4; 1 is one copy in the middle or over the 
#                       whole image based upon the scale value; 2 is a variant 
#                       in the southeast and northwest corners; 3 is a variant 
#                       in the southwest and northeast corners; 4 is a variant 
#                       in all 4 corners of the output; default=1
# -s     scale          scale factor of the overlay image variants as a 
#                       percent of the output image dimensions;  
#                       0<integer<=100; default=50
# -g     geometry		geometry offset in pixels for both x and y for 
#                       overlay image variants; integer; default=0
# -f     format         preprocessing format for the overlay image; choices 
#                       are: none, flip, flop, rot90, rot180, rot270; 
#                       default=none
# -r     radius         radius of the vignette as a percent of half the image 
#                       input image dimensions: 0<integer<=100; default=50
# -t     taper          taper rate of the vignette expressed as a sigma value;
#                       integer>0; default=10
# 
###
# 
# NAME: COLLAGE 
# 
# PURPOSE: Creates a multi-variant collage of the input image.
# 
# DESCRIPTION: Creates a multi-variant collage of the input image. Choices are 
# either 18 variants in the collage or 8 variants in the collage. Variants are 
# randomized in position, orientation and scale. An optional overlay image may 
# be added with 4 choices of number and positions of the variants added atop 
# of the collaged input image.
# 
# OPTIONS: 
#
# -d dimensions ... DIMENSIONS are the WidthxHeight of output. If only one  
# value is specified, then the output will be a square. If neither are 
# provided, then the output will be the size of input. Values are integers>0. 
# The default is the size of input.
# 
# -b bgcolor ... BGCOLOR is the background color for the output image. Any 
# valid opaque IM color is allowed. The default is black.
# 
# -m mode ... MODE is either all (a) or half (h); all uses 18 variants of the 
# input image in the collage. Half uses only 9 variants of the input image 
# in the collage. The default is all.
# 
# -n newseed ... NEWSEED is the seed value for the randomization of the 
# variants. Values are integers>0. The default causes every output to be 
# different. Use of any other value will cause the output to be repeatable. 
#   
# -p pattern ... PATTERN for the optional overlay image. The choices are: 
# 1, 2, 3 or 4. 1 is one copy in the middle or over the whole output based 
# upon the scale value. 2 is a variant in the southeast and northwest corners. 
# 3 is a variant in the southwest and northeast corners. 4 is a variant in 
# all 4 corners of the output. The default=1.
# 
# -s scale ... SCALE is the scale factor for the overlay image variants 
# expressed a as a percent of the output image dimensions. Values are   
# 0<integer<=100. The default=50.
# 
# -g geometry ... GEOMETRY offset in pixels for both x and y for the overlay 
# image variants. Values are integers. The default=0.
# 
# -f format ... FORMAT is the preprocessing format for the overlay image. The 
# choices are: none, flip, flop, rot90, rot180, rot270. The default=none.
# 
# -r radius ... RADIUS of the vignette expressed as a percent of half of the 
# input image dimensions. Values are 0<integer<=100. The default=50.
# 
# -t taper ... TAPER rate of the vignette expressed as a (blur) sigma value. 
# Values are integers>0. The default=10.
# 
# NOTE: The optional overlay image should be a mostly transparent image so 
# that it does not cover too much of the collage.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#

# set default values
dimensions=""		# WxH of output; if only one, then will be square; if none, then will be size of input
bgcolor="black"		# background color
newseed=""			# random seed
mode="all"			# all (18) or half (9) image pattern 
pattern=1			# overlay pattern: 1, 2, 3 or 4
radius=50			# vignette radiusius as percent of half image dimensions: 0<integer<=100
taper=10			# vignette taper sigma value integer>0
scale=50			# overlay scale for copies as percent of overlay dimensions 0<integer<=100
geometry=0			# overlay image offset geometry; integer
format="none"       # overlay image preprocessing: none, flip, flop, rot90, rot180, rot270


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
				-d)    # get dimensions
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID DIMENSIONS SPECIFICATION ---"
					   checkMinus "$1"
					   dimensions=`expr "$1" : '\([0-9]*[x]*[0-9]*\)'`
					   ;;
				-b)    # get bgcolor
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID BGCOLOR SPECIFICATION ---"
					   checkMinus "$1"
					   bgcolor="$1"
					   ;;
				-n)    # get newseed
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID NEWSEED SPECIFICATION ---"
					   checkMinus "$1"
					   newseed=`expr "$1" : '\([0-9]*\)'`
					   [ "$newseed" = "" ] && errMsg "--- NEWSEED=$newseed MUST BE AN INTEGER ---"
		   			   testA=`echo "$newseed == 0" | bc`
					   [ $testA -eq 1 ] && errMsg "--- NEWSEED=$newseed MUST BE AN INTEGER GREATER THAN 0 ---"
					   ;;
			   	-m)    # mode
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID MODE SPECIFICATION ---"
					   checkMinus "$1"
					   mode=`echo "$1" | tr "[:upper:]" "[:lower:]"`
					   case "$mode" in
					   		all|a) mode="all" ;;
					   		half|h) mode="half" ;;
					   		*) errMsg "--- MODE=$mode IS NOT A VALID CHOICE ---" ;;
					   esac
					   ;;
				-p)    # get pattern
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID PATTERN SPECIFICATION ---"
					   checkMinus "$1"
					   pattern=`expr "$1" : '\([0-9]*\)'`
					   [ "$pattern" = "" ] && errMsg "--- PATTERN=$pattern MUST BE AN INTEGER ---"
		   			   testA=`echo "$pattern < 1" | bc`
		   			   testB=`echo "$pattern > 4" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- PATTERN=$pattern MUST BE AN INTEGER GREATER THAN 0 AND LESS THAN 5 ---"
					   ;;
				-r)    # get radius
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID RADIUS SPECIFICATION ---"
					   checkMinus "$1"
					   radius=`expr "$1" : '\([0-9]*\)'`
					   [ "$radius" = "" ] && errMsg "--- RADIUS=$radius MUST BE AN INTEGER ---"
		   			   testA=`echo "$radius == 0" | bc`
		   			   testB=`echo "$radius > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- RADIUS=$radius MUST BE AN INTEGER GREATER THAN 0 AND LESS THAN OR EQUAL 100 ---"
					   ;;
				-t)    # get taper
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID TAPER SPECIFICATION ---"
					   checkMinus "$1"
					   taper=`expr "$1" : '\([0-9]*\)'`
					   [ "$taper" = "" ] && errMsg "--- TAPER=$taper MUST BE AN INTEGER ---"
		   			   testA=`echo "$taper == 0" | bc`
					   [ $testA -eq 1 ] && errMsg "--- TAPER=$taper MUST BE AN INTEGER GREATER THAN 0 ---"
					   ;;
				-s)    # get scale
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID SCALE SPECIFICATION ---"
					   checkMinus "$1"
					   scale=`expr "$1" : '\([0-9]*\)'`
					   [ "$scale" = "" ] && errMsg "--- SCALE=$scale MUST BE AN INTEGER ---"
		   			   testA=`echo "$scale == 0" | bc`
		   			   testb=`echo "$scale > 100" | bc`
					   [ $testA -eq 1 -o $testB -eq 1 ] && errMsg "--- SCALE=$scale MUST BE AN INTEGER GREATER THAN 0 AND LESS THAN OR EQUAL 100 ---"
					   ;;
				-g)    # get geometry
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID GEOMETRY SPECIFICATION ---"
					   #checkMinus "$1"
					   geometry=`expr "$1" : '\([-0-9]*\)'`
					   [ "$geometry" = "" ] && errMsg "--- GEOMETRY=$geometry MUST BE AN INTEGER ---"
					   ;;
			   	-f)    # format
					   shift  # to get the next parameter
					   # test if parameter starts with minus sign 
					   errorMsg="--- INVALID FORMAT SPECIFICATION ---"
					   checkMinus "$1"
					   format=`echo "$1" | tr "[:upper:]" "[:lower:]"`
					   case "$format" in
					   		flip) ;;
					   		flop) ;;
					   		rot90) ;;
					   		rot180) ;;
					   		rot270) ;;
					   		*) errMsg "--- FORMAT=$format IS NOT A VALID CHOICE ---" ;;
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
	# get infile, overlayfile outfile
	numfiles="$#"
	if [ $# -eq 3 ]; then
		infile="$1"
		overlayfile="$2"
		outfile="$3"
	elif [ $numfiles -eq 2 ]; then
		infile="$1"
		outfile="$2"
	else
		errMsg "--- INCOMPATIBLE NUMBER OF FILES SPECIFIED ---"
	fi
fi


# test that infile provided
[ "$infile" = "" ] && errMsg "--- NO INPUT FILE SPECIFIED ---"

# test that outfile provided
[ "$outfile" = "" ] && errMsg "--- NO OUTPUT FILE SPECIFIED ---"

# test infile
[ -f $infile -a -r $infile -a -s $infile ] || errMsg "--- INFILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"

# test overlayfile
if [ "$overlayfile" != "" ]; then
	[ -f $overlayfile -a -r $overlayfile -a -s $overlayfile ] || errMsg "--- OVERLAYFILE $overlayfile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"
fi


function randomVals()
	{
	seed=$1
	if [ "$seed" = "" ]; then
		seed_x=""
		seed_y=""
		seed_scale=""
		seed_quad=""
		seed_ang=""
	else
		seed_x="-seed $((seed+1))"
		seed_y="-seed $((seed+2))"
		seed_scale="-seed $((seed+3))"
		seed_quad="-seed $((seed+4))"
		seed_ang="-seed $((seed+5))"
	fi
	xx=`convert xc: $seed_x -format "%[fx:round($f1*$ww*random())]" info:`
	yy=`convert xc: $seed_y -format "%[fx:round($f1*$hh*random())]" info:`
	scale=`convert xc: $seed_scale -format "%[fx:round(20*random())-10]" info:`
	quad=`convert xc: $seed_quad -format "%[fx:90*round(3*random())]" info:`
	ang=`convert xc: $seed_ang -format "%[fx:round(60*random())-30]" info:`
	angle=$((quad+ang))
	#echo "$i $xx $yy $scale $quad $ang $angle"
	}

# extract provided dimensions
if [ "$dimensions" = "" ]; then
	dim1=`convert -ping "$infile" -format "%wx%h" info:`
	ww=`echo ${dim1} | cut -dx -f1`
	hh=`echo ${dim1} | cut -dx -f2`
else
	count=`echo "$dimensions" | tr "x" " " | wc -w | sed 's/^[ ]*//'`
	if [ $count -eq 1 ]; then
		ww=$dimensions
		hh=$dimensions
	elif [ $count -eq 2 ]; then
		ww=`echo ${dimensions} | cut -dx -f1`
		hh=`echo ${dimensions} | cut -dx -f2`
	else
		errMsg "--- INCOMPATIBLE NUMBER OF DIMENSIONS SPECIFIED ---"
	fi
fi
#odim=`echo "scale=0; ($ww+$hh)/2" | bc`
odim=`convert xc: -format "%[fx:min($ww,$hh)]" info:`
#echo "ww=$ww; hh=$hh; odim=$odim;"

# get the input width and height
wxh=`convert -ping "$infile" -format "%wx%h" info:`
wd=`echo "$wxh" | cut -dx -f1`
ht=`echo "$wxh" | cut -dx -f2`

# get complement of radius and size for overlay
radius=$((100-radius))
osize=$((odim*scale/100))

# compute scaling factors (based upon initial development for 500x500 fixed size output)
f1=`echo "scale=6; 25/500" | bc`
f2=`echo "scale=6; 100/500" | bc`
f3=`echo "scale=6; 125/500" | bc`
f4=`echo "scale=6; 150/500" | bc`
f5=`echo "scale=6; 200/500" | bc`

# compute random x y scale and angle values
[ "$mode" = "all" ] && num=18
[ "$mode" = "half" ] && num=9
for ((i=0; i<num; i++)); do
	if [ "$newseed" = "" ]; then
		randomVals "$newseed"
	else
		newseed=$((newseed+i))
		randomVals "$newseed"
	fi		
	x_Arr[$i]=$xx
	y_Arr[$i]=$yy
	s_Arr[$i]=$scale
	a_Arr[$i]=$angle
done

# set up geometry
offset="+${geometry}+${geometry}"
offsetting="-geometry $offset"

# set up format
if [ "$format" = "none" ]; then
	formatting=""
elif [ "$format" = "flip" ]; then
	formatting="-flip"
elif [ "$format" = "flop" ]; then
	formatting="-flop"
elif [ "$format" = "rot90" ]; then
	formatting="-rotate 90"
elif [ "$format" = "rot180" ]; then
	formatting="-rotate 180"
elif [ "$format" = "rot270" ]; then
	formatting="-rotate 270"
fi
 
# set up for overlay image
if [ "$overlayfile" = "" ]; then
	overlay_setup=""
	overlay1=""
	overlay2=""
	overlay3=""
	overlay4=""
	composite1=""
	composite2=""
	composite3=""
	composite4=""	
elif [ $pattern -eq 1 ]; then
	overlay_setup="$overlayfile -resize ${osize}x${osize} $formatting \
		-alpha on -channel alpha -evaluate multiply 0.8 +channel \
		-write mpr:overimg +delete"
	overlay1="mpr:overimg"
	composite1="-gravity center $offsetting -compose over -composite"
	overlay2=""
	composite2=""
	overlay3=""
	composite3=""
	overlay4=""
	composite4=""	
elif [ $pattern -eq 2 ]; then
	overlay_setup="$overlayfile -resize ${osize}x${osize} $formatting \
		-alpha on -channel alpha -evaluate multiply 0.8 +channel \
		-write mpr:overimg +delete"
	overlay1="mpr:overimg"
	composite1="-gravity southeast $offsetting -compose over -composite"
	overlay2="mpr:overimg -rotate 180"
	composite2="-gravity northwest $offsetting -compose over -composite"
	overlay3=""
	composite3=""
	overlay4=""
	composite4=""	
elif [ $pattern -eq 3 ]; then
	overlay_setup="$overlayfile -resize ${osize}x${osize} $formatting \
		-alpha on -channel alpha -evaluate multiply 0.8 +channel \
		-write mpr:overimg +delete"
	overlay1=""
	composite1=""
	overlay2=""
	composite2=""
	overlay3="mpr:overimg -flip"
	composite3="-gravity northeast $offsetting -compose over -composite"
	overlay4="mpr:overimg -flop"
	composite4="-gravity southwest $offsetting -compose over -composite"
elif [ $pattern -eq 4 ]; then
	overlay_setup="$overlayfile -resize ${osize}x${osize} $formatting \
		-alpha on -channel alpha -evaluate multiply 0.8 +channel \
		-write mpr:overimg +delete"
	overlay1="mpr:overimg"
	composite1="-gravity southeast $offsetting -compose over -composite"
	overlay2="mpr:overimg -rotate 180"
	composite2="-gravity northwest $offsetting -compose over -composite"
	overlay3="mpr:overimg -flip"
	composite3="-gravity northeast $offsetting -compose over -composite"
	overlay4="mpr:overimg -flop"
	composite4="-gravity southwest $offsetting -compose over -composite"
fi

# set up initial resize
if [ "$dimensions" = "" ]; then
	resizing=""
elif [ $ww -eq $wd -a $hh -eq $ht ]; then
	resizing=""
else
	resizing="-resize ${ww}x${hh}"
fi

if [ "$mode" = "all" ]; then
	# set up offsets
	ox1=`echo "scale=0; (${x_Arr[1]}+$f2*$ww)/1" | bc`
	oy1=`echo "scale=0; (${y_Arr[1]}+$f2*$hh)/1" | bc`
	ox2=`echo "scale=0; (${x_Arr[2]}-$f4*$ww)/1" | bc`
	oy2=`echo "scale=0; (${y_Arr[2]}-$f4*$hh)/1" | bc`
	ox3=`echo "scale=0; (${x_Arr[3]}+$f5*$ww)/1" | bc`
	oy3=`echo "scale=0; (${y_Arr[3]}-$f2*$hh)/1" | bc`
	ox4=`echo "scale=0; (${x_Arr[4]}-$f2*$ww)/1" | bc`
	oy4=`echo "scale=0; (${y_Arr[4]}+$f3*$hh)/1" | bc`
	ox5=`echo "scale=0; (${x_Arr[5]}+$f2*$ww)/1" | bc`
	oy5=`echo "scale=0; (${y_Arr[5]}-$f2*$hh)/1" | bc`
	ox6=`echo "scale=0; (${x_Arr[6]}+$f2*$ww)/1" | bc`
	oy6=`echo "scale=0; (${y_Arr[6]}+$f2*$hh)/1" | bc`
	ox7=`echo "scale=0; (${x_Arr[7]}-$f2*$ww)/1" | bc`
	oy7=`echo "scale=0; (${y_Arr[7]}-$f2*$hh)/1" | bc`
	ox8=`echo "scale=0; (${x_Arr[8]}-$f2*$ww)/1" | bc`
	oy8=`echo "scale=0; (${y_Arr[8]}+$f2*$hh)/1" | bc`
	ox9=`echo "scale=0; (${x_Arr[9]})/1" | bc`
	oy9=`echo "scale=0; (${y_Arr[9]}-$f2*$hh)/1" | bc`
	ox10=`echo "scale=0; (${x_Arr[10]}-$f2*$ww)/1" | bc`
	oy10=`echo "scale=0; (${y_Arr[10]}-$f2*$hh)/1" | bc`
	ox11=`echo "scale=0; (${x_Arr[11]}-$f2*$ww)/1" | bc`
	oy11=`echo "scale=0; (${y_Arr[11]})/1" | bc`
	ox12=`echo "scale=0; (${x_Arr[12]}-$f2*$ww)/1" | bc`
	oy12=`echo "scale=0; (${y_Arr[12]}-$f2*$hh)/1" | bc`
	ox13=`echo "scale=0; (${x_Arr[13]})/1" | bc`
	oy13=`echo "scale=0; (${y_Arr[13]}-$f2*$hh)/1" | bc`
	ox14=`echo "scale=0; (${x_Arr[14]}-$f2*$ww)/1" | bc`
	oy14=`echo "scale=0; (${y_Arr[14]}-$f2*$hh)/1" | bc`
	ox15=`echo "scale=0; (${x_Arr[15]}-$f2*$ww)/1" | bc`
	oy15=`echo "scale=0; (${y_Arr[15]})/1" | bc`
	ox16=`echo "scale=0; (${x_Arr[16]}-$f2*$ww)/1" | bc`
	oy16=`echo "scale=0; (${y_Arr[16]}-$f2*$hh)/1" | bc`
	ox17=`echo "scale=0; (${x_Arr[17]})/1" | bc`
	oy17=`echo "scale=0; (${y_Arr[17]})/1" | bc`
	
	# process image
	eval 'convert -respect-parentheses -size ${ww}x${hh} xc:"$bgcolor" \
	\( "$infile" -alpha set -background none -vignette 0x${taper}+${radius}+${radius} $resizing -write mpr:img +delete \) \
	\( mpr:img -channel a -evaluate multiply 0.15 +channel -rotate ${a_Arr[0]} \) \
		-gravity center -geometry +0+0 -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.25 +channel -resize $((75+${s_Arr[1]}))% -rotate ${a_Arr[1]} \) \
		-gravity center -geometry +${ox1}+${oy1} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.25 +channel -resize $((75+${s_Arr[2]}))% -rotate ${a_Arr[2]} \) \
		-gravity center -geometry +${ox2}+${oy2} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.25 +channel -resize $((75+${s_Arr[3]}))% -rotate ${a_Arr[3]} \) \
		-gravity center -geometry +${ox3}+${oy3} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.25 +channel -resize $((75+${s_Arr[4]}))% -rotate ${a_Arr[4]} \) \
		-gravity center -geometry +${ox4}+${oy4} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.4 +channel -resize $((50+${s_Arr[5]}))% -rotate ${a_Arr[5]} \) \
		-gravity center -geometry +${ox5}+${oy5} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.4 +channel -resize $((50+${s_Arr[6]}))% -rotate ${a_Arr[6]} \) \
		-gravity center -geometry +${ox6}+${oy6} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.4 +channel -resize $((50+${s_Arr[7]}))% -rotate ${a_Arr[7]} \) \
		-gravity center -geometry +${ox7}+${oy7} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.4 +channel -resize $((50+${s_Arr[8]}))% -rotate ${a_Arr[8]} \) \
		-gravity center -geometry +${ox8}+${oy8} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[9]}))% -rotate ${a_Arr[9]} \) \
		-gravity north -geometry +${ox9}+${oy9} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[10]}))% -rotate ${a_Arr[10]} \) \
		-gravity northeast -geometry +${ox10}+${oy10} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[11]}))% -rotate ${a_Arr[11]} \) \
		-gravity east -geometry +${ox11}+${oy11} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[12]}))% -rotate ${a_Arr[12]} \) \
		-gravity southeast -geometry +${ox12}+${oy12} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[13]}))% -rotate ${a_Arr[13]} \) \
		-gravity south -geometry +${ox13}+${oy13} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[14]}))% -rotate ${a_Arr[14]} \) \
		-gravity southwest -geometry +${ox14}+${oy14} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[15]}))% -rotate ${a_Arr[15]} \) \
		-gravity west -geometry +${ox15}+${oy15} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.7 +channel -resize $((40+${s_Arr[16]}))% -rotate ${a_Arr[16]} \) \
		-gravity northwest -geometry +${ox16}+${oy16} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.6 +channel -resize $((50+${s_Arr[17]}))% -rotate ${a_Arr[17]} \) \
		-gravity center -geometry +${ox17}+${oy17} -compose over -composite -alpha off \
	\( '$overlay_setup' \) \
	\( '$overlay1' \) $composite1 \
	\( '$overlay2' \) $composite2 \
	\( '$overlay3' \) $composite3 \
	\( '$overlay4' \) $composite4 \
	"$outfile"'
elif [ "$mode" = "half" ]; then
	# set up offsets
	ox1=`echo "scale=0; (${x_Arr[1]}+$f2*$ww)/1" | bc`
	oy1=`echo "scale=0; (${y_Arr[1]}+$f2*$hh)/1" | bc`
	ox2=`echo "scale=0; (${x_Arr[2]}-$f4*$ww)/1" | bc`
	oy2=`echo "scale=0; (${y_Arr[2]}-$f4*$hh)/1" | bc`
	ox3=`echo "scale=0; (${x_Arr[3]}+$f5*$ww)/1" | bc`
	oy3=`echo "scale=0; (${y_Arr[3]}-$f2*$hh)/1" | bc`
	ox4=`echo "scale=0; (${x_Arr[4]}-$f2*$ww)/1" | bc`
	oy4=`echo "scale=0; (${y_Arr[4]}+$f3*$hh)/1" | bc`
	ox5=`echo "scale=0; (${x_Arr[5]}+$f3*$ww)/1" | bc`
	oy5=`echo "scale=0; (${y_Arr[5]}-$f3*$hh)/1" | bc`
	ox6=`echo "scale=0; (${x_Arr[6]}+$f3*$ww)/1" | bc`
	oy6=`echo "scale=0; (${y_Arr[6]}+$f3*$hh)/1" | bc`
	ox7=`echo "scale=0; (${x_Arr[7]}-$f3*$ww)/1" | bc`
	oy7=`echo "scale=0; (${y_Arr[7]}-$f3*$hh)/1" | bc`
	ox8=`echo "scale=0; (${x_Arr[8]}-$f3*$ww)/1" | bc`
	oy8=`echo "scale=0; (${y_Arr[8]}+$f3*$hh)/1" | bc`

	# process image
	eval 'convert -respect-parentheses -size ${dimensions}x${dimensions} xc:"$bgcolor" \
	\( "$infile" -alpha set -background none -vignette 0x${taper}+${radius}+${radius} -resize 500x500 -write mpr:img +delete \) \
	\( mpr:img -channel a -evaluate multiply 0.2 +channel -rotate ${a_Arr[0]} \) \
		-gravity center -geometry +0+0 -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.3 +channel -resize $((75+${s_Arr[1]}))% -rotate ${a_Arr[1]} \) \
		-gravity center -geometry +${ox1}+${oy1} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.3 +channel -resize $((75+${s_Arr[2]}))% -rotate ${a_Arr[2]} \) \
		-gravity center -geometry ++${ox2}+${oy2} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.3 +channel -resize $((75+${s_Arr[3]}))% -rotate ${a_Arr[3]} \) \
		-gravity center -geometry +${ox3}+${oy3} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.3 +channel -resize $((75+${s_Arr[4]}))% -rotate ${a_Arr[4]} \) \
		-gravity center -geometry +${ox4}+${oy4} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.5 +channel -resize $((50+${s_Arr[5]}))% -rotate ${a_Arr[5]} \) \
		-gravity center -geometry +0+${oy5} -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.5 +channel -resize $((50+${s_Arr[6]}))% -rotate ${a_Arr[6]} \) \
		-gravity center -geometry +${ox6}+0 -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.5 +channel -resize $((50+${s_Arr[7]}))% -rotate ${a_Arr[7]} \) \
		-gravity center -geometry +${ox7}+0 -compose over -composite \
	\( mpr:img -channel a -evaluate multiply 0.5 +channel -resize $((50+${s_Arr[8]}))% -rotate ${a_Arr[8]} \) \
		-gravity center -geometry +0+${oy8} -compose over -composite \
	\( '$overlay_setup' \) \
	\( '$overlay1' \) $composite1 \
	\( '$overlay2' \) $composite2 \
	\( '$overlay3' \) $composite3 \
	\( '$overlay4' \) $composite4 \
	"$outfile"'
fi

exit 0
