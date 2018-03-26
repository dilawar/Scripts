#!/bin/bash
# 
# Modified by Fred Weinhaus 4/2/2009 ............ revised 4/25/2015
# Enhanced by Anthony Thyssen ................... revised 1/8/2009
# Developed by Fred Weinhaus 11/5/2007 .......... revised 4/25/2015
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
# USAGE: morphology [-m mode] [-t type] [-i iterations] [-r ramp] infile outfile
# USAGE: morphology [-h or -help]
# 
# OPTIONS:
# -m mode          binary or grayscale morphology; default=binary
# -t type          dilate, erode, open, close, majority, edgein, 
#                  edgeout, gradient, feather, average, spread, tophat, 
#                  bottomhat, thin, prune, thicken; default=close
# -i iterations    number of iterations to perform; default=1 for 
#                  all but thin, prune and thicken for which the 
#                  default is to iterate until convergence.
# -r ramp          ramp distance; for type=feather, the white areas 
#                  of a binary image will be ramped linearly to 
#                  black from ramp distance inside the white area up 
#                  to the black edge; ramp>=0; default=0
# 
###
# 
# NAME: MORPHOLOGY
# 
# PURPOSE: To perform binary or grayscale morphologic operations on an image,
# including dilate, erode, open and close.
# 
# DESCRIPTION: MORPHOLOGY applies any one of the following morphologic
# operations to an image: dilate, erode, open or close, and other similar
# types of operations, depending on the values in a 3x3 neighborhood of each
# pixel in the image. The dilate operation returns the maximum value in the
# neighborhood. The erode operation returns the minimum value in the
# neighborhood. The open operation performs a dilate followed by an erode.
# The close operations performs an erode followed by a dilate. If the mode
# is set to binary, the script assumes the input image is binary
# (thresholded to black and white) and applies a much more efficient
# algorithm.
# 
# OPTIONS:
# 
# -m mode ... MODE specifies the algorithmic approach to use. The allowed
# values are binary and grayscale. If mode is set to binary, the script
# assumes the input image will be binary (thresholded to 0 and max quantum
# value) and will apply a more efficient algorithm. If mode is set to
# grayscale, the script will apply a slightly less efficient algorithm that
# can be used on grayscale or color images. The default is binary.
# 
# -t type ... TYPE specifies the morphologic technique to apply to the image.
# The morphological operators: dilate, erode, open, close, tophat, bottomhat,
# and gradient can be used in any mode.  For 'binary' mode you can also
# use edgein, edgeout, majority, feather, thin, prune and thicken. 
# For 'grayscale' mode you can also use average and spread.
# 
# The dilate operation returns the maximum value in the neighborhood.
# 
# The erode operation returns the minimum value in the neighborhood.
# 
# The open operation performs a dilate followed by an erode. 
# 
# The close operation performs an erode followed by a dilate. 
# 
# The tophat operation is the result produced from the open operation  
# subtracted from the original image.
# 
# The bottomhat operation is the result produced from the close operation  
# subtracted by the original image.
# 
# The gradient operation is the result produced by subtracting the erode  
# result from the dilate result.
# 
# The edgein operation return the interior edge at the transition between 
# white and black. (Binary mode only)
# 
# The edgeout operation return the exterior edge at the transition between 
# white and black. (Binary mode only)
# 
# The majority operation just selects the majority from all the neighborhood 
# values. 
# 
# The thin operation strips the outer layers of the white areas and if
# iterated until convergence produces a skeleton. (Binary mode only)
# 
# The prune operation is generally used to remove spurs left from the 
# thinning operation. It is typically used only a specific number of iterations. 
# If iterated until convergence, it will produce a totally black image unless 
# the thinned white area is a closed curve. (Binary mode only)
# 
# The thicken operation produces a 45 degree convex hull, if continued to 
# convergence. (Binary mode only) 
# 
# The feather operation will linearly ramp from white to black, but just 
# within the white area. Only one iteration is allowed in this case. 
# (Binary mode only, but produces a grayscale result)
#
# The average operation generates a simple average of the neighborhood. 
# (Grayscale mode only, but works on color images, also)
#
# The spread operation does the same thing as average, but assumes the input 
# image has an alpha channel and resets pixels with a defined color back to
# their original color after each iteration.  That is, it spreads the color
# in an average way out into the transparent areas of the image.
# (Grayscale mode only, but works on color images, also)
# 
# When the input is a binary image, dilate/erode will add/remove white
# pixels at all transitions from white to black. Similarly, open/close will
# make interior black areas larger or smaller, but keep the overall outer
# shape about the same.
# 
# When the input image is grayscale or color, dilate/erode and close/open
# will brighten/darken the color transitions.
# 
# -i iterations ... ITERATIONS specifies the number of iterations to apply for
# the specified technique. Note that for open and close, the iterations are
# applied successively to the first erode or dilate and then repeated
# successively for the second dilate or erode. Iteration is ignored for
# binary mode edgein and edgeout. The default is 1 for all but thin, prune 
# and thicken, which run until convergence as a default.
# 
# -r ramp ... RAMP distance for type=feather only. The white areas of a 
# binary image will be linearly ramped to black from ramp distance inside 
# the white area up to the black edge. Ramp is a float>=0; The default=0.
# 
# NOTE: The thin, prune and thicken operations can be slow due to each 
# iteration containing 8 complex operations.
# 
# References: 
# http://homepages.inf.ed.ac.uk/rbf/HIPR2/morops.htm
# http://www.naun.org/journals/computers/ijcomputers-11.pdf
# http://www.ene.unb.br/~juliana/cursos/pimagens/chapter6.pdf
# 
# CAVEAT: No guarantee that this script will work on all platforms, nor that
# trapping of inconsistent parameters is complete and foolproof. Use At Your
# Own Risk.
# 
# ENHANCEMENTS:
# Anthony Thyssen 1/8/2009: speed and added types=average and spread
# Fred Weinhaus 4/2/2009: added type=feather
# Fred Weinhaus 4/14/2009: added type=tophat,bottomhat,thin,prune,thicken
# Fred Weinhaus 4/16/2009: added type=gradient
# 
######
# 

# set default values
mode="binary"  # binary or grayscale
type="open"    # dilate, erode, open, close, majority, edgein, edgeout, average, spread, tophat, bottomhat, thin, prune, thicken
iter=""        # initialize and determine default of 1 or convergence later
ramp=0		   # ramp distance for type=feather
#rmse=1000000   # init rmse so won't stop on first iteration

# set directory for temporary folder and files
dir="/tmp"    # suggestions are dir="." or dir="/tmp"

# set up functions to report Usage and Usage with Description
PROGNAME=`type $0 | awk '{print $3}'`  # search for executable on path
PROGDIR=`dirname $PROGNAME`            # extract directory of program
PROGNAME=`basename $PROGNAME`          # base name of program
usage() 
	{
	echo >&2 ""
	echo >&2 "$PROGNAME:" "$@"
	sed >&2 -e '1,/^####/d;  /^###/g;  /^#/!q;  s/^#//;  s/^ //;  4,$p' "$PROGDIR/$PROGNAME"
	}
usage_verbose() 
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
	usage
	exit 1
	}


# function to test for minus at start of value of second part of option 1 or 2
checkMinus()
	{
	test=`echo "$1" | grep -c '^-.*$'`   # returns 1 if match; 0 otherwise
    [ $test -eq 1 ] && errMsg "$errorMsg"
	}

# test for correct number of arguments and get values
if [ $# -eq 0 ]; then
	# help information
	echo ""
	usage_verbose
	exit 0
elif [ $# -gt 8 ]; then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
  while [ $# -gt 0 ]; do
    # get parameter values
    case "$1" in
      -h|-help) # help information
           usage_verbose
           exit 0
           ;;
      -m)  # mode
           shift  # to get the next parameter - mode
           # test if parameter starts with minus sign
           errorMsg="--- INVALID MODE SPECIFICATION ---"
           checkMinus "$1"
           # test mask values
           mode="$1"
           mode=`echo "$mode" | tr "[:upper:]" "[:lower:]"`
           [ "$mode" != "binary" -a "$mode" != "grayscale" ] &&
              errMsg "--- MODE=$mode IS NOT A VALID VALUE ---"
           ;;
      -t) # type
          shift  # to get the next parameter - type
          # test if parameter starts with minus sign
          errorMsg="--- INVALID TYPE SPECIFICATION ---"
          checkMinus "$1"
          # test region values
          type="$1"
          type=`echo "$type" | tr "[:upper:]" "[:lower:]"`
          case "$type" in
            erode|dilate|open|close|majority|feather|edgein|edgeout|average|spread|tophat|bottomhat|thin|prune|thicken|gradient) ;;
            *)  errMsg "--- TYPE=$type IS NOT A VALID VALUE ---" ;;
          esac
          ;;
      -i) # get iterations
          shift  # to get the next parameter - opacity
          # test if parameter starts with minus sign
          errorMsg="--- INVALID ITERATIONS SPECIFICATION ---"
          checkMinus "$1"
          # test width values
          iter=`expr "$1" : '\([0-9]*\)'`
          [ "$iter" = "" ] &&
             errMsg "ITERATIONS=$iter IS NOT A NON-NEGATIVE INTEGER"
          itertest=`echo "$iter < 1" | bc`
          [ $itertest -eq 1 ] &&
             errMsg "--- ITERATIONS=$iter MUST BE A POSITIVE INTEGER ---"
          ;;
      -r) # get ramp distance
          shift  # to get the next parameter - ramp
          # test if parameter starts with minus sign
          errorMsg="--- INVALID RAMP SPECIFICATION ---"
          checkMinus "$1"
          # test width values
          ramp=`expr "$1" : '\([.0-9]*\)'`
          [ "$ramp" = "" ] &&
             errMsg "RAMP=$ramp IS NOT A NON-NEGATIVE INTEGER"
          ;;
      -)  # STDIN, end of arguments
          break
          ;;
      -*) # any other - argument
          errMsg "--- UNKNOWN OPTION ---"
          ;;
      *)  # end of arguments
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

tmpdir="$dir/$PROGNAME.$$"
mkdir "$tmpdir" || errMsg "Failed to create temporary file directory"
TMP="$tmpdir/morphology_$$.mpc"
TMPORIG="$tmpdir/morphology_orig_$$.mpc"
TMPOLD="$tmpdir/morphology_old_$$.mpc"
trap "rm -rf $tmpdir;" 0
trap "rm -ff $tmpdir; exit 1" 1 2 3 15
trap "rm -ff $tmpdir; exit 1" ERR

# read the input image into the TMP cached image.
convert -quiet "$infile" +repage "$TMP" ||
  errMsg "--- FILE $infile NOT READABLE OR HAS ZERO SIZE ---"

# set default for iteration depending upon type
if [ "$iter" != "" ]; then
	iter=$iter
elif [ "$iter" = "" -a "$type" != "thin" -a "$type" != "prune" -a "$type" != "thicken" ]; then
	iter=1
else
	iter=1000000
fi

# shifted images
shifted="$tmpdir/shifted"
shift1="$shifted-1.png"
shift2="$shifted-2.png"
shift3="$shifted-3.png"
shift4="$shifted-4.png"
shift5="$shifted-5.png"
shift6="$shifted-6.png"
shift7="$shifted-7.png"
shift8="$shifted-8.png"

# function to create 8 1-pixel shifted images in each direction from center
grav1="NorthWest"
grav2="North"
grav3="NorthEast"
grav4="West"
grav5="East"
grav6="SouthWest"
grav7="South"
grav8="SouthEast"

shift_size=`identify -format '%[fx:w-2]x%[fx:h-2]+1+1' $TMP`

create_shifted_images() {
  # create 8 shifted images from the given image
  for ((j=1; j<=8; j++)); do
    eval grav=\$grav$j
    eval img=\$shift$j
    convert "$1" "$1"[${shift_size}] -gravity $grav -composite $img
  done
}

# process image
if [ $iter -eq 1000000 ]; then
	convert $TMP $TMPOLD
fi

if [ "$mode" = "binary" ]; then
  # Binary images can be processed using a fast convolution method

  if [ "$type" = "dilate" ]; then
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 0 $TMP
    done

  elif [ "$type" = "erode" ]; then
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 99% $TMP
    done

  elif [ "$type" = "close" -o "$type" = "bottomhat" ]; then
    if [ "$type" = "bottomhat" ]; then
      convert $TMP $TMPOLD
    fi
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 0 $TMP
    done
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 99% $TMP
    done
    if [ "$type" = "bottomhat" ]; then
      convert $TMP $TMPOLD +swap -compose minus -composite $TMP
    fi

  elif [ "$type" = "open" -o "$type" = "tophat" ]; then
    if [ "$type" = "tophat" ]; then
      convert $TMP $TMPOLD
    fi
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 99% $TMP
    done
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 0 $TMP
    done
    if [ "$type" = "tophat" ]; then
      convert $TMPOLD $TMP +swap -compose minus -composite $TMP
    fi

  elif [ "$type" = "gradient" ]; then
    convert $TMP $TMPORIG
  	# dilate
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 0 $TMP
    done
    convert $TMP $TMPOLD
    convert $TMPORIG $TMP
    # erode
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 99% $TMP
    done
    # dilate - erode
    convert $TMPOLD $TMP +swap -compose minus -composite $TMP

  elif [ "$type" = "majority" ]; then
    for ((i=1; i<=$iter; i++)); do
      convert $TMP -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 50% $TMP
    done

  elif [ "$type" = "feather" ]; then
  	# note only one iteration allowed
  	if [ "$ramp" != "0" -a "$ramp" != "0.0" ]; then
		convert $TMP -blur ${ramp}x65535 -black-threshold 50% $TMP
	else
		errMsg "--- RAMP FOR FEATHERING MUST BE GREATER THAN 0 ---"
	fi

  elif [ "$type" = "edgeout" ]; then
    convert $TMP \( +clone -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 0 \) \
            -compose difference -composite -threshold 99% $TMP

  elif [ "$type" = "edgein" ]; then
    convert $TMP \( +clone -define convolve:scale=\! -convolve "1,1,1,1,1,1,1,1,1" -threshold 99% \) \
            -compose difference -composite -threshold 99% $TMP

  elif [ "$type" = "thin" ]; then
	echo ""
	i=1
	until [ "$rmse" = "0" -o $i -gt $iter ]; do
		echo "iteration=$i"
		# struct1
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct2
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 100% \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct3
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct4
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct5
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct6
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct7
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct8
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		if [ $iter -eq 1000000 ]; then
			rmse=`compare -metric rmse $TMP $TMPOLD null: 2>&1`
			rmse=`echo "$rmse" | cut -d\  -f1`
			echo "rmse=$rmse"
			convert $TMP $TMPOLD
		fi
		i=`expr $i + 1`
	done
	echo ""

  elif [ "$type" = "prune" ]; then
	echo ""
	i=1
	until [ "$rmse" = "0" -o $i -gt $iter ]; do
		echo "iteration=$i"
		# struct1
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct2
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct3
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct4
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct5
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct6
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift5 -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct7
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		# struct8
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift2 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift7 -evaluate xor 0 \) \
			-compose plus -flatten \) \
			-compose multiply -composite $TMP
		if [ $iter -eq 1000000 ]; then
			rmse=`compare -metric rmse $TMP $TMPOLD null: 2>&1`
			rmse=`echo "$rmse" | cut -d\  -f1`
			echo "rmse=$rmse"
			convert $TMP $TMPOLD
		fi
		i=`expr $i + 1`
	done
	echo ""

  elif [ "$type" = "thicken" ]; then
	echo ""
	i=1
	until [ "$rmse" = "0" -o $i -gt $iter ]; do
		echo "iteration=$i"
		# struct1
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct2
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct3
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 0 \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct4
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct5
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 0 \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift5 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct6
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct7
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift3 -evaluate xor 0 \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift6 -evaluate xor 100% \) \
			\( $shift7 -evaluate xor 100% \) \
			\( $shift8 -evaluate xor 100% \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		# struct8
		create_shifted_images $TMP
		convert $TMP \
			\( -background black \
			\( $shift1 -evaluate xor 100% \) \
			\( $shift2 -evaluate xor 100% \) \
			\( $shift3 -evaluate xor 100% \) \
			\( $shift4 -evaluate xor 100% \) \
			\( $TMP -evaluate xor 0 \) \
			\( $shift8 -evaluate xor 0 \) \
			-compose plus -flatten -negate \) \
			-compose plus -composite $TMP
		if [ $iter -eq 1000000 ]; then
			rmse=`compare -metric rmse $TMP $TMPOLD null: 2>&1`
			rmse=`echo "$rmse" | cut -d\  -f1`
			echo "rmse=$rmse"
			convert $TMP $TMPOLD
		fi
		i=`expr $i + 1`
	done
	echo ""
  fi

elif [ "$mode" = "grayscale" ]; then
  # Grayscale images (only some method types) requires a more complex
  # comparsion convolution method

  if [ "$type" = "dilate" ]; then
    for ((i=1; i<=$iter; i++)); do
      # DILATE: maximum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#0000' -compose lighten -flatten $TMP
    done

  elif [ "$type" = "erode" ]; then
    for ((i=1; i<=$iter; i++)); do
      # ERODE: minimum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#FFF0' -compose darken -flatten $TMP
    done

  elif [ "$type" = "close" -o "$type" = "bottomhat" ]; then
    if [ "$type" = "bottomhat" ]; then
      convert $TMP $TMPOLD
    fi
    for ((i=1; i<=$iter; i++)); do
      # DILATE: maximum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#0000' -compose lighten -flatten $TMP
    done
    for ((i=1; i<=$iter; i++)); do
      # ERODE: minimum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#FFF0' -compose darken -flatten $TMP
    done
    if [ "$type" = "bottomhat" ]; then
      convert $TMP $TMPOLD -alpha off +swap -compose minus -composite $TMP
    fi

  elif [ "$type" = "open" -o "$type" = "tophat" ]; then
    if [ "$type" = "tophat" ]; then
      convert $TMP $TMPOLD
    fi
    for ((i=1; i<=$iter; i++)); do
      # ERODE: minimum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#FFF0' -compose darken -flatten $TMP
    done
    for ((i=1; i<=$iter; i++)); do
      # DILATE: maximum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#0000' -compose lighten -flatten $TMP
    done
    if [ "$type" = "tophat" ]; then
      convert $TMPOLD $TMP -alpha off +swap -compose minus -composite $TMP
    fi

  elif [ "$type" = "gradient" ]; then
    convert $TMP $TMPORIG
  	# dilate
    for ((i=1; i<=$iter; i++)); do
      # DILATE: maximum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#0000' -compose lighten -flatten $TMP
    done
    convert $TMP $TMPOLD
    convert $TMPORIG $TMP
    # erode
    for ((i=1; i<=$iter; i++)); do
      # ERODE: minimum value convolution - shift and compare
      create_shifted_images $TMP
      convert $TMP $shifted-* \
              -background '#FFF0' -compose darken -flatten $TMP
    done
    # dilate - erode
    convert $TMPOLD $TMP -alpha off +swap -compose minus -composite $TMP

  elif [ "$type" = "average" ]; then
    for ((i=1; i<=$iter; i++)); do
      # AVERAGE blur the pixels.
      convert $TMP -channel RGBA -blur 2x65000 $TMP
    done

  elif [ "$type" = "spread" ]; then
    for ((i=1; i<=$iter; i++)); do
      # SPREAD the pixel colors from the image edges into the transparent parts
      convert $TMP \( +clone -channel RGBA -blur 2x65000 \) \
              -compose DstOver -composite $TMP
    done
  else
    errMsg "--- INVALID TYPE FOR GRAYSCALE MODE ---"
  fi
else
  # This actually should be not possible, sanity check
  errMsg "--- INVALID MODE (BINARY OR GRAYSCALE) ---"
fi

# Final Output
convert $TMP "$outfile"

exit 0


# Structure Elements
#
# Thin
# 
# see http://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm
# if pattern matches change center pixel to 0
#
# struct1
# 0 0 0
# x 1 x 
# 1 1 1
#
# struct2
# x 0 0
# 1 1 0
# x 1 x
#
# struct3
# 1 x 0
# 1 1 0
# 1 x 0
#
# struct4
# x 1 x
# 1 1 0
# x 0 0
#
# struct5
# 1 1 1
# x 1 x
# 0 0 0
#
# struct6
# x 1 x
# 0 1 1
# 0 0 x
#
# struct7
# 0 x 1
# 0 1 1
# 0 x 1
#
# struct8
# 0 0 x
# 0 1 1
# x 1 x
#
#
# Prune
# 
# see http://homepages.inf.ed.ac.uk/rbf/HIPR2/thin.htm
# if pattern matches change center pixel to 0
#
# struct1
# 0 0 0
# 0 1 0 
# 0 x x
#
# struct2
# 0 0 0
# 0 1 0
# x x 0
#
# struct3
# 0 0 0
# x 1 0
# x 0 0
#
# struct4
# X 0 0
# X 1 0
# 0 0 0
#
# struct5
# x x 0
# 0 1 0
# 0 0 0
#
# struct6
# 0 x x
# 0 1 0
# 0 0 0
#
# struct7
# 0 0 x
# 0 1 x
# 0 0 0
#
# struct8
# 0 0 0
# 0 1 x
# 0 0 x
#
#
# Thicken
#
# see http://homepages.inf.ed.ac.uk/rbf/HIPR2/thick.htm
# if pattern matches change center pixel to 1
#
# struct1
# 1 1 x
# 1 0 x 
# 1 x 0
#
# struct2
# x 1 1
# x 0 1
# 0 x 1
#
# struct3
# 1 1 1
# x 0 1
# 0 x x
#
# struct4
# 0 x x
# x 0 1
# 1 1 1
#
# struct5
# 0 x 1
# x 0 1
# x 1 1
#
# struct6
# 1 x 0
# 1 0 x
# 1 1 x
#
# struct7
# x x 0
# 1 0 x
# 1 1 1
#
# struct8
# 1 1 1
# 1 0 x
# x x 0
