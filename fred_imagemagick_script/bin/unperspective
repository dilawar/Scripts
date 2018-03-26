#!/bin/bash
# 
# Developed by Fred Weinhaus 11/16/2011 .......... revised 9/18/2016
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
# USAGE: unperspective [-P prerotate] [-p procedure] [-C channel] [-c coords] 
# [-b bgcolor] [-f fuzzval] [-F filter] [-A area] [-a aspect] [-w width] 
# [-h height] [-d default] [-m method] [-t thresh] [-s smooth] [-S sharpen] 
# [-B blur] [-r rotate] [-M] [-i images] [-k kind] [-ma maxaspect] 
# [-ml minlength] [-mp maxpeaks] [-mr maxratio] [-T traps] [-V] infile outfile
# 
# USAGE: unperspective [-help]
# 
# OPTIONS:
# 
# -P     prerotate      prerotate image; choices are: autorotate (a), 90, 
#                       180, 270; default is no prerotate; autorotate only 
#                       works, if the image has auto-orient metadata
# -p     procedure      background extraction procedure; choices are: 
#                       floodfill (f), threshold (t), autothresh (a); 
#                       autothresh requires my otsuthresh script; 
#                       default=floodfill
# -C     channel        image channel to use for non-floodfill background 
#                       extraction; choices are: gray, red, green, blue, cyan, 
#                       magenta, yellow, black; default is no specific channel
# -c     coords         pixel coordinate to extract background color; 
#                       may be expressed as gravity value 
#                       (e.g. northwest) or as "x,y" value; 
#                       default is 0,0; used with -p floodfill
# -b     bgcolor		background color outside the distorted 
#                       quadrilateral; any valid IM color;
#                       default determined by coords argument; used with 
#                       -p floodfill
# -f     fuzzval        fuzz or threshold value expressed as percent for    
#                       isolating quadrilateral from background; 
#                       0<=float<=100; default=10
# -F     filter         morphology filter to smooth gaps and bumps on the 
#                       mask boundary; integer>=0; default=0
# -A     area           area threshold for connected components filtering of 
#                       mask image expressed as percent of image area; 
#                       0<integer<100; default is no connected components 
#                       filtering; requires IM 6.9.2-8 or higher 
# -a     aspect			desired width/height aspect ratio; float>0; 
#                       default will be computed automatically
# -w     width          desired width of output; default determined 
#                       automatically from "default" parameter below;
#                       only one of width or height may be specified
# -h     height         desired height of output; default determined 
#                       automatically from "default" parameter below;
#                       only one of width or height may be specified
# -d     default        default output dimension; choices are: el 
#                       (length of first edge of quadrilateral used 
#                       as height), bh (quadrilateral bounding box 
#                       height), bw (quadrilateral bounding box width),
#                       h (input image height), w (input image width);
#                       default=el
# -m     method         method of determining quadrilateral corners 
#                       from peaks in depolar image; choices are: 
#                       peak (p) or derivative (d); default=peak
# -t     thresh         threshold value for removing false peaks; integer>=0;
#                       default=4 for method=peak; 
#                       default=10 for method=derivative
# -s     smooth         smoothing amount to remove false peaks; float>=0;
#                       default=1 for method=peak; 
#                       default=5 for method=derivative
# -S     sharpen        sharpening amount to amplify true peaks; float>=0;
#                       default=5 for method=peak; 
#                       default=0 for method=derivative
# -B     blur           blurring amount for preprocessing images of 
#                       text with no quadrilateral outline; float&gt;=0
# -r     rotate         desired rotation of output image; choices are: 
#                       90, 180 or 270; default is no rotation
# -M                    monitor and display textual information about  
#                       processing to the terminal
# -i     images         keep ancillary processing images; choices are:
#                       view or save; default is neither
# -k     kind           kind of ancillary processing images; choices are:
#                       mask, polar, edge or all; default=mask
# -ma    maxaspect      trap for maximum aspect ratio; integer>0; 
#                       default=10
# -ml    minlength      trap for minimum edge length; integer>0; 
#                       default=10
# -mp    maxpeaks       trap for maximum number of false peaks before 
#                       filtering to remove false peaks; integer>0; 
#                       default=40
# -mr    maxratio       trap for maximum intermediate/input dimension ratio; 
#                       integer>0; default=10
# -T     traps          turn off internal traps; choices are; maxaspect (ma),
#                       minlenght (ml), maxpeaks (mp), maxratio (mr) or all (a)
# -V                    disable viewport crop of output
# 
# 
###
# 
# NAME: UNPERSPECTIVE 
#  
# PURPOSE: To automatically remove pespective distortion from an image.
# 
# DESCRIPTION: UNPERSPECTIVE attempts to automatically remove pespective
# distortion from an image without the need to manually pick control points.
# This technique is limited and relies upon the ability to isolate the
# outline or boundary of the distorted quadrilateral in the input image
# from its surrounding background. This technique will not look for
# internal edges or other details to assess the distortion. This technique
# also works to correct affine distortions such as rotation and/or skew.
# 
# The basic principal is to isolate the quadrilateral of the distorted 
# region from its background to form a binary mask. The mask is converted 
# from cartesian coordinates to polar coordinates and averaged down to 
# one row. This row is then processed either to find the highest peaks 
# or the highest second derivative peaks. The four peaks identified are 
# then converted back to cartesian coordinates and used with the ouput 
# dimensions determined from the user specified (or computed) aspect 
# ratio and user specified dimension.
#
# The processing is done using +distort perspective and thus a larger 
# intermediate image is computed to encompass the undistorted result. 
# Potential errors in finding the peaks may cause excessively large 
# intermediate images to be generated taking a long time and much disk 
# space. Therefore several traps have been specified, though one may 
# disable them if one wants.
#
# Also to avoid erroneous situations, there is an option to view or save 
# some ancillary images. These include the quadrilateral mask, a graph 
# of the 1D polar domain image or its second derivative, the outline 
# of the edges drawn between the four corners found from the peaks, or 
# all of them. If this mode is enabled, then processing will pause at 
# a prompt on the terminal after each image is displayed (not saved) 
# so that one may decide to continue or quit so as to avoid erroneous 
# situations.
#
# The most important one is the mask. If the mask shows that the fuzzy 
# floodfill to remove the outside background color is either insufficient 
# to isolate the quadrilateral cleanly or has incursions into the 
# quadrilateral, then the processing will fail. In note, the peak method
# (default) is more robust to such flaws than the derivative method. It 
# is also more robust to evenly rounded corners on the quadrilateral region. 
# However, the peak method does not work well for aspect ratios greater 
# than about five; whereas the derivative method is more robust in this 
# regard and can work with aspect ratios of at least 10.
# 
# It is highly recommended that one ensures that the quadrilateral is 
# cleanly separated from the background. This can be done either by 
# enabling the mask view in the script so you can quit it if there is 
# insufficient separation or prior to running the script do your own 
# fuzzy floodfill. See http://www.imagemagick.org/Usage/draw/#matte
#
# One may also enable monitoring of the processing steps to get textual 
# information about the progress and also numerical information about the 
# peaks, corner coordinates found, aspect ratio and sizes and ratios of 
# the intermediate images. This is helpful when the result is not 
# satisfactory or has quit from the traps so that one may refine some 
# of the arguments. 
# 
# 
# Arguments: 
# 
# 
# -P prerotate ... PREROTATE the input image. The choices are: autorotate (a), 
# 90, 180, 270. The default is no prerotate. Autorotate only works, if 
# the image has auto-orient metadata.
# 
# -p procedure ... background extraction PROCEDURE. The choices are: 
# floodfill (f), threshold (t), autothresh (a). NOTE: -p autothresh requires 
# my otsuthresh script. The default=floodfill.
# 
# -C channel ... the image CHANNEL to use for non-floodfill background 
# extraction. The choices are: gray, red, green, blue, cyan, magenta, yellow. 
# The default is no specific channel. This can be useful, if one can 
# select a channel that will have the highest contrast between the foreground 
# and background color. Typically that will be the complement of the 
# background colors closest primary or secondary color. For example, if the 
# background color is yellowish, then choose the blue channel.
#
# -c coords ... COORDS is any location within the input image for the 
# algorithm to find the background color. It may be specified in terms of 
# gravity parameters (NorthWest, North, NorthEast, East, SouthEast, South, 
# SouthWest or West) or as a pixel coordinate "x,y". The default is the 
# upper left corner = NorthWest = "0,0". Used only with -p floodfill.
# 
# -b bgcolor ... BGCOLOR is the	background color outside the distorted 
# quadrilateral. Any valid IM color is allowed. The background color 
# is used to do a fuzzy trim of the image to the bounding box around 
# the quadrilateral. The default color is determined by the above 
# "coords" argument. Used only with -p floodfill.
#
# -f fuzzval ... FUZZVAL is the fuzz amount specified as a float percent 
#  0 to 100 (without the % sign). It is used 1) for trimming the image to 
# bounding box about the quadrilaterls, 2) for floodfilling the background 
# to convert the the quadrilateral into a binary mask and 3) for trimming 
# the output image. The default=10. Use a value that will produce a mask 
# that cleanly corresponds to the distorted quadrilateral area of the image. 
# Note that method=peak is fairly robust to minor imperfections in the mask, 
# but method=derivative is not.
# 
# -F filter ... FILTER is a morphology filter to smooth gaps and bumps on the 
# mask boundary. Values are integer2>=0. The default=0.
#
# -A area ... AREA threshold for connected components filtering of the 
# internally generate mask image expressed as percent of image area. Values 
# are in the range of 0<integers<100. The default is no connected components 
# filtering. If a value is provided, then this option requires IM 6.9.2-8 or 
# higher. 
#
# -a  aspect ... ASPECT is the desired output width/height aspect ratio
# Values are floats>0. The default will be computed automatically.
# 
# -w width ... WIDTH is the desired width of the output. The default 
# is determined automatically from the "default" parameter below. 
# Only one of width or height may be specified. Note: the output 
# size will only be close to the value specified.
# 
# -h height ... HEIGHT is the desired height of the output. The default 
# is determined automatically from the "default" parameter below. 
# Only one of width or height may be specified. Note: the output 
# size will only be close to the value specified.
# 
# -d default ... DEFAULT is the default output dimension. Choices are: el 
# (length of first edge of quadrilateral used as height), bh (quadrilateral
# bounding box height), bw (quadrilateral bounding box width), 
# h (input image height), or w (input image width). The default is el.
# 
# -m method ... METHOD is the method of determining the quadrilateral 
# corners from peaks in the polar image. Choices are: peak (p) or 
# derivative (d or deriv). The default=peak
# 
# -t thresh ... THRESH is the threshold value for removing false peaks. 
# Values are integers>=0. The default=4 for method=peak and the  
# default=10 for method=derivative. Higher values remove more peaks and
# if too high can remove true corner peaks.
# 
# -s smooth ... SMOOTH is the smoothing amount used to help remove false 
# peaks. Values are floats>=0. This is filtering step applied to the 1D 
# polar image. The default=1 for method=peak and the default=5 for 
# method=derivative. Larger values will remove more peaks and if too high 
# can remove true corner peaks.
# 
# -S sharpen ... SHARPEN is the sharpening amount used to amplify true peaks. 
# Values are floats>=0. This is a filtering step applied after the smoothing 
# to the 1D polar images. The default=5 for method=peak and the default=0 
# for method=derivative. Larger values will amplify the peaks. This is 
# necessary when in method=peak, because obtuse angles in the quadrilateral 
# show up as plateaus rather than peaks. The sharpening raises them enough 
# to be turned into peaks. Sharpening also helps locate the true peaks 
# more accurately when they are rounded and not sharp.
# 
# -B blur ... BLUR is the blurring amount for preprocessing images of 
# text with no quadrilateral outline. Values are floats>=0. This option 
# is only useful for text images with no quadrilateral outline when the 
# text is evenly distributed, especially to the four corners of the 
# quadrilateral. The option only works if at all with method=peak due 
# to its more robust nature. It is also not compatible with the use of option 
# -A (connected components processing)
# 
# -r rotate ... ROTATE is the desired rotation of output image. Choices  
# are: 90, 180 or 270. The default is no rotation. The technique used  
# here can correct rotation when the top left corner of the distorted 
# quadrilateral is in the top left or top right quadrant of the input 
# image and the bottom left corner is in the bottom left quadrant of 
# the input image. Otherwise, the technique cannot automatically 
# determine 90 degree increment rotations.
# 
# -M ... ENABLE MONITORING and display textual information about  
# processing to the terminal. Such information is usefull for 
# determining progress as the non-IM bash scripting is extensive and 
# slow. It is also usefull for determining changes to the parameters 
# when the process fails to create a good image.
# 
# -i images ... IMAGES permits the viewing or saving of the ancillary 
# images generated during the processing. The choices are: view or save.
# the default is neither. This option is important because it allows 
# one to view the extracted quadrilateral (mask) to make sure it is 
# well-isolated from the background. Other images may also be useful 
# for determining why the process failed to create a good image so as 
# to permit changing of the arguments. When images=view, processing 
# will pause while one views the image and can be restarted or quit 
# from a prompt at the terminal. This is important as a bad mask image 
# can lead to bad corner points being extracted, which can lead to 
# huge or strange output images with big memory or disk requirements.
# 
# -k kind ... KIND is the kind of ancillary images that can be viewed 
# or saved. The choices are: mask (m), polar (p), edge (e) or all (a). 
# Mask is the quadrilateral isolated from the background and binarized 
# by the fuzzy floodfill process. Polar is the conversion of the mask 
# from cartesian to polar coordinates, averaged down to one row and 
# presented as a graph image. Edge is an image showing the lines 
# connecting the extracted corner coordinates from the mask image.
# The default is mask.
# 
# -ma maxaspect ... MAXASPECT is a trap on the maximum aspect ratio to 
# permit especially from the automatic aspect ratio computation. Values  
# are integers>0. The default=10.
# 
# -ml minlength ... MINLENGTH is a trap on the minimum edge length to 
# permit from the lengths between connected corners of the extracted 
# quadrilateral. Values are integers>0. The default=10.
# 
# -mp maxpeaks ... MAXPEAKS is a trap on the maximum number of peaks 
# to permit before filtering on false peaks. If too many peaks are 
# located, then processing will take a long time to filter and may 
# end up with erroneous peaks. This trap permits the user to change 
# the thresh and smooth arguments to reduce the number of peaks.
# Values are integers>0. The default=40.
# 
# -mr maxratio ... MAXRATIO is a trap on the maximum ratio between 
# the intermediate and input image's dimensions. Due to the use of 
# +distort perspective to unwarp the image, a very large image may 
# be produced before it is trimmed to its bounding box for output. 
# This trap tries to prevent such situations as this may cause 
# long processing times and need large memory and/or disk resources.
# Values are integer>0. The default=10.
# 
# -T traps ... TURNS OFF (disables) the internal TRAPS that are
# used to stop processing if their thresholds have been exceeded. The  
# traps are used to avoid situations where the result may be erroneous 
# and produce huge output images. The choices are: maxaspect (ma),
# minlenght (ml), maxpeaks (mp), maxratio (mr) or all (a). By default 
# all traps are on.
#
# -V ... disables the viewport crop of the output image and allows
# +distort to compute a larger output image before doing a fuzzy
# trim. When disabled, processing time will be longer. Disabling
# the viewport crop is usefull when the resulting image is cropped
# too much, such as can occur when the quadrilateral has rounded corners. 
#
# REFERENCES:
# http://stackoverflow.com/questions/3790445/1d-multiple-peak-detection
# http://www.sagenb.org/home/pub/704/
# http://research.microsoft.com/users/zhang/Papers/WhiteboardRectification.pdf
# http://research.microsoft.com/en-us/um/people/zhang/papers/tr03-39.pdf
# 
# REQUIREMENTS: Method=peak requires IM 6.4.2.6 or higher due to the  
# use of -distort depolar. Method=derivative requires IM 6.5.9.2 due 
# to the use of -morphology. NetPBM is needed for both methods. IM 6.9.2-8 
# or higher is needed for the connected components processing when using -A. 
# The argument -p autothresh requires my otsuthresh script.
# 
# CAVEAT: No guarantee that this script will work on all platforms, 
# nor that trapping of inconsistent parameters is complete and 
# foolproof. Use At Your Own Risk. 
# 
######
#
# set default values; 
prerotate=""		# prerotate image: 90, 180, 270 or autorotate
procedure=floodfill	# masking procedure: floodfill, threshold, auto
channel=""          # convert to grayscale via channel
coords="0,0"		# coords to get background color
bgcolor=""			# background color overwrites coords
fuzzval=10			# fuzz value for isolating quadrilateral from background
filter=0			# morpholgy smoothing filter for mask boundary
area=""				# connected components area as percent of image area
aspect=""			# desired aspect ratio (width/height)
width=""			# desired output width; only one of width or height
height=""			# desired output height; only one of width or height 
default="el"		# default dimension: el, bh, bw, h, w
method="peak"		# method of finding corners: peak or derivative
thresh=""			# threshold for removing false peaks
smooth=""			# smoothing amount for 1D polar image
sharpen=""			# sharpening of 1D polar image
blur=0				# blurring for use with text images
rotate="0"			# 90 rotation increments
monitor="no"		# show/monitor details of processing; yes or no
images=""			# save or view ancillary image(s); default=neither
kind="mask"			# kind of ancillary image: mask, polar, edge, all
maxaspect=10		# trap: maximum aspect ratio
minlength=10		# trap: minimum length of extracted edge
maxpeaks=40			# trap: threshold on peak number before removing false peaks
maxratio=10			# trap: maximum intermediate/input dimensions ratio
traps=""			# turn specified traps off
viewport="on"       # turn off viewport crop
debug="false"		# debugging; true or false

# set directory for temporary files
tmpdir="."		# suggestions are tmpdir="." or tmpdir="/tmp"

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
elif [ $# -gt 54 ]
	then
	errMsg "--- TOO MANY ARGUMENTS WERE PROVIDED ---"
else
	while [ $# -gt 0 ]
		do
		# get parameters
		case "$1" in
	     -help)    # help information
				   echo ""
				   usage2
				   ;;
			-P)    # get  prerotate
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID PREROTATE SPECIFICATION ---"
				   checkMinus "$1"
				   prerotate="$1"
				   case "$prerotate" in 
						autorotate|a) prerotate=autorotate;;
						90) prerotate=90;;
						180) prerotate=180;;
						270) prerotate=270;;
						*) errMsg "--- PREROTATE=$prerotate IS AN INVALID VALUE ---" 
					esac
				   ;;
			-p)    # get  procedure
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID PROCEDURE SPECIFICATION ---"
				   checkMinus "$1"
				   procedure="$1"
				   case "$procedure" in 
						floodfill|f) procedure=floodfill;;
						threshold|t) procedure=threshold;;
						autothresh|a) procedure=autothresh;;
						*) errMsg "--- PROCEDURE=$procedure IS AN INVALID VALUE ---" 
					esac
				   ;;
			-C)    # get  channel
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID CHANNEL SPECIFICATION ---"
				   checkMinus "$1"
				   channel="$1"
				   case "$channel" in 
						gray) channel=gray;;
						red) channel=red;;
						green) channel=green;;
						blue) channel=blue;;
						cyan) channel=cyan;;
						magenta) channel=magenta;;
						yellow) channel=yellow;;
						black) channel=black;;
						*) errMsg "--- CHANNEL=$channel IS AN INVALID VALUE ---" 
					esac
				   ;;
			-c)    # coords
				   shift  # to get the next parameter - coords
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID COORDS SPECIFICATION ---"
				   checkMinus "$1"
				   coords=$1
				   # further testing done later
				   ;;
			-b)    # bgcolor
				   shift  # to get the next parameter - coords
				   bgcolor=$1
				   ;;
			-f)    # fuzzval
				   shift  # to get the next parameter - fuzzval
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID FUZZVAL SPECIFICATION ---"
				   checkMinus "$1"
				   fuzzval=`expr "$1" : '\([.0-9]*\)'`
				   [ "$fuzzval" = "" ] && errMsg "--- FUZZVAL=$fuzzval MUST BE A NON-NEGATIVE FLOATING POINT VALUE (with no sign) ---"
				   test1=`echo "$fuzzval < 0" | bc`
				   test2=`echo "$fuzzval > 100" | bc`
				   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- FUZZVAL=$fuzzval MUST BE A NON-NEGATIVE FLOATING POINT VALUE BETWEEN 0 AND 100 ---"
				   ;;
			-F)    # filter
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID FILTER SPECIFICATION ---"
				   checkMinus "$1"
				   filter=`expr "$1" : '\([0-9]*\)'`
				   [ "$filter" = "" ] && errMsg "--- FILTER=$filter MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   ;;
			-A)    # area
				   shift  # to get the next parameter - fuzzval
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID AREA SPECIFICATION ---"
				   checkMinus "$1"
				   area=`expr "$1" : '\([0-9]*\)'`
				   [ "$area" = "" ] && errMsg "--- AREA=$area MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$area <= 0" | bc`
				   test2=`echo "$area >= 100" | bc`
				   [ $test1 -eq 1 -o $test2 -eq 1 ] && errMsg "--- AREA=$area MUST BE A NON-NEGATIVE INTEGER GREATER THAN 0 AND LESS THAN 100 ---"
				   ;;
			-a)    # aspect
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID ASPECT SPECIFICATION ---"
				   checkMinus "$1"
				   aspect=`expr "$1" : '\([.0-9]*\)'`
				   [ "$aspect" = "" ] && errMsg "--- ASPECT=$aspect MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
				   test1=`echo "$aspect <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- ASPECT=$aspect MUST BE A POSITIVE FLOAT ---"
				   ;;
			-w)    # width
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID WIDTH SPECIFICATION ---"
				   checkMinus "$1"
				   width=`expr "$1" : '\([0-9]*\)'`
				   [ "$width" = "" ] && errMsg "--- WIDTH=$width MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$width <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- WIDTH=$width MUST BE A POSITIVE INTEGER ---"
				   ;;
			-h)    # height
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID HEIGHT SPECIFICATION ---"
				   checkMinus "$1"
				   height=`expr "$1" : '\([0-9]*\)'`
				   [ "$height" = "" ] && errMsg "--- HEIGHT=$height MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$height <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- HEIGHT=$height MUST BE A POSITIVE INTEGER ---"
				   ;;
			-d)    # get  default
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID DEFAULT SPECIFICATION ---"
				   checkMinus "$1"
				   default=`echo "$1" | tr "[:upper:]" "[:lower:]"`
				   case "$default" in 
						el) default=el;;
						bh) default=bh;;
						bw) default=bw;;
						h) default=h;;
						w) default=w;;
						*) errMsg "--- DEFAULT=$default IS AN INVALID VALUE ---" 
					esac
				   ;;
			-m)    # get  method
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID METHOD SPECIFICATION ---"
				   checkMinus "$1"
				   method=`echo "$1" | tr "[:upper:]" "[:lower:]"`
				   case "$method" in 
						peak|p) method=peak;;
						derivative|deriv|d) method=derivative;;
						*) errMsg "--- METHOD=$method IS AN INVALID VALUE ---" 
					esac
				   ;;
			-t)    # thresh
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID THRESH SPECIFICATION ---"
				   checkMinus "$1"
				   thresh=`expr "$1" : '\([0-9]*\)'`
				   [ "$thresh" = "" ] && errMsg "--- THRESH=$thresh MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   ;;
			-s)    # smooth
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID SMOOTH SPECIFICATION ---"
				   checkMinus "$1"
				   smooth=`expr "$1" : '\([.0-9]*\)'`
				   [ "$smooth" = "" ] && errMsg "--- SMOOTH=$smooth MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
				   ;;
			-S)    # sharpen
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID SHARPEN SPECIFICATION ---"
				   checkMinus "$1"
				   sharpen=`expr "$1" : '\([.0-9]*\)'`
				   [ "$sharpen" = "" ] && errMsg "--- SHARPEN=$sharpen MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
				   ;;
			-B)    # blur
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID BLUR SPECIFICATION ---"
				   checkMinus "$1"
				   blur=`expr "$1" : '\([.0-9]*\)'`
				   [ "$blur" = "" ] && errMsg "--- BLUR=$blur MUST BE A NON-NEGATIVE FLOAT (with no sign) ---"
				   ;;
			-r)    # get  rotate
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID ROTATE SPECIFICATION ---"
				   checkMinus "$1"
				   rotate="$1"
				   case "$rotate" in 
						0) rotate=0;;
						90) rotate=90;;
						180) rotate=180;;
						270) rotate=270;;
						*) errMsg "--- ROTATE=$rotate IS AN INVALID VALUE ---" 
					esac
				   ;;
			-M)    # monitor
				   monitor="yes"
				   ;;
			-i)    # images
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID IMAGES SPECIFICATION ---"
				   checkMinus "$1"
				   images=`echo "$1" | tr "[:upper:]" "[:lower:]"`
				   case "$images" in 
						view|v) images=view;;
						save|s) images=save;;
						*) errMsg "--- IMAGES=$images IS AN INVALID VALUE ---" 
					esac
				   ;;
			-k)    # kind
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID KIND SPECIFICATION ---"
				   checkMinus "$1"
				   kind=`echo "$1" | tr "[:upper:]" "[:lower:]"`
				   case "$kind" in 
						mask|m) kind=mask;;
						polar|p) kind=polar;;
						edge|e) kind=edge;;
						all|a) kind=all;;
						*) errMsg "--- KIND=$kind IS AN INVALID VALUE ---" 
					esac
				   ;;
		   -ma)    # maxaspect
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID MAXASPECT SPECIFICATION ---"
				   checkMinus "$1"
				   maxaspect=`expr "$1" : '\([0-9]*\)'`
				   [ "$maxaspect" = "" ] && errMsg "--- MAXASPECT=$maxaspect MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$maxaspect <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- MAXASPECT=$maxaspect MUST BE A POSITIVE INTEGER ---"
				   ;;
		   -ml)    # minlength
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID MINLENGTH SPECIFICATION ---"
				   checkMinus "$1"
				   minlength=`expr "$1" : '\([0-9]*\)'`
				   [ "$minlength" = "" ] && errMsg "--- MINLENGTH=$minlength MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$minlength <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- MINLENGTH=$minlength MUST BE A POSITIVE INTEGER ---"
				   ;;
		   -mp)    # maxpeaks
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID MAXPEAKS SPECIFICATION ---"
				   checkMinus "$1"
				   maxpeaks=`expr "$1" : '\([0-9]*\)'`
				   [ "$maxpeaks" = "" ] && errMsg "--- MAXPEAKS=$maxpeaks MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$maxpeaks <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- MAXPEAKS=$maxpeaks MUST BE A POSITIVE INTEGER ---"
				   ;;
		   -mr)    # maxratio
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID MAXRATIO SPECIFICATION ---"
				   checkMinus "$1"
				   maxratio=`expr "$1" : '\([0-9]*\)'`
				   [ "$maxratio" = "" ] && errMsg "--- MAXRATIO=$maxratio MUST BE A NON-NEGATIVE INTEGER (with no sign) ---"
				   test1=`echo "$maxratio <= 0" | bc`
				   [ $test1 -eq 1 ] && errMsg "--- MAXRATIO=$maxratio MUST BE A POSITIVE INTEGER ---"
				   ;;
			-T)    # traps
				   shift  # to get the next parameter
				   # test if parameter starts with minus sign 
				   errorMsg="--- INVALID TRAPS SPECIFICATION ---"
				   checkMinus "$1"
				   traps=`echo "$1" | tr "[:upper:]" "[:lower:]"`
				   case "$traps" in 
						maxaspect|ma) traps=maxaspect;;
						minlength|ml) traps=minlength;;
						maxpeaks|mp) traps=maxpeaks;;
						maxratio|mr) traps=maxratio;;
						all|a) traps=all;;
						*) errMsg "--- IMAGES=$images IS AN INVALID VALUE ---" 
					esac
				   ;;
			-V)    # viewport
				   viewport="off"
				   ;;
			 -)    # STDIN and end of arguments
				   break
				   ;;
			-*)    # any other - argument
				   errMsg "--- UNKNOWN OPTION ---"
				   ;;
			*)     # end of arguments
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


# setup defaults depending upon method
if [ "$method" = "peak" ]; then
	[ "$thresh" = "" ] && thresh=4
	[ "$smooth" = "" ] && smooth=1
	[ "$sharpen" = "" ] && sharpen=5
elif [ "$method" = "derivative" ]; then
	[ "$thresh" = "" ] && thresh=10
	[ "$smooth" = "" ] && smooth=5
	[ "$sharpen" = "" ] && sharpen=0
fi


# set up to remove temporary directory
dir="$tmpdir/UNPERSPECTIVE.$$"

mkdir "$dir" || errMsg "--- FAILED TO CREATE TEMPORARY FILE DIRECTORY ---"
trap "rm -rf $dir; exit 0" 0
trap "rm -rf $dir; exit 1" 1 2 3 15
#trap "rm -rf $dir; exit 1" ERR


#function to send prompt to terminal for displaying images to continue or quit
prompt()
	{
	echo ""
	echo "Press q to quit or any other key to continue:"
	read -n 1 prompt
	if [ "$prompt" = "q" ]; then
		prompt=""
		echo ""
		exit 1
	fi
	}

# function to get color at user specified location
getColor()
	{
	img="$1"
	wd=`identify -ping -format "%w" $img`
	ht=`identify -ping -format "%h" $img`
	widthm1=$((wd-1))
	heightm1=$((ht-1))
	midwidth=`echo "scale=0; $wd / 2" | bc`
	midheight=`echo "scale=0; $ht / 2" | bc`
	case "$coords" in
		NorthWest|Northwest|northwest)	coords="0,0"
										;;
						  North|north)	coords="$midwidth,0"
										;;
		NorthEast|Northeast|northeast)	coords="$widthm1,0"
										;;
							East|east)	coords="$widthm1,$midheight"
										;;
		SouthEast|Southeast|southeast)	coords="$widthm1,$heightm1"
										;;
						  South|south)	coords="$midwidth,$heightm1"
										;;
		SouthWest|Southwest|southwest)	coords="0,$heightm1"
										;;
							West|west)	coords="0,$midheight"
										;;
						[0-9]*,[0-9]*)	coords=$coords
										;;
									*)	errMsg "--- INVALID COORDS ---"
										;;
	esac
	bgcolor=`convert "$dir/tmpI.mpc" -format "%[pixel:u.p{$coords}]" info:`
	}


# function to pad image width
padImage()
	{
	if [ $ww -lt 500 ]; then
		# pad width to 500
		convert $dir/tmpB.mpc \
			-gravity center -background black \
			-extent 500x${hh} \
			$dir/tmpC.mpc
		xoffset=`convert xc: -format "%[fx:(500-$ww)/2]" info:`
		yoffset=0
	else
		convert $dir/tmpB.mpc $dir/tmpC.mpc
		xoffset=0
		yoffset=0		
	fi
	
	# get new image width
	nww=`convert $dir/tmpC.mpc -format "%w" info:`
	}
	

# function to make profile image for a given channel of a 1D grayscale image
generateProfile()
	{
	img=$1
	strokecolor="black"
	hit=201
	wid="$nww"

	# get line of image and convert into text format
	frac=`convert xc: -format "%[fx:($hit-1)/255]" info:`
	pixvals=`convert $img -strip -depth 8 -evaluate multiply $frac -compress none pgm:- | sed '1,3d'`

	# create point pair list
	plist=""
	x=0
	for y in $pixvals; do
		plist="$plist $x,$y"
		x=$((x+1))
	done
	numpairs=$((x-1))

	# draw graph of point pairs
	convert -size ${wid}x${hit} xc: \
		-stroke $strokecolor -fill white \
		-draw "polyline $plist" -flip $dir/tmpP.mpc
	}


# function to get X,Y coordiantes from radius and angle
getXY()
	{
	# computes X,Y coordinates from c coeffs extracted from -verbose info of -distort depolar 1
	index=$1
	max=$2
	i=$index
	j=`convert xc: -format "%[fx:($max*$hh)/$range]" info:`
	aa=`convert xc: -format "%[fx:($i+.5)*${cArr[6]} + ${cArr[5]}]" info:`
	rr=`convert xc: -format "%[fx:($j+.5)*${cArr[7]} + ${cArr[1]}]" info:`
	xx=`convert xc: -format "%[fx:$rr*sin($aa) + ${cArr[2]} - 0.5]" info:`
	yy=`convert xc: -format "%[fx:$rr*cos($aa) + ${cArr[3]}- 0.5]" info:`
	}


# get im_version
im_version=`convert -list configure | \
	sed '/^LIB_VERSION_NUMBER /!d; s//,/;  s/,/,0/g;  s/,0*\([0-9][0-9]\)/\1/g' | head -n 1`

# setup prerotate
if [ "$prerotate" = "autorotate" ]; then
	rproc="-auto-orient"
elif [ "$prerotate" = "90" ]; then
	rproc="-rotate 90"
elif [ "$prerotate" = "270" ]; then
	rproc="-rotate 270"
elif [ "$prerotate" = "270" ]; then
	rproc="-rotate 270"
else
	rproc=""
fi

# get infile name
inname=`convert -ping "$infile" -format "%t" info:`

# test input image
convert -quiet "$infile" +repage $rproc "$dir/tmpI.mpc" ||
	errMsg "--- FILE $infile DOES NOT EXIST OR IS NOT AN ORDINARY FILE, NOT READABLE OR HAS ZERO SIZE ---"

# get original image width and height
wo=`convert $dir/tmpI.mpc -format "%w" info:`
ho=`convert $dir/tmpI.mpc -format "%h" info:`


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Converting Image To Mask"
fi

# get background color
if [ "$bgcolor" = "" ]; then
	# get bgcolor from coords
	getColor $dir/tmpI.mpc
	color="$bgcolor"

elif [ "$bgcolor" != "" ]; then
	color="$bgcolor"
fi
#echo "color=$bgcolor"


# set up bordersize
if [ "$blur" != "0" ]; then
	bordersize=`convert xc: -format "%[fx:3*$blur+5]" info:`
else
	bordersize=5
fi

# trim image
convert $dir/tmpI.mpc \
	-bordercolor "$color" -border $bordersize \
	-fuzz ${fuzzval}% -trim +repage \
	$dir/tmpA.mpc

	
# get trimmed dimensions
www=`convert $dir/tmpA.mpc -format "%w" info:`
hhh=`convert $dir/tmpA.mpc -format "%h" info:`

# pad trimmed image
convert $dir/tmpA.mpc -bordercolor "$color" -border $bordersize $dir/tmpA.mpc


# get image dimensions and max radius for depolar
ww=`convert $dir/tmpA.mpc -format "%w" info:`
hh=`convert $dir/tmpA.mpc -format "%h" info:`
ww2=`convert xc: -format "%[fx:$ww/2]" info:`
hh2=`convert xc: -format "%[fx:$hh/2]" info:`
maxrad=`convert xc: -format "%[fx:0.5*hypot($ww,$hh)]" info:`
totpix=$((ww*hh))

# set up floodfill
if [ "$im_version" -ge "07000000" ]; then
	matte_alpha="alpha"
else
	matte_alpha="matte"
fi

# set up channel
if [ "$channel" = "gray" ]; then
	cproc="-colorspace gray"
elif [ "$channel" = "red" ]; then
	cproc="-channel red -separate +channel"
elif [ "$channel" = "green" ]; then
	cproc="-channel green -separate +channel"
elif [ "$channel" = "blue" ]; then
	cproc="-channel blue -separate +channel"
elif [ "$channel" = "cyan" ]; then
	cproc="-colorspace CMYK -channel cyan -separate +channel -negate"
elif [ "$channel" = "magenta" ]; then
	cproc="-colorspace CMYK -channel magenta -separate +channel -negate"
elif [ "$channel" = "yellow" ]; then
	cproc="-colorspace CMYK -channel yellow -separate +channel -negate"
elif [ "$channel" = "black" ]; then
	cproc="-colorspace CMYK -channel black -separate +channel -negate"
else
	cproc=""
fi

# setup for morhphology smoothing filter
if [ $filter -gt 0 ]; then
	morphfiltering="-morphology smooth octagon:$filter"
else
	morphfiltering=""
fi

# test if otsuthresh available
# redirect output from otsuthresh to &>/dev/null to avoid seeing the threshold value
convert -size 1x10 gradient: $dir/tmpG.mpc 
otsuthresh $dir/tmpG.mpc null: &>/dev/null
[ "$procedure" = "autothresh" -a $? -ne 0 ] && errMsg "--- OTSUTHRESH IS NOT AVAILABLE ---"

# create binary mask
if [ "$procedure" = "floodfill" ]; then
	convert $dir/tmpA.mpc -alpha off \
		-alpha set -fuzz ${fuzzval}% -fill none \
		-draw "$matte_alpha 0,0 floodfill" \
		-fill "rgba(255,255,255,1)" +opaque none \
		-fill "rgba(0,0,0,1)" -opaque none \
		-alpha off $morphfiltering $dir/tmpB.mpc
elif [ "$procedure" = "threshold" ]; then
	convert $dir/tmpA.mpc -alpha off \
		$cproc -threshold -fuzz ${fuzzval}% \
		$morphfiltering $dir/tmpB.mpc
elif [ "$procedure" = "autothresh" ]; then
	convert $dir/tmpA.mpc -alpha off $cproc $dir/tmp.miff
		otsuthresh $dir/tmp.miff $dir/tmpB.mpc
	if [ $filter -gt 0 ]; then
		convert $dir/tmpB.mpc -morphology smooth octagon:$filter $dir/tmpB.mpc
	fi
fi


# process CLL
if [ "$procedure" = "autothresh" -a "$im_version" -ge "06090208" ]; then
	cclarea=`convert xc: -format "%[fx:floor($totpix*$area/100)]" info:`
	convert $dir/tmpB.mpc \
		-define connected-components:area-threshold=$cclarea \
		-define connected-components:mean-color=true \
		-connected-components 4 \
		$dir/tmpB.mpc
else
	# process text blurring and threshold
	if [ "$blur" != "0" ]; then
		convert $dir/tmpB.mpc -blur 0x$blur -threshold 0 $dir/tmpB.mpc
	fi
fi



# save or view mask image
if [ "$kind" = "mask" -o "$kind" = "all" ]; then
	if [ "$images" = "save" ]; then
		convert $dir/tmpB.mpc ${inname}_unperspective_mask.png
	elif [ "$images" = "view" ]; then
		convert $dir/tmpB.mpc show:
		prompt
	fi
fi


# padded image tmpB as needed to get tmpC
padImage

if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Converting To Depolar Image"
fi

# convert to polar coords
convert $dir/tmpC.mpc -virtual-pixel black -distort depolar $maxrad $dir/tmpD.mpc


# get qval for use in radius accuracy
qval=`convert xc: -format "%q" info:`
if [ $qval -eq 8 ]; then
	range=255
else
	range=65535
fi


# scale depolar image down to one row
convert $dir/tmpD.mpc -scale ${nww}x1! -depth $qval $dir/tmpE.mpc


# save or view polar graph
if [ "$method" = "peak" ]; then
	if [ "$kind" = "polar" -o "$kind" = "all" ]; then
		generateProfile $dir/tmpE.mpc
		if [ "$images" = "save" ]; then
			convert $dir/tmpP.mpc ${inname}_unperspective_depolar_peaks.png
		elif [ "$images" = "view" ]; then
			convert $dir/tmpP.mpc show:
			prompt
		fi
	fi
fi


# set up blur and sharpening
if [ "$smooth" != "0" ]; then
	smoothing="-blur 0x$smooth"
else
	smoothing=""
fi
if [ "$sharpen" != "0" ]; then
	sharpening="-sharpen 0x$sharpen"
else
	sharpening=""
fi

# set up for biasing
if [ "$im_version" -ge "07000000" ]; then
	biasing1="-define convolve:bias=50%"
	biasing2="-define convolve:bias=0"
else
	biasing1="-bias 50%"
	biasing2="-bias 0"
fi

# append two copies and blur and/or sharpen (and convert to second derivative)
if [ "$method" = "peak" ]; then
	# note: cropping center section done later
	convert $dir/tmpE.mpc \( -clone 0 \) \( -clone 0 \) \
		+append $smoothing $sharpening $dir/tmpF.mpc
elif [ "$method" = "derivative" ]; then
	# note: cropping center section done here
	convert $dir/tmpE.mpc \( -clone 0 \) \( -clone 0 \) \
		+append $smoothing $sharpening \
		-virtual-pixel tile -define convolve:scale=50%\! $biasing1 -morphology Convolve '3x1: -1,0,1' -auto-level \
		-virtual-pixel tile -define convolve:scale=50%\! $biasing2 -morphology Convolve '3x1: 1,0,-1' -auto-level \
		-gravity center -crop ${nww}x1+0+0 +repage \
		$dir/tmpF.mpc
fi



# save or view polar derivative graph
if [ "$method" = "derivative" ]; then
	if [ "$kind" = "polar" -o "$kind" = "all" ]; then
		generateProfile $dir/tmpF.mpc
		if [ "$images" = "save" ]; then
			convert $dir/tmpP.mpc ${inname}_unperspective_depolar_derivative.png
		elif [ "$images" = "view" ]; then
			convert $dir/tmpP.mpc show:
			prompt
		fi
	fi
fi


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Finding Peaks And Valleys"
fi

# convert row image to array
if [ "$im_version" -le "06080609" -o "$im_version" -ge "06090201" ]; then
	htArr=(`convert $dir/tmpF.mpc txt:- | tail -n +2 | tr -cs "0-9\n" " " | cut -d\  -f 3`)
	num="${#htArr[*]}"
else
	# txt: format changed from raw values in qrange to percent at 6.8.6.10
	htArr=(`convert $dir/tmpF.mpc txt:- | tail -n +2 | tr -cs ".0-9\n" " " | cut -d\  -f 3`)
	num="${#htArr[*]}"
	for ((i=0; i<num; i++)); do
		htArr[$i]=`echo "scale=0; ${htArr[$i]}*$range/100" | bc`
	done
fi
if $debug; then
	echo "${htArr[*]}"
	echo "num=$num"
fi



# find peaks (and valleys)
# ref: http://stackoverflow.com/questions/3790445/1d-multiple-peak-detection
# note this finds the last index from a continuous series of peak value
# note using delta>0, seems to shift the index off the true peak
# probably better to use some kind of smoothing process, but it will lower the peak
# so will have to find indices from smoothed process and then go back to find the 
# actual peaks from the the original within limited group of indices about the peak index
delta=0
min=1000000
max=-1000000
maxlist=""
maxposlist=""
minlist=""
minposlist=""
lookformax=1
for ((i=0; i<num; i++)) do
	this=${htArr[$i]}
	if [ $this -ge $max ]; then
		max=$this
		maxpos=$i
	fi
	if [ $this -le $min ]; then
		min=$this
		minpos=$i
	fi

	if [ $lookformax = 1 ]; then
		if [ $this -lt $((max-delta)) ]; then
			# max is here
			j=$((i-1))
			maxlist="$maxlist ${htArr[$j]}"
			maxposlist="$maxposlist $j"
			min=$this
			minpos=$i
			lookformax=0
		fi
	else
		if [ $this -gt $((min+delta)) ]; then
			# min is here
			j=$((i-1))
			minlist="$minlist ${htArr[$j]}"
			minposlist="$minposlist $j"
			max=$this
			maxpos=$i
			lookformax=1
		fi
	fi
done
if $debug; then
	echo ""
	echo "maxlist=$maxlist"
	echo "maxposlist=$maxposlist"
	echo ""
	echo "minlist=$minlist"
	echo "minposlist=$minposlist"
fi
maxArr=($maxlist)
maxposArr=($maxposlist)
maxnum="${#maxposArr[*]}"
minArr=($minlist)
minposArr=($minposlist)
minnum="${#minposArr[*]}"
if $debug; then
	echo "maxnum=$maxnum; minnum=$minnum"
	echo ""
fi

if [ "$method" = "peak" ]; then
	# take middle section of arrays
	maxlist=""
	maxposlist=""
	for ((i=0; i<maxnum; i++)); do
		k=${maxposArr[i]}
		j=$((k-nww))
		test=`echo "scale=0; $k>=$nww && $k<(2*$nww)" | bc`
	#echo "i=$i; k=$k; j=$j; test=$test"
		if [ $test -eq 1 ]; then
			maxlist="$maxlist ${maxArr[i]}"
			maxposlist="$maxposlist $j"
		fi
	done
	maxArr=($maxlist)
	maxposArr=($maxposlist)
	maxnum="${#maxposArr[*]}"
	
	minlist=""
	minposlist=""
	for ((i=0; i<$minnum; i++)); do
		k=${minposArr[i]}
		j=$((k-nww))
		test=`echo "scale=0; $k>=$nww && $k<(2*$nww)" | bc`
	#echo "i=$i; k=$k; j=$j; test=$test"
		if [ $test -eq 1 ]; then
			minlist="$minlist ${minArr[i]}"
			minposlist="$minposlist $j"
		fi
	done
	minArr=($minlist)
	minposArr=($minposlist)
	minnum="${#minposArr[*]}"
	
	if $debug; then
		echo ""
		echo "maxlist=$maxlist"
		echo "maxposlist=$maxposlist"
		echo ""
		echo "minlist=$minlist"
		echo "minposlist=$minposlist"
	fi
fi


# test if too many peaks before removing false peaks
if [ "$traps" != "maxpeaks" -a "$traps" != "all" -a $maxnum -gt $maxpeaks ]; then
	echo ""
	echo "--- Number Of Peaks = $maxnum Is Larger Than $maxpeaks ---"
	echo ""
	exit 1
fi


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Number Of Peaks=$maxnum"
	echo ""
	echo "Removing False Peaks"
fi


# remove false peaks
if [ "$method" = "peak" -a "$thresh" != "0" ]; then
	# eliminate false peaks when a peak and valley are within thresh in index and also in ht
	maxlist=""
	maxposlist=""
	for ((i=0; i<maxnum; i++)); do
		for ((j=0; j<minnum; j++)); do
			test=`echo "scale=1; (${maxposArr[i]}-${minposArr[j]})^2<=($thresh)^2 && (${maxArr[i]}-${minArr[j]})^2<=($thresh*$range/255)^2" | bc`
			if [ $test -eq 1 ]; then
				unset maxposArr[i]
				unset maxArr[i]
				break
			fi
		done
		maxlist="$maxlist ${maxArr[i]}"
		maxposlist="$maxposlist ${maxposArr[i]}"
	done
	if $debug; then
		echo "maxlist=$maxlist"
		echo "maxposlist=$maxposlist"
	fi
	maxArr=($maxlist)
	maxposArr=($maxposlist)
	maxnum="${#maxposArr[*]}"
	minArr=($minlist)
	minposArr=($minposlist)
	minnum="${#minposArr[*]}"

elif [ "$method" = "derivative" -a "$thresh" != "0" ]; then
	# eliminate false peaks when a peak is less than thresh
	maxlist=""
	maxposlist=""
	thresh2=`convert xc: -format "%[fx:($thresh*$range/255)]" info:`
	for ((i=0; i<maxnum; i++)); do
			test=`echo "scale=1; ${maxArr[i]}<=$thresh2" | bc`
#echo "i=$i; max=${maxArr[i]}; thresh2=$thresh2; test=$test"
			if [ $test -eq 1 ]; then
				unset maxposArr[i]
				unset maxArr[i]
			fi
	done
	maxArr=(${maxArr[*]})
	maxposArr=(${maxposArr[*]})
	maxnum="${#maxposArr[*]}"
fi

if $debug; then
	echo "maxposArr=${maxposArr[*]}"
	echo "maxArr=${maxArr[*]}"
	echo "maxnum=$maxnum; minnum=$minnum"
	echo ""
fi


# reset maxArr to values from original 1D polar image hts
if [ "$sharpen" != "0" -o "$method" = "derivative" ]; then
	if [ "$im_version" -le "06080609" -o "$im_version" -ge "06090201" ]; then
		htArr=(`convert $dir/tmpE.mpc txt:- | tail -n +2 | tr -cs "0-9\n" " " | cut -d\  -f 3`)
		num="${#htArr[*]}"
	else
		# txt: format changed from raw values in qrange to percent at 6.8.6.10
		htArr=(`convert $dir/tmpE.mpc txt:- | tail -n +2 | tr -cs ".0-9\n" " " | cut -d\  -f 3`)
		num="${#htArr[*]}"
		for ((i=0; i<num; i++)); do
			htArr[$i]=`echo "scale=0; ${htArr[$i]}*$range/100" | bc`
		done
	fi
	if $debug; then
		echo "${htArr[*]}"
		echo "num=$num"
	fi
	# replace maxArr with values from unsharpened row image
	for ((i=0; i<maxnum; i++)); do
		j="${maxposArr[$i]}"
		maxArr[$i]=${htArr[$j]}
	done
fi
if $debug; then
	echo "maxArr=${maxArr[*]}"
fi

if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Depolar Image Peak X Coordinates: ${maxposArr[*]}"
	echo "Number of Peaks: $maxnum"
fi

# test if too few or too many peaks
if [ $maxnum -lt 4 ]; then
	echo ""
	echo "--- Number Of Peaks Is Less Than 4 ---"
	echo ""
	exit 1
elif [ $maxnum -gt 4 ]; then
	echo ""
	echo "--- Number Of Peaks Is Greater Than 4 ---"
	echo ""
	exit 1
fi


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Converting Corners To Image Coordinates"
fi

# get c coefs array from verbose -distort depolar 
# note use -define viewport with 1 pixel to avoid processing the whole image
# note - added >/dev/null to fix script to run under Cygwin per feedback from Dirk Lemstra
data=`convert $dir/tmpC.mpc -verbose \
	-define distort:viewport=1x1+0+0 \
	-distort depolar $maxrad null: 2>&1 >/dev/null`
data=`echo "$data" | tail -n +2 | head -n 8 | tr -cs "\-\+.0-9\012" " " | sed -n 's/^ *\(.*\)$/\1/p'`
clist=`echo "$data" | cut -d\  -f 2`
cArr=($clist)
if $debug; then
	echo ""
	echo "data=$data"
	echo "cArr=${cArr[*]}"
fi

# Get 4 X,Y input coords using c coeffs array
# points extracted are ccw from top left
ii=0
getXY "${maxposArr[$ii]}" "${maxArr[$ii]}"
xx=`convert xc: -format "%[fx:$xx-$xoffset]" info:`
yy=`convert xc: -format "%[fx:$yy-$yoffset]" info:`
ix0=$xx
iy0=$yy
ipt0="$xx,$yy"
ii=$((ii+1))

getXY "${maxposArr[$ii]}" "${maxArr[$ii]}"
xx=`convert xc: -format "%[fx:$xx-$xoffset]" info:`
yy=`convert xc: -format "%[fx:$yy-$yoffset]" info:`
ix1=$xx
iy1=$yy
ipt1="$xx,$yy"
ii=$((ii+1))

getXY "${maxposArr[$ii]}" "${maxArr[$ii]}"
xx=`convert xc: -format "%[fx:$xx-$xoffset]" info:`
yy=`convert xc: -format "%[fx:$yy-$yoffset]" info:`
ix2=$xx
iy2=$yy
ipt2="$xx,$yy"
ii=$((ii+1))

getXY "${maxposArr[$ii]}" "${maxArr[$ii]}"
xx=`convert xc: -format "%[fx:$xx-$xoffset]" info:`
yy=`convert xc: -format "%[fx:$yy-$yoffset]" info:`
ix3=$xx
iy3=$yy
ipt3="$xx,$yy"
ii=$((ii+1))

if $debug; then
	echo "ipt0=$ipt0; ipt1=$ipt1; ipt2=$ipt2; ipt3=$ipt3"
fi

if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Corner Points Are: $ipt0 $ipt1 $ipt2 $ipt3"
fi


# save or view edge image
if [ "$kind" = "polar" -o "$kind" = "all" ]; then
	if [ "$images" = "save" ]; then
		convert -size ${ww}x${hh} xc:black -strokewidth 2 \
			-stroke red -draw "line $ipt0 $ipt1" \
			-stroke lime -draw "line $ipt1 $ipt2" \
			-stroke blue -draw "line $ipt2 $ipt3" \
			-stroke yellow -draw "line $ipt3 $ipt0" \
			${inname}_unperspective_edge.png
	elif [ "$images" = "view" ]; then
		convert -size ${ww}x${hh} xc:black -strokewidth 2 \
			-stroke red -draw "line $ipt0 $ipt1" \
			-stroke lime -draw "line $ipt1 $ipt2" \
			-stroke blue -draw "line $ipt2 $ipt3" \
			-stroke yellow -draw "line $ipt3 $ipt0" \
			show:
		prompt
	fi
fi

# get four side lengths
left=`convert xc: -format "%[fx:hypot($ix0-$ix1,$iy0-$iy1)]" info:`
bottom=`convert xc: -format "%[fx:hypot($ix1-$ix2,$iy1-$iy2)]" info:`
right=`convert xc: -format "%[fx:hypot($ix2-$ix3,$iy2-$iy3)]" info:`
top=`convert xc: -format "%[fx:hypot($ix3-$ix0,$iy3-$iy0)]" info:`
if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Edge Lengths Are: left=$left; bottom=$bottom; right=$right; top=$top"
fi


# test for edge length too small
if [ "$traps" != "minlength" -a "$traps" != "all"  ]; then
	ltest=`convert xc: -format "%[fx:($left<$minlength || $bottom<$minlength || $right<$minlength || $top<$minlength)?1:0]" info:`
	if [ $ltest -eq 1 ]; then
		echo ""
		echo "Edge Length Is Too Small"
		echo ""
		exit 1
	fi
fi


# get the aspect ratio if not provided
# references: 
# http://www.sagenb.org/home/pub/704/
# http://research.microsoft.com/users/zhang/Papers/WhiteboardRectification.pdf
# http://research.microsoft.com/en-us/um/people/zhang/papers/tr03-39.pdf

cx=`convert xc: -format "%[fx:$ww/2]" info:`
cy=`convert xc: -format "%[fx:$hh/2]" info:`
m1x=`echo $ipt1 | cut -d, -f1`
m1y=`echo $ipt1 | cut -d, -f2`
m2x=`echo $ipt2 | cut -d, -f1`
m2y=`echo $ipt2 | cut -d, -f2`
m3x=`echo $ipt0 | cut -d, -f1`
m3y=`echo $ipt0 | cut -d, -f2`
m4x=`echo $ipt3 | cut -d, -f1`
m4y=`echo $ipt3 | cut -d, -f2`
if $debug; then
	echo "cx=$cx; cy=$cy"
	echo "m1x=$m1x; m1y=$m1y"
	echo "m2x=$m2x; m2y=$m2y"
	echo "m3x=$m3x; m3y=$m3y"
	echo "m4x=$m4x; m4y=$m4y"
	echo ""
fi
# convert to proper x,y coordinates relative to center
m1x=`convert xc: -format "%[fx:$m1x-$cx]" info:`
m1y=`convert xc: -format "%[fx:$cy-$m1y]" info:`
m2x=`convert xc: -format "%[fx:$m2x-$cx]" info:`
m2y=`convert xc: -format "%[fx:$cy-$m2y]" info:`
m3x=`convert xc: -format "%[fx:$m3x-$cx]" info:`
m3y=`convert xc: -format "%[fx:$cy-$m3y]" info:`
m4x=`convert xc: -format "%[fx:$m4x-$cx]" info:`
m4y=`convert xc: -format "%[fx:$cy-$m4y]" info:`
if $debug; then
	echo "m1x=$m1x; m1y=$m1y"
	echo "m2x=$m2x; m2y=$m2y"
	echo "m3x=$m3x; m3y=$m3y"
	echo "m4x=$m4x; m4y=$m4y"
	echo ""
fi
	
if [ "$aspect" = "" ]; then
	#simplified equations, assuming u0=0, v0=0, s=1
	k2=`echo "scale=5; (($m1y - $m4y)*$m3x - ($m1x - $m4x)*$m3y + $m1x*$m4y - $m1y*$m4x)/(($m2y- $m4y)*$m3x - ($m2x - $m4x)*$m3y + $m2x*$m4y - $m2y*$m4x)" | bc`
	k3=`echo "scale=5; (($m1y - $m4y)*$m2x - ($m1x - $m4x)*$m2y + $m1x*$m4y - $m1y*$m4x)/(($m3y- $m4y)*$m2x - ($m3x - $m4x)*$m2y + $m3x*$m4y - $m3y*$m4x)" | bc`
	ff=`echo "scale=5; (($k3*$m3y - $m1y)*($k2*$m2y - $m1y) + ($k3*$m3x - $m1x)*($k2*$m2x- $m1x))/(($k3 - 1)*($k2 - 1))" | bc`
	if $debug; then
		echo "k2=$k2; k3=$k3"
	fi
	if [ "$ff" = "" ]; then
		errMsg "--- ASPECT RATIO CANNOT BE DETERMINED ---"
	else
		# sqrt( $ff*$ff) = abs($ff)
		f=`echo "scale=5; sqrt( sqrt( $ff*$ff) )" | bc`
		aspect=`echo "scale=5; sqrt((($k2 - 1)^2 + ($k2*$m2y - $m1y)^2/$f^2 + ($k2*$m2x - $m1x)^2/$f^2)/(($k3 - 1)^2 + ($k3*$m3y - $m1y)^2/$f^2 + ($k3*$m3x - $m1x)^2/$f^2))" | bc`
	fi
	if $debug; then
		echo "f=$f; aspect=$aspect"
	fi
fi

# test for aspect ratio too large
if [ "$traps" != "maxaspect" -a "$traps" != "all"  ]; then
	atest=`convert xc: -format "%[fx:($aspect>$maxaspect || $aspect<(1/$maxaspect))?1:0]" info:`
	if [ $atest -eq 1 ]; then
		echo ""
		echo "Aspect Ratio = $aspect Is Larger Than $maxaspect"
		echo ""
		exit 1
	fi
fi


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Aspect Ratio = $aspect"
fi


# compute output size from aspect ratio, and width or height
if [ "$height" != "" -a "$width" != "" ]; then
	echo "--- BOTH WIDTH AND HEIGHT CANNOT BE SPECIFIED AT THE SAME TIME ---"
elif [ "$height" != "" ]; then
	oh=$height
	ow=`convert xc: -format "%[fx:floor($aspect*$oh)]" info:`
elif [ "$width" != "" ]; then
	ow=$width
	oh=`convert xc: -format "%[fx:floor($ow/$aspect)]" info:`
elif [ "$default" = "el" ]; then
	oh=`convert xc: -format "%[fx:$left]" info:`
	ow=`convert xc: -format "%[fx:floor($aspect*$oh)]" info:`
elif [ "$default" = "bh" ]; then
	oh=$hhh
	ow=`convert xc: -format "%[fx:floor($aspect*$oh)]" info:`
elif [ "$default" = "bw" ]; then
	ow=$www
	oh=`convert xc: -format "%[fx:floor($ow/$aspect)]" info:`
elif [ "$default" = "h" ]; then
	oh=$ho
	ow=`convert xc: -format "%[fx:floor($aspect*$oh)]" info:`
elif [ "$default" = "w" ]; then
	ow=$wo
	oh=`convert xc: -format "%[fx:floor($ow/$aspect)]" info:`
fi

if $debug; then
	echo ""
	echo "left=$left; oh=$oh; ow=$ow"
fi


# Get 4 output coords
opt0="0,0"
opt1="0,$oh"
opt2="$ow,$oh"
opt3="$ow,0"

if $debug; then
	echo ""
	echo "opt0=$opt0; opt1=$opt1; opt2=$opt2; opt3=$opt3"
fi

if [ "$rotate" != "0" ]; then
	rotation="-rotate $rotate"
else
	quad4=`convert xc: -format "%[fx:($m3x<=0 && $m3y>=0)?1:0]" info:`
	quad3=`convert xc: -format "%[fx:($m3x<0 && $m3y<0)?1:0]" info:`
	quad2=`convert xc: -format "%[fx:($m3x>0 && $m3y<0)?1:0]" info:`
	quad1=`convert xc: -format "%[fx:($m3x>0 && $m3y>0)?1:0]" info:`
	if [ $quad4 -eq 1 ]; then
		rotation=""
	elif [ $quad3 -eq 1 ]; then
		rotation="-rotate 270"
	elif [ $quad2 -eq 1 ]; then
		rotation="-rotate 180"
	elif [ $quad1 -eq 1 ]; then
		rotation="-rotate 90"
	else
		rotation=""
	fi			
fi


# get output/intermediate size and viewport
# note use -define viewport with 1 pixel to avoid processing the whole image
# compute the intermediate size from forward projection of input image dimensions
# using coefs from control points
# compute the output viewport from forward projection of input control pts
# using coefs from control points
# 
clist=`convert $dir/tmpA.mpc -verbose \
	-define distort:viewport=1x1+0+0 +distort perspective \
	"$ipt0 $opt0  $ipt1 $opt1  $ipt2 $opt2  $ipt3 $opt3" null: 2>&1 \
	| sed -n '3,4 p' | tr "'" "\000" | tr "," " "`
cArr=($clist)
c1=${cArr[0]}
c2=${cArr[1]}
c3=${cArr[2]}
c4=${cArr[3]}
c5=${cArr[4]}
c6=${cArr[5]}
c7=${cArr[6]}
c8=${cArr[7]}
if [ "$viewport" = "off" ]; then
	x=0
	y=0
else
	x=$ix0
	y=$iy0
fi
i1=`convert xc: -format "%[fx:floor(($c1*$x+$c2*$y+$c3)/($c7*$x+$c8*$y+1))]" info:`
j1=`convert xc: -format "%[fx:floor(($c4*$x+$c5*$y+$c6)/($c7*$x+$c8*$y+1))]" info:`
if [ "$viewport" = "off" ]; then
	x=0
	y=$hh
else
	x=$ix1
	y=$iy1
fi
i2=`convert xc: -format "%[fx:floor(($c1*$x+$c2*$y+$c3)/($c7*$x+$c8*$y+1))]" info:`
j2=`convert xc: -format "%[fx:floor(($c4*$x+$c5*$y+$c6)/($c7*$x+$c8*$y+1))]" info:`
if [ "$viewport" = "off" ]; then
	x=$ww
	y=$hh
else
	x=$ix2
	y=$iy2
fi
i3=`convert xc: -format "%[fx:floor(($c1*$x+$c2*$y+$c3)/($c7*$x+$c8*$y+1))]" info:`
j3=`convert xc: -format "%[fx:floor(($c4*$x+$c5*$y+$c6)/($c7*$x+$c8*$y+1))]" info:`
if [ "$viewport" = "off" ]; then
	x=$ww
	y=0
else
	x=$ix3
	y=$iy3
fi
i4=`convert xc: -format "%[fx:floor(($c1*$x+$c2*$y+$c3)/($c7*$x+$c8*$y+1))]" info:`
j4=`convert xc: -format "%[fx:floor(($c4*$x+$c5*$y+$c6)/($c7*$x+$c8*$y+1))]" info:`
xmax=`convert xc: -format "%[fx:max(max(max($i1,$i2),$i3),$i4)]" info:`
ymax=`convert xc: -format "%[fx:max(max(max($j1,$j2),$j3),$j4)]" info:`
xmin=`convert xc: -format "%[fx:min(min(min($i1,$i2),$i3),$i4)]" info:`
ymin=`convert xc: -format "%[fx:min(min(min($j1,$j2),$j3),$j4)]" info:`
iw=`convert xc: -format "%[fx:abs($xmax-$xmin)]" info:`
ih=`convert xc: -format "%[fx:abs($ymax-$ymin)]" info:`
if [ "$viewport" = "on" ]; then
	test=`convert xc: -format "%[fx:sign($xmin)]" info:`
	if [ $test -eq 1 ]; then
	xoff="+$xmin"
	else
	xoff="$xmin"
	fi
	test=`convert xc: -format "%[fx:sign($ymin)]" info:`
	if [ $test -eq 1 ]; then
	yoff="+$ymin"
	else
	yoff="$ymin"
	fi
	viewporting="-define distort:viewport=${iw}x${ih}${xoff}${yoff}"
else
	viewporting=""
fi

# test for input to intermediate (output) dimensions ratio too large
wratio=`convert xc: -format "%[fx:$iw/$ww]" info:`
hratio=`convert xc: -format "%[fx:$ih/$hh]" info:`
if [ "$traps" != "maxratio" -a "$traps" != "all"  ]; then
	if [ "$monitor" = "yes" ]; then
		echo ""
		echo "Input And Intermediate Dimensions: input=$ww x $hh; intermediate=$iw x $ih"
		echo "Intermediate/Input Ratios: width ratio=$wratio; height ratio=$hratio"
	fi
	# test for dimension ratio too large
	wtest=`convert xc: -format "%[fx:$wratio>$maxratio)?1:0]" info:`
	if [ $wtest -eq 1 ]; then
		echo ""
		echo "Intermediate/Input Width Ratio = $wratio Is Larger Than $maxratio"
		echo ""
		exit 1
	fi
	htest=`convert xc: -format "%[fx:$hratio>$maxratio)?1:0]" info:`
	if [ $htest -eq 1 ]; then
		echo ""
		echo "Intermediate/Input Height Ratio = $hratio Is Larger Than $maxratio"
		echo ""
		exit 1
	fi
fi


if [ "$monitor" = "yes" ]; then
	echo ""
	echo "Dewarping Image"
	echo ""
fi

# process image
convert $dir/tmpA.mpc  \
	-virtual-pixel background -background $color \
	$viewporting +distort perspective \
	"$ipt0 $opt0  $ipt1 $opt1  $ipt2 $opt2  $ipt3 $opt3" \
	-bordercolor $color -border 2 \
	-fuzz ${fuzzval}% -trim +repage \
	$rotation \
	"$outfile"



exit 0

