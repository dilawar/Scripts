#!/bin/sh
#
# by lyon8 (lyon8@gmx.net)
# modified by dilawar@ee.iitb.ac.in
# show your laptop battery state in dzen
 
BG='#444'  # dzen backgrounad
FG='#fff'  # dzen foreground
W=150     # width of the dzen bar
GW=50      # width of the gauge
GFG='#999'  # color of the gauge
GH=6       # height of the gauge
GBG='#3f6'  # color of gauge background
X=1060      # x position
Y=0    # y position
FN='fixed' # font
 
STATEFILE='/proc/acpi/battery/BAT0/state' # battery's state file
INFOFILE='/proc/acpi/battery/BAT0/info'   # battery's info file
 
LOWBAT=25        # percentage of battery life marked as low
LOWCOL='#ff4747' # color when battery is low
TIME_INT=1         # time intervall in seconds
 
PREBAR='/usr/share/dzen2/bitmaps/battery.xbm' # caption (also icons are possible)
 
while true; do
# look up battery's data
BAT_FULL=`cat $INFOFILE|grep design|line|cut -d " " -f 11`;
STATUS=`cat $STATEFILE|grep charging|cut -d " " -f 12`;
RCAP=`cat $STATEFILE|grep remaining|cut -d " " -f 8`;
 
# calculate remaining power
RPERCT=`expr $RCAP \* 100`;
RPERC=`expr $RPERCT / $BAT_FULL`;
 
# draw the bar and pipe everything into dzen
if [ $RPERC -le $LOWBAT ]; then GFG=$LOWCOL; fi
$echo  $PREBAR
eval echo $RPERC | dzen2-gdbar -h $GH -w $GW -fg $GFG -bg $GBG
sleep $TIME_INT;
done | dzen2 -ta c -tw $W -y $Y -x $X -fg $FG -bg $BG -fn $FN
