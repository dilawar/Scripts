#!/usr/bin/env bash
# Add this to crontab 
# 0,15,30,45 * * * * ~/Scripts/remind.sh

# This is from here  https://unix.stackexchange.com/a/111190/5362
DBUSFILE=$HOME/.dbus/Xdbus
touch $DBUSFILE
chmod 600 $DBUSFILE
env | grep DBUS_SESSION_BUS_ADDRESS > $DBUSFILE
echo 'export DBUS_SESSION_BUS_ADDRESS' >> $DBUSFILE
# Now source this file.
. $DBUSFILE
export DISPLAY=:0
export XAUTHORITY=$HOME/.Xauthority
notify-send "Anything in next 15 mins: "
gcalcli remind 30
