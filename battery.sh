#!/bin/bash

cd ~/Scripts
notification=$(grep 'notification:' notification|awk '{print $2}')

cd /proc/acpi/ac_adapter/C240;
power=$(grep 'state:' state|awk '{print $2}')
s1="$power"

s2="charged"
s3="on-line"
s4="on"

export DISPLAY=:0

if [ "$s1" = "on-line" ]; then
  cd /proc/acpi/battery/C241;
  state=$(grep 'charging state:' state|awk '{print $3}')
  if [ $state = $s2 ] && [ "$notification" = "$s4" ];
    then
            notify-send  --urgency=critical "Power Manager" "battery is full" -i battery_full
            echo "notification: off" >~/.scripts/notification

    fi

else
  if [ $notification != "on" ]; then
    echo "notification: on" >~/Scripts/notification
  fi
fi
