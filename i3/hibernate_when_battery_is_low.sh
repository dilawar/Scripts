#!/bin/sh
set -x

# http://unix.stackexchange.com/questions/84437/how-do-i-make-my-laptop-sleep-when-it-reaches-some-low-battery-threshold

###########################################################################
#
# Usage: system-low-battery
#
# Checks if the battery level is low. If “low_threshold” is exceeded
# a system notification is displayed, if “critical_threshold” is exceeded
# a popup window is displayed as well. If “OK” is pressed, the system
# shuts down after “timeout” seconds. If “Cancel” is pressed the script
# does nothing.
#
# This script is supposed to be called from a cron job.
# 
###########################################################################

# This is required because the script is invoked by cron. Dbus information
# is stored in a file by the following script when a user logs in. Connect
# it to your autostart mechanism of choice.
#
# #!/bin/sh
# touch $HOME/.dbus/Xdbus
# chmod 600 $HOME/.dbus/Xdbus
# env | grep DBUS_SESSION_BUS_ADDRESS > $HOME/.dbus/Xdbus
# echo 'export DBUS_SESSION_BUS_ADDRESS' >> $HOME/.dbus/Xdbus
# exit 0
#

# User should be able to run pm-suspend command without password. Add the
# following to sudoer file using visudo command.
# dilawar ALL=(ALL) NOPASSWD: ALL

# let crontab know whom to talk to
# Using it with i3.

export DISPLAY=:0

if [ -r ~/.dbus/Xdbus ]; then
  . ~/.dbus/Xdbus
fi

low_threshold=20
critical_threshold=15
timeout=15
suspend_cmd='/usr/sbin/pm-suspend'

level=$(cat /sys/class/power_supply/BAT0/capacity)
state=$(cat /sys/class/power_supply/BAT0/status)

if [ x"$state" != x'Discharging' ]; then
  exit 0
fi

do_shutdown() {
  sleep $timeout && kill $zenity_pid 2>/dev/null

  if [ x"$state" != x'Discharging' ]; then
    exit 0
  else
    sudo $suspend_cmd
  fi
}

if [ "$level" -gt $critical_threshold ] && [ "$level" -lt $low_threshold ]; then
  i3-nagbar -m "Battery level is low: $level%" -b "Out of power" &
fi


if [ "$level" -lt $critical_threshold ]; then

  i3-nagbar -m "Battery level is low: $level%. The system is going to hibernate in 15 seconds." -b "Out of power" &
  do_shutdown &
  shutdown_pid=$!

  trap 'kill $shutdown_pid' 1

  if ! wait $zenity_pid; then
    kill $shutdown_pid 2>/dev/null
  fi
fi
exit 0
