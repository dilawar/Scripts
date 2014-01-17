#!/bin/bash
xfconf-query -c xfce4-session -p /sessions/Failsafe/Client0_Command -t string -s "killall xfce4 && awesome" -a
