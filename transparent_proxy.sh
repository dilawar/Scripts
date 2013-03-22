#!/bin/bash
if [ -f /sbin/iptables ];
then 
  IPTABLES="/sbin/iptables"
elif [ -f /usr/sbin/iptables ];
then 
  IPTABLES="/usr/sbin/iptables"
else
  echo -e "\nERROR : Install iptables. \n"
  exit
fi

REDSOCKS_DIR="/usr/local/sbin"
REDSOCKS="$REDSOCKS_DIR/redsocks"

REDSOCKS_PORT="5124"
SOCKS_HOST="10.201.13.50"
SOCKS_PORT="80"

# start redsocks
if [ "$USER" != "root" ]; then 
  echo -n 'Restarting redsocks...'
  pkill -U $USER redsocks 2>/dev/null 
  sleep 1
  cd $REDSOCKS_DIR && $REDSOCKS -c $HOME/.redsocks.conf
  if [ $? -eq 0 ]; then 
    echo Done 
  else 
    echo Error 
  fi 
  exit 0;


elif [ "$1" != 'iptables' ]; then 
  exit 0
fi 

 
$IPTABLES -t nat -D PREROUTING -p tcp -j REDSOCKS_FILTER 2>/dev/null
$IPTABLES -t nat -D OUTPUT     -p tcp -j REDSOCKS_FILTER 2>/dev/null
$IPTABLES -t nat -F REDSOCKS_FILTER 2>/dev/null
$IPTABLES -t nat -X REDSOCKS_FILTER 2>/dev/null
$IPTABLES -t nat -F REDSOCKS 2>/dev/null
$IPTABLES -t nat -X REDSOCKS 2>/dev/null
 
# Create our own chain
$IPTABLES -t nat -N REDSOCKS
$IPTABLES -t nat -N REDSOCKS_FILTER
 
# Do not try to redirect local traffic
$IPTABLES -t nat -I REDSOCKS_FILTER -o lo -j RETURN
 
### Below whitelist and blacklist cannot operate together.
### If you want to change it, refactor the code. It's simple.
 
# Redirect only specified addresses and do not try redirect other traffic. (whitelist option)
$IPTABLES -t nat -A REDSOCKS_FILTER -m iprange --dst-range 192.168.0.10-192.168.0.30 -j REDSOCKS
$IPTABLES -t nat -A REDSOCKS_FILTER -d 126.0.0.0/8 -j REDSOCKS
$IPTABLES -t nat -A REDSOCKS_FILTER -j RETURN
 
## Do not redirect LAN traffic and some other reserved addresses. (blacklist option)
$IPTABLES -t nat -A REDSOCKS_FILTER -d 0.0.0.0/8 -j RETURN
$IPTABLES -t nat -A REDSOCKS_FILTER -d 10.0.0.0/8 -j RETURN
$IPTABLES -t nat -A REDSOCKS_FILTER -d 127.0.0.0/8 -j RETURN
#$IPTABLES -t nat -A REDSOCKS_FILTER -d 169.254.0.0/16 -j RETURN
#$IPTABLES -t nat -A REDSOCKS_FILTER -d 172.16.0.0/12 -j RETURN
$IPTABLES -t nat -A REDSOCKS_FILTER -d 192.168.0.0/16 -j RETURN
#$IPTABLES -t nat -A REDSOCKS_FILTER -d 224.0.0.0/4 -j RETURN
#$IPTABLES -t nat -A REDSOCKS_FILTER -d 240.0.0.0/4 -j RETURN
#$IPTABLES -t nat -A REDSOCKS_FILTER -j REDSOCKS
 
## Do not redirect traffic for the SOCKS-Server
## Not needed if server is not on a whitelist or is already blacklisted.
#$IPTABLES -t nat -I REDSOCKS -p tcp -d $SOCKS_HOST --dport $SOCKS_PORT -j RETURN
 
# Redirect all traffic that gets to the end of our chain
#$IPTABLES -t nat -A REDSOCKS   -p tcp -j REDIRECT --to-port $REDSOCKS_PORT
 
## Filter all traffic from the own host
## BE CAREFULL HERE IF THE SOCKS-SERVER RUNS ON THIS MACHINE
#$IPTABLES -t nat -A OUTPUT     -p tcp -j REDSOCKS
 
# Filter all traffic that is routed over this host
#$IPTABLES -t nat -A PREROUTING -p tcp -j REDSOCKS_FILTER
 
echo "\mIPtables reconfigured."
