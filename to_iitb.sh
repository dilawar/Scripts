#!/bin/bash
# This script creates a connection to iitb network from outside. Procedure is
# suggested on this thread.
# https://groups.google.com/forum/#!topic/wncc_iitb/5hLDnR38iy8
#
# Local host port and tunnel.
port="5050"
host="127.0.0.1"

# On some platform nc-traditional is installed as default netcat. We are using
# netcat-openbsd.
netcat="nc"
if [ ! $(which $netcat) ]; then
    echo "ERROR: Not nc command found. I was checking for $netcat"
    echo "Replace this variable with netcat-openbsd"
    exit
fi

if [[ ! `which sshpass` =~ "sshpass" ]]; then
    echo "No program sshpass found. Please intall it first"
    exit
fi

p_status=`$netcat -z $host $port; echo $?`

# If port is not open, create a tunnel.
if [[ "$p_status" == "1" ]]; then
    sshpass -pextvoxac ssh -o UserKnownHostsFile=/dev/null \
        -o StrictHostKeyChecking=no \
        -t -t -D 5050 secure@login.iitb.ac.in -p 5022 &
fi

printf "Waiting for port to open "
while [[ "$p_status" == "1" ]]; do
    printf "."
    p_status=`$netcat -z -w2 $host $port; echo $?`
done
printf ".. port is open!\n"
echo "[INFO] Making connection"
ssh -o ProxyCommand="$netcat -X 5 -x $host:$port %h %p" sharada.ee.iitb.ac.in -ldilawar
