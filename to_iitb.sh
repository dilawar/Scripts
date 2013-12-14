#!/bin/bash

# Create a terminal and create a tunnel
port="5050"
host="127.0.0.1"
# On some platform nc-traditional is installed as default netcat. We are using
# netcat-openbsd.
netcat="nc.openbsd"
if [ ! $(which $netcat) ]; then
    echo "ERROR: Not nc command found. I was checking for $netcat"
    exit
fi
p_status=`$netcat -z $host $port; echo $?`
echo "Status, $p_status"
if [ $p_status == "1" ]; then
    sshpass -pextvoxac ssh -t -t -D 5050 secure@login.iitb.ac.in -p 5022 > /dev/null &
fi

while [ $p_status == "0" ]; do
    echo "Waiting for port to open...."
    p_status=`$netcat -z -w2 $host $port; echo $?`
done
echo "Port open. Making connection..."
ssh -o ProxyCommand='$netcat -X 5 -x $host:$port %h %p' sharada.ee.iitb.ac.in -ldilawar
