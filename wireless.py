#!/usr/bin/env python

## Guide

import os
import sys
import subprocess
import re

def print_con(con):
    msg = ""
    essid, f, q, enc = con
    msg += "ESSID:{0}, Freq:{1} Ghz, Quality:{2}, Encryption:{3}".format(
        essid, f, q, enc)
    return msg

def connect(interface, con):
    essid, f, q, enc = con
    if enc:
        key = raw_input("This connection demands a key: ")
    if key:
        cmd = ["sudo", "/sbin/iwconfig", interface, essid, "key", key]
    else:
        cmd = ["sudo", "/sbin/iwconfig", interface, essid]
    subprocess.check_call(cmd)

print("+ Checking for network cards ...")
lspciOutput = subprocess.check_output("lspci | grep Network", shell=True)
if lspciOutput :
    print("+ A card found ")
    print("\t {0}".format(lspciOutput))
else:
    print("No network card found..")
    sys.exit()

print("+ Checking if there is a wireless interface")
res = subprocess.check_output("iwconfig ", shell=True
                              #, stderr=subprocess.STDOUT
                              )
wpoints = res.split("\n")
wirelessPoints = list()
for wp in wpoints:
    m = re.match(r"^(?P<name>\w+).*", wp)
    if m:
        wirelessPoints.append(m.group('name'))
    else :
        pass
if len(wirelessPoints) > 1:
    print("+ I am confused. More than one wireless interface is found")
    print("+ Which one I should use? ")
    i = 0
    for p in wirelessPoints:
        i += 1
        print("{0} : {1}".format(i, p))
    choice = raw_input("Your choice :")
    interface = wirelessPoints[int(choice)-1]
else:
    interface = wirelessPoints.pop()

print("+ Searching for available connections for interface: "+interface)
res = subprocess.Popen(['sudo', '/sbin/iwlist',  interface, 'scanning']
                       , stdin = subprocess.PIPE
                       , stderr = subprocess.PIPE
                       , stdout = subprocess.PIPE
                       )
res = res.stdout.read()
cells = res.split("Cell")
con = list()
for cell in cells:
    encryption = False
    for line in cell.split("\n"):
        line = line.strip()
        if "Address" in line:
            address = line.split("Address:").pop()
        elif "Frequency" in line:
            m = re.search("Frequency:([\d\.]+)", line)
            freq = m.group(1)
        elif "Quality" in line:
            m = re.search("Quality=(\d+)\/", line)
            quality =  m.group(1)
        elif "Encryption key:on" in line:
            encryption = True
        elif "ESSID" in line:
            essid = line.split(":").pop()
            con.append((essid, freq, quality, encryption))
        else:pass

i = 0
for c in con:
    print("{0} : {1}".format(i, print_con(c)))
    i += 1
res = raw_input("Which connection: ")
connection = con[int(res)]
print("Connecting to {0}".format(connection))
connect(interface, connection)

#echo $lspciOutput
#if [ ! "$lspciOutput" ] ; then
#  echo "[Info] No wireless hardware found."
#  exit
#else
#  echo "|- A wireless hardware is found."
#fi
#
#echo "+ Scanning..."
#nm-tool | grep "Freq.*Strength" | sed -ne "s|\(.*Strength \([0-9]\+\).*\)|\2}\1|p" | sort -n -r
