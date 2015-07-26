#!/bin/bash
echo "Information about this cpu"
echo "+ No of cpu cores: `grep --count processor /proc/cpuinfo`"
echo "+ Memory: `sudo dmidecode --type memory`"
