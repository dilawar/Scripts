#!/bin/bash
# First argument is the name of device
if [ $# -neq 2 ]; then
    echo "USAGE: $0 /dev/sdxY imageName.vmdk"
    exit
fi
devName=$1
fileName=$2
VBoxManage internalcommands createrawvmdk -filename $fileName \
    -rawdisk $devName -register
