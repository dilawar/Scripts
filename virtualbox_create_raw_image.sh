#!/bin/bash -x
# First argument is the name of device
if [ ! $# -eq 2 ]; then
    echo "USAGE: $0 /dev/sdxY imageName.vmdk"
    exit
fi
devName=$1
fileName=$2
if [ ! -f $fileName ]; then
    rm -f $fileName 
fi

if [[ $devName =~ [a-z/]*[[:digit:]] ]]; then
    echo "Dealing with a single partition ... "
    partitionID=$(echo $devName | sed 's/[^0-9]//g')
    dName=${devName%%$partitionID}
    VBoxManage internalcommands createrawvmdk -filename $fileName \
        -rawdisk $dName -partitions $partitionID -relative 
else
    VBoxManage internalcommands createrawvmdk -filename $fileName \
        -rawdisk $devName 
fi
echo ".. done!"
