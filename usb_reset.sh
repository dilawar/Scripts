#!/usr/bin/env bash

if [ $# -lt 1 ]; then
    echo "USAGE: $0 VENDOR_ID"

    echo "I found following"
    for X in /sys/bus/usb/devices/*
    do
        if [ -e "$X/idVendor" ] && [ -e "$X/idProduct" ]; then
            VENDOR=$(cat "$X/idVendor" )
            PRODUCT=$(cat "$X/idProduct" )
            echo "Vendor $VENDOR, Product $PRODUCT"
        fi
    done
    exit
fi

for X in /sys/bus/usb/devices/*
do
    if [ -e "$X/idVendor" ] && [ -e "$X/idProduct" ]
    then
        # Only reset the given vendor
        VENDOR=$(cat "$X/idVendor" )
        PRODUCT=$(cat "$X/idProduct" )
        if [ $VENDOR == $1 ]; then
            echo "Resetting $X"
            echo 0 > "$X/authorized"
            sleep( 0.5 )
            echo 1 > "$X/authorized"
        fi
    fi
done

