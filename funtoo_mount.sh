#!/usr/bin/env bash
echo "Mouting filesystems"
mount -t proc none proc
mount --rbind /sys sys
mount --rbind /dev dev
echo "Chrooting into it"
if [ ! -f etc/resolv.conf ]; then
    cp /etc/resolv.conf etc
fi
env -i HOME=/root TERM=$TERM chroot . bash -l
