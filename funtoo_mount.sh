#!/usr/bin/env bash
echo "Mouting filesystems"
(
    sudo mount -t proc none proc
    sudo mount --rbind /sys sys
    sudo mount --rbind /dev dev
    echo "Chrooting into it"
    if [ ! -f etc/resolv.conf ]; then
        cp /etc/resolv.conf etc
    fi
    sudo env -i HOME=/root TERM=$TERM /usr/sbin/chroot . bash -l
)
export PS1="(chroot ) $PS1"
