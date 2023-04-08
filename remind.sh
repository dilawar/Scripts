#!/usr/bin/env bash
set -e
(
    flock -x -n /tmp/__remind_lock_ -c "sleep 1" || (echo "Already running"; exit 1)

    # if gcalcli is not found. do nothing.


    if ! [ -x "$(command -v gcalcli)" ]; then
        echo "gcalcli is not installed."
        exit 1;
    fi

    while true; do
        gcalcli --calendar="dilawar" remind
        sleep 300
    done &
) 200> $HOME/.cache/remind.sh.exclusivelock
