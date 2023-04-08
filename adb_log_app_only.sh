#!/usr/bin/env bash
set -e
set -x
PID=$(adb shell pidof -s $1)
if [ -z "${PID}" ]; then
    echo "No PID for $1"
    exit;
fi
adb logcat --pid=$PID
