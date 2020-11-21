#!/bin/sh
NARGS=${1:-10}
ps -eo pid,ppid,cmd,%mem,%cpu --sort=-%cpu,-%mem | head -n "${NARGS}"
