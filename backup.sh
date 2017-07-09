#!/bin/bash
# Original script : https://wiki.archlinux.org/index.php/Full_System_Backup_with_rsync
# Modified by : Dilawar Singh <dilawars@ncbs.res.in>

if [ $# -lt 2 ]; then 
    echo "No source and destination defined. Usage: $0 source destination" >&2
    exit 1
elif [ $# -gt 2 ]; then
    echo "Too many arguments. Usage: $0 source destination" >&2
    exit 1
elif [ ! -d "$1" ]; then
   echo "Invalid path: $1" >&2
   exit 1
   if [ ! -d "$1" ]; then
     echo "Invalid path: $2" >&2
     exit 1
   fi
elif [ ! -w "$2" ]; then
  echo "Directory not writable: $2" >&2
  exit 1
fi

# Enable it if you are backing up in /media mount point.
case "$2" in
  "/mnt") ;;
  "/mnt/"*) ;;
  "/media") ;;
  "/media/"*) ;;
  *) echo "Destination not allowed." >&2 
     exit 1 
     ;;
esac

START=$(date +%s)
outfile=$2/"Backup_$(date '+%A,%d%B%Y,%T')"
rsync -azv --progress $1 $2 | tee $outfile
FINISH=$(date +%s)
echo "total time: $(( ($FINISH-$START) / 60 )) minutes, $(( ($FINISH-$START) % 60 )) seconds"
notify-send "Done backing up $1 to $2"
