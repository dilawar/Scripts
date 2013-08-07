#! /bin/bash
temp="JXJhc2htaXJhdGhpCg=="
pass=`echo "$temp" | base64 --decode`
echo $pass
