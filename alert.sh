# From here:
# http://askubuntu.com/questions/409611/desktop-notification-when-long-running-commands-complete

function alert() 
{
    start=$(date +%s)
    "$@"
    [ $(($(date +%s) - start)) -le 15 ] || \
        notify-send "Notification" \
        "Long running command \"$(echo $@)\" took $(($(date +%s) - start)) seconds to finish"
}
