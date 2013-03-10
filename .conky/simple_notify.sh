#!/usr/bin/bash
## working together with simple_notify.pl script for irssi. Check tmp file for flagging, play sound and output message for conky

tfile='/tmp/.notify_irssi'
sound='/usr/share/sounds/freedesktop/stereo/message-new-instant.oga'
## take same as refresh rate in conky. this is used to not play the sound twice for the same message
interval=5;
flag=`cat $tfile`

[[ $flag == "0" ]] && echo 0 && exit 0;

date_now=`date +%s`
date_file=`stat -c %Y $tfile`
date_diff=$((date_now - date_file))

if [[ $date_diff -le $interval ]]
then
    mplayer $sound &>/dev/null
fi

echo 1
