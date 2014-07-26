#!/usr/bin/bash
## working together with simple_notify.pl script for irssi. Check tmp file for flagging, play sound and output message for conky

tfile='/tmp/.notifyirssi'
sound='/usr/share/sounds/freedesktop/stereo/message-new-instant.oga'

[[ ! -f $tfile ]] && exit 1;

flag=`cat $tfile`

if [[ $flag -gt 0 ]]
then
    mplayer $sound &>/dev/null &
    echo -n 0 > $tfile
fi

exit 0
