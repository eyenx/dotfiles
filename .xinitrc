#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi
[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

#/usr/bin/xscreensaver -no-splash &
/usr/bin/xautolock -time 15 -locker slock &
feh --bg-scale --no-fehb $(find ~/img/wallpapers/wallbase/ -iname "wallbase*jpg" -type f|sort -R|head -1) &
numlockx &
#PYTHONPATH="" udiskie &
udiskie &
unclutter &
urxvtd -f -o -q
#~/.xmobar/conkyxmobar &
#dropbox start & ## now using btsync
#u1sdtool -c &
#oneko -tofocus -speed 30 &
#stalonetray &
trayer --edge top --widthtype request --SetDockType true --SetPartialStrut true --align right --margin 0 --height 16 --padding 0 --transparent true --alpha 0 --tint 0x1f1f1b&
#mplayer "http://translate.google.com/translate_tts?tl=en&q=welcome+$(whoami)" &
#~/bin/mpdstart &
#notify-listener.py &
#mpdscribble &
# btsync start
~/bin/btsync.sh start
#skype &
#exec openbox-session
#exec ck-launch-session openbox-session
xmonad
# exec startxfce4
# ...or the Window Manager of your choice
