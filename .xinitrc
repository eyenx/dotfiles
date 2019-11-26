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

/usr/bin/xautolock -time 15 -locker bin/lock.sh &
feh --bg-scale --no-fehbg ~/.wp.png
udiskie &
unclutter &
dunst &
urxvtd -f -o -q
#windowmanager
xinput disable $(xinput list --id-only "SynPS/2 Synaptics TouchPad")
# gnome keyring
eval $(usr/bin/gnome-keyring-daemon --start --components=secrets)
redshift-gtk &
~/bin/mytrayer.sh &
~/bin/gotify-send.py &
xmodmap ~/.Xmodmap
xmonad
