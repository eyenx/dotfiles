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

/usr/bin/xautolock -time 15 -locker slock &
feh --bg-scale --no-fehbg ~/.wp.png
udiskie &
unclutter &
dunst &
#windowmanager
xinput disable $(xinput list --id-only "Elan Touchpad")
# gnome keyring
#eval $(/usr/bin/env gnome-keyring-daemon --start --components=secrets)
redshift-gtk &
~/bin/mytrayer.sh &
~/bin/gotify-send.py &
xmodmap ~/.Xmodmap
ldd ~/.xmonad/xmonad-x86_64-linux | grep "not found" &>/dev/null && mv ~/.xmonad/xmonad-x86_64-linux{,.old}
xmonad
