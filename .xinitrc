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

/usr/bin/xautolock -time 15 -locker slimlock &
feh --bg-scale --no-fehb $(find ~/img/wallpapers/wallbase/ -iname "wallbase*jpg" -type f|sort -R|head -1) &
numlockx &
udiskie &
unclutter &
urxvtd -f -o -q
trayer --edge top --widthtype request --SetDockType true --SetPartialStrut true --align right --margin 0 --height 16 --padding 0 --transparent true --alpha 0 --tint 0x1f1f1b&
#windowmanager
xmonad
