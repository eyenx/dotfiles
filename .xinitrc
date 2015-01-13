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
#perl -e 'use LWP::Simple;use JSON;print get(from_json(get("https://api.desktoppr.co/1/wallpapers/random"),{utf8 => 1})->{"response"}->{"image"}->{"url"})' | feh --bg-scale --no-fehbg - &
~/bin/randompatternsvg.rb > /tmp/.w.svg && feh --magick-timeout 1 --bg-tile --no-fehbg /tmp/.w.svg
#xsetroot -solid "#1f1f1b"
udiskie &
unclutter &
urxvtd -f -o -q
trayer --edge top --widthtype request --SetDockType true --SetPartialStrut true --align right --margin 0 --height 16 --padding 0 --transparent true --alpha 0 --tint 0x303030&
# create download folder
mkdir /tmp/dwn
#windowmanager
xmonad
