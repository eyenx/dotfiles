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

#/usr/bin/xautolock -time 15 -locker slock &
/usr/bin/xautolock -time 15 -locker ~/bin/lock.sh & 
#perl -e 'use LWP::Simple;use JSON;print get(from_json(get("https://api.desktoppr.co/1/wallpapers/random"),{utf8 => 1})->{"response"}->{"image"}->{"url"})' | feh --bg-scale --no-fehbg - &
#~/bin/randompatternsvg.rb > /tmp/.w.svg && feh --magick-timeout 1 --bg-tile --no-fehbg /tmp/.w.svg
feh --bg-scale --no-fehbg ~/.wp.png
#xsetroot -solid "#303030"
udiskie &
unclutter &
dunst &
urxvtd -f -o -q
#~/bin/mytrayer.sh
~/bin/snom-eventd.py &
#windowmanager
xinput disable $(xinput list --id-only "SynPS/2 Synaptics TouchPad")
# gnome keyring
eval $(usr/bin/gnome-keyring-daemon --start --components=secrets)
#~/bin/emulatewheel.sh
redshift-gtk &
i3
#2bwm
#xmonad
