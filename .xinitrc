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
udiskie &
unclutter &
urxvtd -f -o -q
~/.xmonad/conkyxmobar &
u1sdtool -c &
tĥunar --daemon &
mplayer "http://translate.google.com/translate_tts?tl=en&q=welcome+$(whoami)" &
~/bin/mpdstart &
# Start a D-Bus session
source /etc/X11/xinit/xinitrc.d/30-dbus
# Start GNOME Keyring
eval $(/usr/bin/gnome-keyring-daemon --start --components=gpg,pkcs11,secrets,ssh)
# You probably need to do this too:
export SSH_AUTH_SOCK
export GPG_AGENT_INFO
export GNOME_KEYRING_CONTROL
export GNOME_KEYRING_PID

#exec openbox-session
#exec ck-launch-session openbox-session
xmonad
# exec startxfce4
# ...or the Window Manager of your choice
