#!/usr/bin/bash
VOL=$(ponymix defaults | grep source\   -A 3 | egrep -o "[0-9]+%")
ponymix --source is-muted && VOL=M 
if [[ $VOL == "M" ]]
then
  echo "<fc=#bb9584><icon=/home/eye/.xmobar/xbm/mic1.xbm/> $VOL</fc>"
else
  echo "<icon=/home/eye/.xmobar/xbm/mic1.xbm/> $VOL"
fi
