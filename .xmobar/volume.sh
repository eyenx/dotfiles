#!/usr/bin/bash
VOL=$(ponymix defaults | grep sink\   -A 3 | egrep -o "[0-9]+%")
ponymix --sink is-muted && VOL=M 
if [[ $VOL == "M" ]]
then
  echo "<fc=#bb9584><icon=/home/eye/.xmobar/xbm/vol3.xbm/></fc>"
else
  echo "<icon=/home/eye/.xmobar/xbm/vol1.xbm/> $VOL"
fi
