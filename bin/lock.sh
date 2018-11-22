#!/bin/bash
#rm -f /tmp/lock.png
#scrot /tmp/lock.png
#convert /tmp/lock.png -scale 10% -scale 1000% /tmp/lock.png
#convert /tmp/lock.png  ~/.lock.png -gravity center -composite -matte /tmp/lock.png
#i3lock -u -i /tmp/lock.png
rm ~/.solid.png
convert -size `xdpyinfo|grep dimensions|awk '{print $2}'` xc:\#2a1f1d ~/.solid.png
#convert ~/.solid.png ~/.lock.png -gravity center -composite -matte ~/.solid.png
convert ~/.solid.png ~/.lock.png -geometry +50+800 -composite ~/.solid.png
i3lock -u -i ~/.solid.png

