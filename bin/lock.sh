#!/bin/bash
# a lot taken from https://github.com/xero/glitchlock

# get screen measures
rm /tmp/lock*.png
scrot /tmp/lock.png
W=`identify -ping -format '%w' /tmp/lock.png`
H=`identify -ping -format '%h' /tmp/lock.png`

convert -size ${W}x${H} canvas:\#362d24 /tmp/lock.png

ICON="/home/eye/img/stop.png"
LOCK=()
while read LINE
do
	if [[ "$LINE" =~ ([0-9]+)x([0-9]+)\+([0-9]+)\+([0-9]+) ]]; then
		W=${BASH_REMATCH[1]}
		H=${BASH_REMATCH[2]}
		Xoff=${BASH_REMATCH[3]}
		Yoff=${BASH_REMATCH[4]}
		if [ ! -z "$ICON" ]; then
			IW=`identify -ping -format '%w' $ICON`
			IH=`identify -ping -format '%h' $ICON`
			MIDXi=$(($W / 2 + $Xoff - $IW / 2))
			MIDYi=$(($H / 2 + $Yoff - $IH / 2))
			LOCK+=($ICON -geometry +$MIDXi+$MIDYi -composite)
		fi
	fi
done <<<"$(xrandr)"

file=/tmp/lock.png
# put icon on it
convert "$file" "${LOCK[@]}" "$file"


# lock 
i3lock -n --bar-indicator --bar-position h --bar-direction 1 --redraw-thread -t "" --bar-step 50 --bar-width 250 --bar-base-width 50 --bar-max-height 100 --bar-periodic-step 50 --bar-color 362d24ff --keyhlcolor 00666666 --ringvercolor cc87875f --wrongcolor ffff0000 --veriftext="" --wrongtext="" --noinputtext="" -i "$file"
