#!/usr/bin/env bash
# a lot taken from https://github.com/xero/glitchlock

# get screen measures
rm /tmp/lock*.png
scrot /tmp/lock.png
W=`identify -ping -format '%w' /tmp/lock.png`
H=`identify -ping -format '%h' /tmp/lock.png`

convert -size ${W}x${H} canvas:\#3b3228 /tmp/lock.png

ICON="/home/eye/img/stop_256.png"
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
            MIDXi=$(($W*2/100 + $Xoff))
            MIDYi=$(($H*75/100 + $Yoff)) 
			LOCK+=($ICON -geometry +$MIDXi+$MIDYi -composite)
		fi
	fi
done <<<"$(xrandr)"

file=/tmp/lock.png
# put icon on it
convert "$file" "${LOCK[@]}" "$file"


# pause dunst
pkill -u "$USER" -SIGUSR1 dunst

# lock 
i3lock -c 3b3228 -u -k --timepos="x+w*0.20:y+h*0.90" --timecolor bb9584 --datecolor bb9584 --datestr="%A, %d. %B %Y" --keyhlcolor 00666666 --ringvercolor cc87875f --wrongcolor ffff0000 --veriftext="" --wrongtext="" --noinputtext="" -i "$file"

# resume dunst
pkill -u "$USER" -SIGUSR2 dunst
