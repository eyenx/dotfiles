if [[ `hostname` == 'mantis' ]] ; then 
 xinput set-prop "AlpsPS/2 ALPS DualPoint Stick" "Evdev Wheel Emulation" 1
 xinput set-prop "AlpsPS/2 ALPS DualPoint Stick" "Evdev Wheel Emulation Button" 2
 xinput set-prop "AlpsPS/2 ALPS DualPoint Stick" "Evdev Wheel Emulation Timeout" 200
 xinput set-prop "AlpsPS/2 ALPS DualPoint Stick" "Evdev Wheel Emulation Axes" 6 7 4 5
fi
if [[ `hostname` == 'hornet' ]]; then
 xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 1
 xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 2
 xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 200
 xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
fi
