#!/usr/bin/bash
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

# launch my bar on every active monitor
for m in $(polybar --list-monitors | cut -d":" -f1); do
      MONITOR=$m polybar --reload mybar &
done

