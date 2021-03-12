#!/usr/bin/env bash
lat=$(ping 9.9.9.9 -q -c 10 -W 10|tail -1|awk '{print $4}'|cut -d/ -f2)
if [[ $? -gt 0 ]]
then
  lat="n/a"
else
  lat=$lat
fi
echo "<icon=/home/eye/.xmobar/xbm/net_down_03.xbm/> $lat ms"
