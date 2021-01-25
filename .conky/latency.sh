#!/usr/bin/env bash
lat=$(ping 1.1.1.1 -q -c 10 -W 10|tail -1|awk '{print $4}'|cut -d/ -f2)
echo "<icon=/home/eye/.xmobar/xbm/net_down_03.xbm/> $lat ms"
