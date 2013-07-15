#!/bin/bash
i=300
file="/tmp/.weenotify"
s=`stat -c "%Y" $file`
d=$(date +%s)
c=$(echo $d - $s | bc -l)
wc=`wc -l $file | awk '{print $1}'`
[[ -e $file ]] || touch $file
[[ $wc -gt 0 ]] && tail -1 $file
[[ $c -ge $i ]]  &&  >$file 
