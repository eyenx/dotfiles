# battery merging status output (using ultrabay + normal battery)
perc=`upower -d | awk '/energy:/ {n=n+$2} /energy-full:/ {m=m+$2} END {print (n*100/m)}'`
printf %.0f $perc
