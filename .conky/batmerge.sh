if [[ `acpi | wc -l` -gt 1 ]]
then
# battery merging status output (using ultrabay + normal battery)
perc=`upower -d | awk '/energy:/ {n=n+$2} /energy-full:/ {m=m+$2} END {print (n*100/m)}'`
else
perc=`acpi | cut -d\, -f2 | egrep "[0-9]+" -o`
fi
percn=`printf %.0f $perc`
if [[ $percn -lt 15 ]]
then
  sleep 1
  notify-send '>>>> BAT LOW! <<<<<'
  #echo ">>> BAT LOW <<<" | dzen2 -p 5 -h 50 -fg  \#2a1f1d -bg \#9b6c4a 
fi
printf %.0f $perc

