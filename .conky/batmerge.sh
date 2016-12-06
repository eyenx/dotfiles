if [[ `acpi | wc -l` -gt 1 ]]
then
# battery merging status output (using ultrabay + normal battery)
perc=`upower -d | awk '/energy:/ {n=n+$2} /energy-full:/ {m=m+$2} END {print (n*100/m)}'`
else
perc=`acpi | cut -d\, -f2 | egrep "[0-9]+" -o`
fi
printf %.0f $perc

