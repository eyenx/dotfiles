#!/bin/sh
set -e
if [ ! -t 0 ]; then
	x-terminal-emulator -e "$0"
	exit 0
fi
basedir="$(dirname "$(readlink -f "${0}")")"
cd "$basedir"
color="$1"
case $color in
	-c|--color)
	newcolor="$2";;
esac
color2="$3"
case $color2 in
	-c2|--color2)
	newcolor2="$4";;
esac
#well, we need some common tools
(type sed >/dev/null 2>&1 && type tr >/dev/null 2>&1 && type find >/dev/null 2>&1)||_missingdep=true
case $_missingdep in
	true)
		printf "You either miss sed, tr, or find! Aborting!\n"
		exit 1;;
esac
#check if we have permissions
if [ ! -w "$basedir" ]; then
	printf "You don't have write permissions!\n"
	exit 1
fi
#check for the right dir
if [ ! -d "$basedir"/gtk-3.0 ]; then
	printf "wrong dir! or wrongly installed\n"
	exit 1
fi
#check for the right dir
if [ ! -d "$basedir"/Extras ]; then
	printf "\n\nExtras are wrongly or not installed!!!\n\n"
fi
####################
###set the background color
####################
if [ -z ${newcolor} ]; then 
	cat <<\EOF


Unavailable Colors:
BB2835,285CBB,BB6F28,288ABB,5DBB28,
ffffff,000000,f1f2f2,424141,323131

Some online color pickers:
https://www.w3schools.com/colors/colors_picker.asp
http://www.color-hex.com

or use gimp, yad --color etc.

EOF
fi
while [ 1 ];do
	_has_error=0
	if [ -z ${newcolor} ]; then 
			read -p "Please enter your new background color in #RRGGBB(The '#' is a must!, #0076e2 is currently set): " newcolor
	fi
#check if there is a color
	if [ -z ${newcolor+x} ]; then
		_has_error=1
		printf "No bgcolor was was selected\n"
	fi
#some fail colors(colors already used by theme)
	case "$(echo ${newcolor}|sed 's/#//'|tr '[:upper:]' '[:lower:]')" in
		BB2835|285CBB|BB6F28|288ABB|5DBB28|ffffff|000000|f1f2f2|424141|323131)
			printf "\nSorry the colors: BB2835,285CBB,BB6F28,288ABB,5DBB28,ffffff,000000,f1f2f2,424141,323131 are not available!\n"
			_has_error=1;;
	esac
#check if we have a proper hex color #RRGGBB
	if [ "${#newcolor}" -lt 7 ]; then
		_has_error=1
		printf "\nSorry ${newcolor} is not a valid color!\n"
	fi
	if [ "$_has_error" = "0" ]; then
		break
	else
		unset newcolor
	fi
done

####################
###same for the fg color
####################
while [ 1 ];do
	_has_error=0
	if [ -z ${newcolor2} ]; then 
			read -p "Please enter your new foreground color in #RRGGBB(The '#' is a must!, #f2f1f1 is currently set): " newcolor2
	fi
#check if there is a color
	if [ -z ${newcolor2+x} ]; then
		_has_error=1
		printf "No fgcolor was was selected\n"
	fi
#some fail colors(colors already used by theme)
	case "$(echo ${newcolor2}|sed 's/#//'|tr '[:upper:]' '[:lower:]')" in
		BB2835|285CBB|BB6F28|288ABB|5DBB28|ffffff|000000|f1f2f2|424141|323131)
			printf "\nSorry the colors: BB2835,285CBB,BB6F28,288ABB,5DBB28,ffffff,000000,f1f2f2,424141,323131 are not available!\n"
			_has_error=1;;
	esac
#check if we have a proper hex color #RRGGBB
	if [ "${#newcolor2}" -lt 7 ]; then
		_has_error=1
		printf "\nSorry ${newcolor2} is not a valid color!\n"
	fi
	if [ "$_has_error" = "0" ]; then
		break
	else
		unset newcolor2
	fi
done

#recolor the theme
printf "\n\nChanging color\n\n"
find "$basedir" -type f -not -path "*/.git/*" -not -path "gtksourceview" -not -path "metacity-1" -not -path "xfwm4" -exec sed -i 's/#0076e2/'$newcolor'/g' {} \;
find "$basedir" -type f -not -path "*/.git/*" -not -path "gtksourceview" -not -path "metacity-1" -not -path "xfwm4" -exec sed -i 's/#0076E2/'$newcolor'/g' {} \;
find "$basedir" -type f -not -path "*/.git/*" -not -path "gtksourceview" -not -path "metacity-1" -not -path "xfwm4" -exec sed -i 's/#f2f1f1/'$newcolor2'/g' {} \;
find "$basedir" -type f -not -path "*/.git/*" -not -path "gtksourceview" -not -path "metacity-1" -not -path "xfwm4" -exec sed -i 's/#F2F1F1/'$newcolor2'/g' {} \;
cat <<\EOF


d8888b.  .d88b.  d8b   db d88888b db 
88  `8D .8P  Y8. 888o  88 88'     88 
88   88 88    88 88V8o 88 88ooooo YP 
88   88 88    88 88 V8o88 88~~~~~    
88  .8D `8b  d8' 88  V888 88.     db 
Y8888D'  `Y88P'  VP   V8P Y88888P YP 


NOTE: you might have to re-login to apply on all apps!

EOF
sleep 5
exit 0
