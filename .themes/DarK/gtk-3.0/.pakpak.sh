#!/bin/sh
set -e
_basedir="$(dirname "$(readlink -f "${0}")")"
cd "$_basedir"
if [ -d "$_basedir/theme" ]; then
	printf "packing....\n"
	if type glib-compile-resources >/dev/null 2>&1; then
		printf "glib-compile-resources found\n"
	else
		printf "glib-compile-resources not found - Aborting!\n"
		exit 1
	fi
	if type xmllint >/dev/null 2>&1; then
		printf "xmllint found\n"
	else
		printf "xmllint not found - Aborting!\n"
		exit 1
	fi
	cat <<\EOF > "$_basedir/"gtk.gresource.xml
<?xml version="1.0" encoding="UTF-8"?>
<gresources>
<gresource prefix="/org/gnome/">
EOF
	printf "<file>gtk-main.css</file>\n" >> "$_basedir/"gtk.gresource.xml
	for _f in $(find theme -name "*.css"); do
		printf "<file>$_f</file>\n" >> "$_basedir/"gtk.gresource.xml
	done
	printf "</gresource>\n</gresources>\n" >> gtk.gresource.xml
	glib-compile-resources "$_basedir/"gtk.gresource.xml
	if [ -f "$_basedir/"gtk.gresource ]; then
		rm "$_basedir/"gtk.gresource.xml "$_basedir/"gtk-main.css
		rm -rf "$_basedir/"theme 
	fi
else
	printf "unpacking....\n"
	GR_FILE="$_basedir/gtk.gresource"
	GR_BASEDIR="/org/gnome/"
	for REQUIRED_PROG in gresource; do
		which ${REQUIRED_PROG} &>/dev/null
		if [ "$?" -ne "0" ]; then
			echo "Unable to find required program '${REQUIRED_PROG}' in PATH."
			exit 1
		fi
	done
	for RSRC in $(gresource list $GR_FILE)
	do
		RSRC_FILE=$(echo "${RSRC#$GR_BASEDIR}")
		mkdir -p $(dirname "$RSRC_FILE") ||:
		gresource extract "$GR_FILE" "$RSRC" > "$RSRC_FILE"
	done
	if [ -d "$_basedir/theme" ]; then
		rm "$_basedir/"gtk.gresource
	fi
fi
