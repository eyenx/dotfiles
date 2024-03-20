#!/bin/bash
pubip=$(curl --silent -4 ifconfig.me)
if [[ $? -gt 0 ]]; then
	echo pub: "n/a"
else
	echo pub: $pubip
fi
