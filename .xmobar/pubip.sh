#!/bin/bash
pubip=$(curl --silent ipinfo.io/ip)
if [[ $? -gt 0 ]]
then
  echo pub: "n/a"
else
  echo pub: $pubip
fi
