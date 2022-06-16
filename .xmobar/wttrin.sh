#!/bin/bash
wttrin=$(curl -s "https://wttr.in/?format=+%C+%t")
if [[ $? -gt 0 ]]
then
  echo "n/a"
else
  echo $wttrin
fi
