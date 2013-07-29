#!/bin/bash
printf "%$(echo $(xwininfo -name panel|grep Width|tr -s ' '|cut -d' ' -f3)*0.2|bc)s"
