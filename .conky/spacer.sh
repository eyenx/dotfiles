#!/usr/bin/env bash
printf "%$(echo $(xwininfo -name panel|awk '/Width/ {print $2}')*0.16|bc)s"
