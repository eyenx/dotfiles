#!/usr/bin/python

import i3
import sys

targetwidth = float(sys.argv[1])

# get currently focused window
current=i3.filter(nodes=[], focused=True)
currentsize = current[0]['percent'] * 100;

d = int(targetwidth - currentsize)

if d>0:
    i3.resize("grow width" + str(d) + " px or " + str(d) + " ppt");
else:
    i3.resize("shrink width" + str(-d) + " px or " + str(-d) + " ppt");
