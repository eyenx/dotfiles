#!/bin/bash
wget checkip.dyndns.org -qO-|egrep -o "[0-9]{1,3}(\.[0-9]{1,3}){3}" 
