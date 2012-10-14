#!/bin/bash

## shell script to read xml weather file from Google API and return Temparature and Weather Condition
## created by gnomeye 2012-01-05

#### Old Google API Shell script, depracated
#API='http://www.google.com/ig/api?weather='
#LOCATION='pratteln'
#LOCATION='zollikofen'
#LANG='en'
#
#URL=$API$LOCATION'&hl='$LANG
#
#XMLDATA=`curl -s $URL`
#CURCOND=`echo $XMLDATA | sed -n 's/.*<current_conditions>\(.*\)<\/current_conditions>.*/\1/p'`
#
#COND=`echo $CURCOND | sed -n 's/.*<condition data="\(.*\)"\/><temp_f.*/\1/p'`
#
#TEMP=`echo $CURCOND | sed -n 's/.*<temp_c data="\(.*\)"\/><humidity.*/\1/p'`
#
#RETVALUE=$COND', '$TEMP' C°'
#
#echo $RETVALUE

### Yahoo weather RSS 
RSS='http://weather.yahooapis.com/forecastrss'
WOEID='784780'
METR='c'
URL="${RSS}?w=${WOEID}&u=${METR}"
RET=$(curl -s "$URL" | grep "C<BR" | sed 's/^\(.*\)<.*$/\1/')

echo $RET
