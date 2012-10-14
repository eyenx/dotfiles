#!/usr/bin/python3

import urllib.request,re

#site

#site='http://www.meteocentrale.ch/en/europe/switzerland/weather-pratteln/details/N-3516319/'
site='http://www.meteocentrale.ch/en/europe/switzerland/weather-zollikofen/details/N-3518015/'

#get html data

html=urllib.request.urlopen(site)
html=html.read().decode()

# get weather data

pat=re.compile(".*<div id\=\"weather-detail-summary\">(.*)<noscript.*")
datastart=html.index('\t\t\t\t<div id="weather-detail-summary">')
mtch=pat.search(html.replace("\n",""))
data=mtch.group(1)


# Actual status

pat=re.compile(".*class=\"mm_detail.*title\=\"(.*)\"><\/div><\/center>.*div class=\"column.*>([0-9]*)<span>.*")
mtch=pat.match(data)
statnow=mtch.group(1)
tempnow=mtch.group(2)

# return command for conky

print(statnow+', '+tempnow+' C°')
