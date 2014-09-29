#!/usr/bin/perl -w
#
# get wallpaper of the day at thepaperwall.com and output it to stdout
#
# author: eye@eyenx.ch

# uses

use v5.14;
use strict;
use LWP::Simple;

# statics

my $url = "http://thepaperwall.com";
get($url) =~ m/.*<div.*active.*href="wallpaper\.php\?.*mblock.*<img src="(\/image\.php\?).*(image=.*)" alt.*<\/a><\/div>/;
print get($url.$1.$2);
