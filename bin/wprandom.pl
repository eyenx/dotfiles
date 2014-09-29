#!/bin/perl -w
#
# get random wallpaper from http://desktoppr.co using their API
#
# author: eye@eyenx.ch

# uses

use v5.14;
use strict;
use LWP::Simple;
use JSON;

# statics

my $api = "https://api.desktoppr.co/1/wallpapers/random";

# get json data and print out raw image

my $response = from_json(get($api),{utf8 => 1});
print get($response->{"response"}->{"image"}->{"url"});
