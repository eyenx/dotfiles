#!/usr/bin/env ruby

require 'geo_pattern';

# generators
#
gens=[
'octogons',
'overlapping_circles',
'plus_signs',
'xes',
'sine_waves',
'hexagons',
'overlapping_rings',
'plaid',
'triangles',
'squares',
'concentric_circles',
'diamonds',
'tessellation',
'nested_squares',
'mosaic_squares',
'chevrons'];

# get chosen generator randomly 

gen=gens[Random.new.rand(gens.length)];

# generate random string

rcount=Random.new.rand(100);
letters=(('a'..'z').to_a + ('A'..'Z').to_a + (0..9).to_a);
rstring="";
while rcount>0 
    rstring<<letters[Random.new.rand(letters.length)].to_s;
    rcount-=1;
end

# generate pattern from random string

puts GeoPattern.generate(rstring,{:generator => gen}).svg_string;
