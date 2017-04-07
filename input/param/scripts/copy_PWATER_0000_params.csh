#!/bin/csh

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   mkdir -p ../$lu/$scen2
   cp ../$lu/$scen1/ATEMP.csv ../$lu/$scen2/
   cp ../$lu/$scen1/SNOW.csv ../$lu/$scen2/
   cp ../$lu/$scen1/PWATER_0000.csv ../$lu/$scen2/PWATER.csv
 end

 foreach lu ($implnds )
   mkdir -p ../$lu/$scen2
   cp ../$lu/$scen1/ATEMP.csv ../$lu/$scen2/
   cp ../$lu/$scen1/SNOW.csv ../$lu/$scen2/
   cp ../$lu/$scen1/IWATER.csv ../$lu/$scen2/
 end

 mkdir -p ../common/$scen2
 cp ../common/$scen1/land_evap_0000.csv ../common/$scen2/land_evap.csv

