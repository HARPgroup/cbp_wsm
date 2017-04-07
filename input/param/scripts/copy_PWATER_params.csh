#!/bin/csh

 if (${#argv} != 2) then 
   echo ' ' 
   echo 'usage: copy_PWATER_params.csh from_scenario to_scenario'
   echo ' ' 
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   mkdir -p ../$lu/$scen2
   cp -vp ../$lu/$scen1/ATEMP.csv ../$lu/$scen2/
   cp -vp ../$lu/$scen1/SNOW.csv ../$lu/$scen2/
   cp -vp ../$lu/$scen1/PWATER.csv ../$lu/$scen2/
 end

 foreach lu ($implnds )
   mkdir -p ../$lu/$scen2
   cp -vp ../$lu/$scen1/ATEMP.csv ../$lu/$scen2/
   cp -vp ../$lu/$scen1/SNOW.csv ../$lu/$scen2/
   cp -vp ../$lu/$scen1/IWATER.csv ../$lu/$scen2/
 end

 mkdir -p ../common/$scen2
 cp -vp ../common/$scen1/land_evap.csv ../common/$scen2/land_evap.csv

