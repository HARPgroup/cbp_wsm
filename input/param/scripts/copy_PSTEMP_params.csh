#!/bin/csh

 if (${#argv} != 2) then
   echo ' '
   echo 'usage: copy_PSTEMP_params.csh from_scenario to_scenario'
   echo ' '
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   cp -vp ../$lu/$scen1/PWTGAS.csv ../$lu/$scen2/PWTGAS.csv
   cp -vp ../$lu/$scen1/PSTEMP.csv ../$lu/$scen2/PSTEMP.csv
 end

 foreach lu ($implnds )
   cp -vp ../$lu/$scen1/IWTGAS.csv ../$lu/$scen2/
 end


