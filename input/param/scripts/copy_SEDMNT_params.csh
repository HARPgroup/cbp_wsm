#!/bin/csh

 if (${#argv} != 2) then
   echo ' '
   echo 'usage: copy_SEDMNT_params.csh from_scenario to_scenario'
   echo ' '
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   cp -v ../$lu/$scen1/SEDMNT.csv ../$lu/$scen2/SEDMNT.csv
 end

 foreach lu ($implnds )
   cp -v ../$lu/$scen1/SOLIDS.csv ../$lu/$scen2/SOLIDS.csv
 end


