#!/bin/csh

 if (${#argv} != 2) then
   echo ' '
   echo 'usage: copy_params.csh from_scenario to_scenario'
   echo ' '
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds $implnds common)

   mkdir -p ../$lu/$scen2

   cp -pv ../$lu/$scen1/*.csv ../$lu/$scen2

 end
