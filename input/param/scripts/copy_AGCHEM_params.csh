#!/bin/csh

 if (${#argv} != 2) then
   echo ' '
   echo 'usage: copy_AGCHEM_params.csh from_scenario to_scenario'
   echo ' '
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 set afiles = ( MSTLAY NITR1 NITR2 NITR3 NITR4 NITR5 NITR6 NITR7 NITR8 RORGN PHOS1 PHOS2 PHOS3 PHOS4 PHOS5 )

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   mkdir -p ../$lu/$scen2
   foreach file ($afiles)
     cp -vp ../$lu/$scen1/${file}.csv ../$lu/$scen2/
   end
 end

