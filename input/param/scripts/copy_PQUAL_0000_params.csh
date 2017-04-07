#!/bin/csh

 if (${#argv} != 2) then
   echo ' '
   echo 'usage: copy_PQUAL_params.csh from_scenario to_scenario'
   echo ' '
   exit
 endif

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 set qualnums = ( 1 2 3 4 5 6 )

 foreach lu ($perlnds )
   foreach num ($qualnums)
     cp -v ../../../../p53/input/param/$lu/$scen1/PQUAL$num*.csv ../$lu/$scen2/
   end
 end

 foreach lu ($implnds )
   foreach num ($qualnums)
     cp -v ../../../../p53/input/param/$lu/$scen1/IQUAL$num*.csv ../$lu/$scen2/
   end
 end


