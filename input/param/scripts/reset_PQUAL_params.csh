#!/bin/csh

 if (${#argv} != 1) then
   echo ' '
   echo 'usage: reset_PQUAL_params.csh scenario '
   echo ' '
   exit
 endif

 set scen = $argv[1]

 source ../../../run/fragments/set_landuse

 set qualnums = ( 1 2 3 4 5 )

 foreach lu ($perlnds )
   foreach num ($qualnums)
     mv -v ../$lu/$scen/PQUAL${num}_0000.csv ../$lu/$scen/PQUAL${num}.csv
     rm -v ../$lu/$scen/PQUAL${num}_*.csv
   end
 end

 foreach lu ($implnds )
   foreach num ($qualnums)
     mv -v ../$lu/$scen/IQUAL${num}_0000.csv ../$lu/$scen/IQUAL${num}.csv
     rm -v ../$lu/$scen/IQUAL${num}_*.csv
   end
 end


