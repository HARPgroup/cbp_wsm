#!/bin/csh

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   cp -v ../../../../p53/input/param/$lu/$scen1/SEDMNT_0000.csv ../$lu/$scen2/SEDMNT.csv
 end

 foreach lu ($implnds )
   cp -v ../../../../p53/input/param/$lu/$scen1/SOLIDS_0000.csv ../$lu/$scen2/SOLIDS.csv
 end


