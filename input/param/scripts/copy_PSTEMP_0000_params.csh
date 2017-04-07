#!/bin/csh

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 source ../../../run/fragments/set_landuse

 foreach lu ($perlnds )
   cp -v ../p53/input/param/$lu/$scen1/PWTGAS.csv ../$lu/$scen2/PWTGAS.csv
   cp -v ../p53/input/param/$lu/$scen1/PSTEMP_0000.csv ../$lu/$scen2/PSTEMP.csv
 end

 foreach lu ($implnds )
   cp -v ../p53/input/param/$lu/$scen1/IWTGAS.csv ../$lu/$scen2/
 end


