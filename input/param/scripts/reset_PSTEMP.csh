#!/bin/csh

 set scen1 = $argv[1]

 source ../set_landuse

 foreach lu ($perlnds )
   cp $lu/$scen1/PSTEMP_0000.csv $lu/$scen1/PSTEMP.csv
   rm $lu/$scen1/PSTEMP_0*.csv
 end


