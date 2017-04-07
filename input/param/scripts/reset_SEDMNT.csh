#!/bin/csh

 set scen1 = $argv[1]

 source ../../../run/fragments/set_landuse

 cd ../

 foreach lu ($perlnds )
   mv -v $lu/$scen1/SEDMNT_0000.csv $lu/$scen1/SEDMNT.csv
   rm -v $lu/$scen1/SEDMNT_0*.csv
 end

 foreach lu ($implnds )
   mv -v $lu/$scen1/SOLIDS_0000.csv $lu/$scen1/SOLIDS.csv
   rm -v $lu/$scen1/SOLIDS_0*.csv
 end



