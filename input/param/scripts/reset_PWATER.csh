#!/bin/csh

 set scen1 = $argv[1]

 source ../../../run/fragments/set_landuse

 cd ../

 foreach lu ($perlnds )
   mv -v $lu/$scen1/PWATER_0000.csv $lu/$scen1/PWATER.csv
   rm -v $lu/$scen1/PWATER_0*.csv
 end

 mv -v common/$scen1/land_evap_0000.csv common/$scen1/land_evap.csv
 rm -v common/$scen1/land_evap_0*.csv


