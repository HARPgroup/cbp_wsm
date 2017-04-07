#!/bin/csh

 set scen1 = $argv[1]

 source ../../../run/fragments/set_landuse

 cd ../

 foreach lu ($perlnds )
   mv -v $lu/$scen1/NITR1_0000.csv $lu/$scen1/NITR1.csv
   rm -v $lu/$scen1/NITR1_0*.csv

   mv -v $lu/$scen1/NITR4_0000.csv $lu/$scen1/NITR4.csv
   rm -v $lu/$scen1/NITR4_0*.csv

   mv -v $lu/$scen1/NITR8_0000.csv $lu/$scen1/NITR8.csv
   rm -v $lu/$scen1/NITR8_0*.csv

   mv -v $lu/$scen1/RORGN_0000.csv $lu/$scen1/RORGN.csv
   rm -v $lu/$scen1/RORGN_0*.csv

   mv -v $lu/$scen1/PHOS1_0000.csv $lu/$scen1/PHOS1.csv
   rm -v $lu/$scen1/PHOS1_0*.csv

   mv -v $lu/$scen1/PHOS2_0000.csv $lu/$scen1/PHOS2.csv
   rm -v $lu/$scen1/PHOS2_0*.csv
 end
