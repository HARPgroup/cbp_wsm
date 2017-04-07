#!/bin/csh

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 mkdir -p $scen2
 mkdir -p ${scen2}/oldstats/

 cp -r ${scen1}/make_calib_weights ${scen2}/make_calib_weights
 cp ${scen1}/set_obscen ${scen2}/set_obscen
 cp ${scen1}/make_river_land_dat_${scen1}.xls ${scen2}/make_river_land_dat_${scen2}.xls 
 cp ${scen1}/${scen1}_factors.dat ${scen2}/${scen2}_factors.dat 
 cp ${scen1}/${scen1}_rfactors.dat ${scen2}/${scen2}_rfactors.dat 
 cp ${scen1}/${scen1}_orphans.csv ${scen2}/${scen2}_orphans.csv 
 cp ${scen1}/${scen1}_river_land.csv ${scen2}/${scen2}_river_land.csv 
 cp ${scen1}/${scen1}_sensitivities.f ${scen2}/${scen2}_sensitivities.f 
 cp ${scen1}/${scen1}_update_river.f ${scen2}/${scen2}_update_river.f 
 cp ${scen1}/lsegs.csv ${scen2}/
 cp ${scen1}/rsegs.csv ${scen2}/

