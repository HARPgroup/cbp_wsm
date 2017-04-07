#!/bin/csh

 set scen1 = $argv[1]
 set scen2 = $argv[2]

 mkdir $scen2

 cp ${scen1}/${scen1}_factors.dat ${scen2}/${scen2}_factors.dat
 cp ${scen1}/${scen1}_sensitivities.f ${scen2}/${scen2}_sensitivities.f
 cp ${scen1}/set_obscen ${scen2}/set_obscen
 cp -r ${scen1}/make_calib_weights ${scen2}/make_calib_weights

