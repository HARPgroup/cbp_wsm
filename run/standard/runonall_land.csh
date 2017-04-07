#!/bin/csh

 set scenario = $argv[1]
 set basin = $argv[2]

 source ../../config/seglists/${basin}.land

 foreach seg ($segments)

   sbatch run_land_oneseg.csh $scenario $seg

 end
