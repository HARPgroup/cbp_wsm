#!/bin/csh 

  if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm_and_postproc_on_all.csh scenario basin'
      echo ' '
      exit
  endif

  set year1 = 1991
  set year2 = 2000

 set scenario = $argv[1]
 set basin = $argv[2]

  source ../../config/seglists/$basin.riv
 
 foreach seg ($segments) 

   sbatch ../useful/run_etm_and_land_and_dat_simultaneously_oneseg.csh $scenario $seg $year1 $year2

 end
