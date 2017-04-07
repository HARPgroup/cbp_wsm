#!/bin/csh 

  if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_etm_on_all.csh scenario basin'
      echo ' '
      exit
  endif

 set scenario = $argv[1]
 set basin = $argv[2]

  source ../../config/seglists/$basin.riv
 
 foreach seg ($segments) 

   sbatch ../useful/run_transfer_oneseg.csh $scenario $seg

 end
