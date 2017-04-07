#!/bin/csh
#GET CALIBRATION SCENARIO AND SCENARIO
 
  if (${#argv} != 4) then
    if (${#argv} != 3) then
        echo ' '
        echo 'usage:  regress_river.csh calibration scenario basin'
        echo 'or  regress_river.csh calibration scenario basin tree'
        echo ' '
        exit
     endif
  endif
  
  set calib = $argv[1]
  set rscen = $argv[2]
  set basin = $argv[3]

  if (${#argv} == 4) then
    set tree = $argv[4]
  else
    source ../fragments/set_tree
    mkdir -p ../../tmp/scratch/temp$$/
    cd ../../tmp/scratch/temp$$/
  endif

####### RIVER SEGMENTS or WQ RECEIVING AREAS ONLY  ########
   source $tree/config/seglists/${basin}.riv

   foreach seg ($segments)
    if (-e problem) then
      rm problem
    endif

   mkdir -p $tree/output/river/regressions/$rscen/
   
   echo $calib,$rscen,$seg|$tree/code/bin/regress_river.exe

   end



