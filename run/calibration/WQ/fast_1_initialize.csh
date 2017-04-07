#!/bin/csh

#  this script runs a calibration of the river parameters for water quality
# in the specified basin.  Calibration sensitivities are calculated at all 
# stations in the basin simultaneously for the best overall calibration rather
# than a strict nesting strategy

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 5) then
    if (${#argv} != 6) then
      echo ' '
      echo 'usage:  run_WQ_stream_optimization.csh scenario calscen basin year1 year2'
      echo ' or     run_WQ_stream_optimization.csh scenario calscen basin year1 year2 tree'
      echo ' '
      exit
    endif
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set year1 = $argv[4]
  set year2 = $argv[5]
  if (${#argv} == 6) then
    set tree = $argv[6]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif
#################### FINISHED WITH INPUT


######## GET OBSERVED SCENARIO
  source $tree/config/control/calib/WQ/$calscen/set_obscen

################# MAKE WEIGHT FILE FROM BASIN BY INSPECTING DATA
    if (-e problem) then
      rm problem
    endif
  $tree/run/calibration/WQ/compile_WQ_weights.csh $calscen $tree
  echo $scenario $calscen WQ $obscen $basin $year1 $year2 | $tree/code/bin/make_WQ_weights_${calscen}.exe
    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

######## COMPILE OPTIMIZATION CODE
  $tree/run/calibration/WQ/compile_WQ_river_opt.csh $calscen $tree

  if (-e $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv) then
    rm $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv
  endif

######## SET DEFAULT PARAMETERS
     echo $calscen, $scenario, $basin | $tree/code/bin/set_defaults_WQ_river_${calscen}.exe

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in setting default parameters'
       cat problem
       exit
     endif

  if (${#argv} == 5) then
    cd ../
    rm -r temp$$
  endif
