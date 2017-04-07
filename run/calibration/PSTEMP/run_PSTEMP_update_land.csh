#!/bin/csh

#### optimizes for in-stream temperature using both land and river parameters
##### runs the land once, then optimizes river parameters for that land setting
##### repeat the cycle of land once, river optimized

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 2) then
    echo 'usage: run_PSTEMP_optimization.csh scenario calscen'
    exit
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

######## SET YEARS FOR CALIBRATION
  set year1 = 1985
  set year2 = 2005

  set limit = 0.1

# SET SCENARIO FOR OBSERVED DATA
  $tree/run/calibration/PSTEMP/sumWTMP.csh $scenario $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in sumWTMP '
       cat problem
       if (${#argv} == 3) then
         rm problem
       endif
       exit
     endif

  $tree/run/calibration/PSTEMP/compile_PSTEMP_opt.csh $calscen $tree
  echo $calscen, $scenario | $tree/code/bin/calib_iter_PSTEMP_params_${calscen}.exe

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

 
  cd ../
  rm -r temp$$
