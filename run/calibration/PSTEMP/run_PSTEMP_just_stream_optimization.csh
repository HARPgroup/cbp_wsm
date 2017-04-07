#!/bin/csh

#### optimizes for in-stream temperature using both land and river parameters
##### runs the land once, then optimizes river parameters for that land setting
##### repeat the cycle of land once, river optimized

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 3) then
    echo 'usage: run_PSTEMP_just_stream_optimization.csh scenario calscen basin'
    exit
  endif

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]

  source ../../fragments/set_tree

  mkdir -p ../../../tmp/scratch/temp$$
  cd ../../../tmp/scratch/temp$$

######## SET YEARS FOR CALIBRATION
  set year1 = 1985
  set year2 = 2005

  set limit = 0.1

# SET SCENARIO FOR OBSERVED DATA
  source $tree/config/control/calib/PSTEMP/$calscen/set_obscen

########### rename .riv and .land files in seglists to a calscen and module specific name
  cp -v $tree/config/seglists/${basin}.riv $tree/config/seglists/${calscen}_PSTEMP.riv
  cp -v $tree/config/seglists/${basin}.land $tree/config/seglists/${calscen}_PSTEMP.land
   
########### make .calib files in seglists
    if (-e problem) then
      rm problem
    endif

  echo $calscen $obscen PSTEMP $year1 $year2 | $tree/code/bin/make_calib_sites.exe 

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

####### compile the optmization code
  $tree/run/calibration/PSTEMP/compile_PSTEMP_river_opt.csh $calscen $tree

################# MAKE WEIGHT FILE FROM BASIN BY INSPECTING DATA
############### weight file not specifically needed, but it contains the list of sites
  $tree/run/calibration/PSTEMP/compile_PSTEMP_weights.csh $calscen $tree
  echo $calscen $scenario $obscen ${calscen}_PSTEMP $year1 $year2 $limit | $tree/code/bin/make_PSTEMP_weights_${calscen}.exe

    if (-e problem) then
      echo ' '
      cat problem
      exit
    endif

###### run the optimization
  $tree/run/calibration/PSTEMP/run_PSTEMP_stream_optimization.csh $scenario $calscen $tree
 
     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif
 
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

  $tree/run/calibration/PSTEMP/run_postproc_PSTEMP_daily.csh $scenario $calscen $basin $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in postproc '
       cat problem
       exit
     endif

  cd ../
  rm -r temp$$
