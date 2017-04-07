#!/bin/csh

#### optimizes for in-stream temperature using both land and river parameters
##### runs the land once, then optimizes river parameters for that land setting
##### repeat the cycle of land once, river optimized

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  set scenario = p52temp
  set calscen = p52temp
  set basin = p52cal

  source ../../fragments/set_tree
  set nums = (5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)

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

####### compile the optmization code
  $tree/run/calibration/PSTEMP/compile_PSTEMP_opt.csh $calscen $tree
  $tree/run/calibration/PSTEMP/compile_PSTEMP_river_opt.csh $calscen $tree

  foreach num ($nums)
     echo 'land run number', $num 
     $tree/run/calibration/PSTEMP/run_PSTEMP_iter.csh $scenario $calscen ${calscen}_PSTEMP $tree

     if (-e problem) then
       echo 'problem in run number: ',$num
       cat problem
       rm problem
       exit
     endif
      
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

     echo $calscen, $scenario | $tree/code/bin/calib_iter_PSTEMP_params_${calscen}.exe

     if (-e problem) then
       echo 'problem in parameter change program, run number: ',$num
       cat problem
       rm problem
       exit
     endif

  end

  $tree/run/calibration/PSTEMP/run_PSTEMP_iter.csh $scenario $calscen ${calscen}_PSTEMP $tree

  if (-e problem) then
    echo 'problem in run number: ',$num
    cat problem
    rm problem
    exit
  endif

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
