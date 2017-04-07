#!/bin/csh

#  this script runs a calibration of the river parameters for water quality
# in the specified basin.  Calibration sensitivities are calculated at all 
# stations in the basin simultaneously for the best overall calibration rather
# than a strict nesting strategy

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo ' use this script to recover a crashed optimization'
      echo ' to use this script'
      echo '   create a temporary seglists with only the remaining segments in it'
      echo '   and edit the script to run only the remaining numbers'
      echo ' '
      echo 'usage:  recover_crash_WQ_stream_optimization.csh scenario calscen basin tempbasin'
      echo ' or     recover_crash_WQ_stream_optimization.csh scenario calscen basin tempbasin tree'
      echo ' '
      exit
    endif
  endif

  set nums = (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30)
  set firstnum = 22
  set nums = (23 24 25 26 27 28 29 30)

  set scenario = $argv[1]
  set calscen = $argv[2]
  set basin = $argv[3]
  set tempbasin = $argv[4]
  if (${#argv} == 5) then
    set tree = $argv[5]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif
#################### FINISHED WITH INPUT

######## SET YEARS FOR CALIBRATION
  set year1 = 1985
  set year2 = 2005

######## GET OBSERVED SCENARIO
  source $tree/config/control/calib/WQ/$calscen/set_obscen

  set num = $firstnum

  $tree/run/run_river.csh $scenario $tempbasin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    exit
  endif

  $tree/run/calibration/WQ/run_postproc_WQ_cal.csh $scenario $calscen WQ $basin $year1 $year2 $tree

  $tree/run/useful/read_dot_out.com $scenario $basin $year1 $year2 $tree

  if (!(-e $tree/output/river/summary/$scenario/sumout_${basin}.csv)) then
    echo 'Problem with read_dot_out'
    echo 'file' $tree/output/river/summary/$scenario/sumout_${basin}.csv 'not created'
    exit
  endif

  grep 'total' $tree/output/river/summary/$scenario/sumout_${basin}.csv >> $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv

  echo $calscen, $scenario, $basin, $num, $year1, $year2 | $tree/code/bin/calib_iter_WQ_river_${calscen}.exe

  if (-e $tree/run/calibration/WQ/${calscen}_${scenario}.converge) then
    $tree/run/calibration/WQ/run_postproc_daily.csh $scenario $basin $year1 $year2 $tree
    if (-e problem) then
      echo ' '
      echo ' PROBLEM in run_postproc_daily.csh '
      cat problem
      exit
    endif

    rm $tree/run/calibration/WQ/${calscen}_${scenario}.converge
    echo 'calibration complete, convergence criterion met'
    exit
  endif

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in calib_iter_WQ_river_'${calscen}'.exe'
    cat problem
    exit
  endif

  foreach num ($nums)

     $tree/run/calibration/WQ/run_WQ_river_iter.csh $scenario $calscen $basin $year1 $year2 $num $tree

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in run_WQ_river_iter.csh '
       cat problem
       exit
     endif

     $tree/run/useful/read_dot_out.com $scenario $basin $year1 $year2 $tree

     if (!(-e $tree/output/river/summary/$scenario/sumout_${basin}.csv)) then
       echo 'Problem with read_dot_out'
       echo 'file' $tree/output/river/summary/$scenario/sumout_${basin}.csv 'not created'
       exit
     endif

     grep 'total' $tree/output/river/summary/$scenario/sumout_${basin}.csv >> $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv

     echo $calscen, $scenario, $basin, $num, $year1, $year2 | $tree/code/bin/calib_iter_WQ_river_${calscen}.exe

     if (-e $tree/run/calibration/WQ/${calscen}_${scenario}.converge) then
       $tree/run/calibration/WQ/run_postproc_daily.csh $scenario $basin $year1 $year2 $tree
       if (-e problem) then
         echo ' '
         echo ' PROBLEM in run_postproc_daily.csh '
         cat problem
         exit
       endif

       rm $tree/run/calibration/WQ/${calscen}_${scenario}.converge
       echo 'calibration complete, convergence criterion met'
       exit
     endif

     if (-e problem) then
       echo ' '
       echo ' PROBLEM in calib_iter_WQ_river_'${calscen}'.exe'
       cat problem
       exit
     endif


  end

   set num = 31
   $tree/run/calibration/WQ/run_WQ_river_iter.csh $scenario $calscen $basin $year1 $year2 $num $tree

   if (-e problem) then
     echo ' '
     echo ' PROBLEM in run_WQ_river_iter.csh '
     cat problem
     exit
   endif

   $tree/run/calibration/WQ/run_postproc_daily.csh $scenario $basin $year1 $year2 $tree

   if (-e problem) then
     echo ' '
     echo ' PROBLEM in run_postproc_daily.csh '
     cat problem
     exit
   endif

   $tree/run/useful/read_dot_out.com $scenario $basin $year1 $year2 $tree

   if (-e problem) then
     echo ' '
     echo ' PROBLEM in read_dot_out.csh '
     cat problem
     exit
   endif

   grep 'total' $tree/output/river/summary/$scenario/sumout_${basin}.csv >> $tree/output/river/summary/$scenario/sumout_iter_${basin}.csv

  if (${#argv} == 4) then
    cd ../calibration/WQ/
    rm -r temp$$
  endif
