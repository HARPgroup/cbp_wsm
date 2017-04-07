#!/bin/csh

#  this script runs a calibration of the river parameters for water quality
# in the specified basin.  Calibration sensitivities are calculated at all 
# stations in the basin simultaneously for the best overall calibration rather
# than a strict nesting strategy

#   GET SCENARIO AND BASIN (SEGMENT LISTS)
  if (${#argv} != 4) then
    if (${#argv} != 5) then
      echo ' '
      echo ' use this script to get out of an optimization.'
      echo ' to use this script'
      echo '   kill a run while the river UCIs are running'
      echo '   create a temporary seglists with only the remaining segments in it'
      echo ' '
      echo 'usage:  graceful_exit.csh scenario calscen basin tempbasin'
      echo ' or     graceful_exit.csh scenario calscen basin tempbasin tree'
      echo ' '
      exit
    endif
  endif

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

  $tree/run/run_river.csh $scenario $tempbasin $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in river '
    cat problem
    exit
  endif

  $tree/run/calibration/WQ/run_postproc_WQ_cal.csh $scenario $calscen WQ $basin $year1 $year2 $tree

  if (-e problem) then
    echo ' '
    echo ' PROBLEM in postproc '
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
