#!/bin/csh
########################################################################
## this script makes subgrid transport factors to convert EOF to EOS  ##  
##  for nitrogen and phosphorous.                                     ##
##                                                                    ##
## This is based on assigning upstream sgtfs to make up for           ##
##   calibration bias                                                 ##
##                                                                    ##
##  The user specifies which data sets are used in the calculation    ##
##    in the ./run/control/calib/subgridtf/$calscen.con  file         ##
##    which references file in the ./pp/data/transport/calc_factors/  ##
##    directory.                                                      ##
##                                                                    ##
##  this always runs on the whole model domain because the chance of  ##
##    errors is too great when passing a segment list                 ##
########################################################################
#   GET SCENARIO 
  if (${#argv} != 3) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  run_subgrid_with_medians.csh cal_scenario river_scenario'
      echo 'usage:  run_subgrid_with_medians.csh cal_scenario river_scenario tree'
      echo ' '
      echo '   cal_scenario refers to a file in ./run/control/calib/subgridTF/'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  set rscen = $argv[2]
  if (${#argv} == 3) then
    set tree = $argv[3]
  else
    source ../../fragments/set_tree
  endif

  echo $calscen $rscen | $tree/code/bin/EOF_to_EOS_median.exe

  if (-e problem) then
    echo ' '
    echo ' PROBLEM EOF_to_EOS program'
    cat problem
    if (${#argv} == 1) then
      rm problem
    endif
    exit
  endif

