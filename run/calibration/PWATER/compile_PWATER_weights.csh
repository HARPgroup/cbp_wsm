#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_WQ_weights.csh calib_scenario '
      echo ' or     compile_WQ_weights.csh calib_scenario tree'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]

  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
  endif

  set codedir = $tree/config/control/calib/PWATER/${calscen}/make_calib_weights/

  cd $codedir
  compile ${calscen}

