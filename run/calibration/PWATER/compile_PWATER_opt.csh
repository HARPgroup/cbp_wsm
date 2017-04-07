#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_PWATER_opt.csh calib_scenario '
      echo ' or     compile_PWATER_opt.csh calib_scenario tree'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$
    cd ../../../tmp/scratch/temp$$
  endif

  f77 -c -o $tree/config/control/calib/PWATER/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/PWATER/$calscen/${calscen}_sensitivities.f

  cd $tree/code/src/calibration_utils/change_param/calib_iter/PWATER/
  compile

#  cd back to ./code/
  cd ../../../../../   

  f77 -o bin/calib_iter_PWATER_params_${calscen}.exe src/calibration_utils/change_param/calib_iter/PWATER/main.o ../config/control/calib/PWATER/$calscen/${calscen}_sensitivities.o src/calibration_utils/change_param/calib_iter/PWATER/caliter.a src/lib/get_lib.a src/lib/util_lib.a

  if (${#argv} == 1) then
    rm -r tmp/scratch/temp$$
  endif
