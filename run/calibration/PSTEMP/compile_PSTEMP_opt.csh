#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_PSTEMP_opt.csh calib_scenario '
      echo ' or     compile_PSTEMP_opt.csh calib_scenario tree'
      echo ' '
      exit
    endif
  endif

  set calscen = $argv[1]
  if (${#argv} == 2) then
    set tree = $argv[2]
  else
    source ../../fragments/set_tree
    mkdir -p ../../../tmp/scratch/temp$$/
    cd ../../../tmp/scratch/temp$$/
  endif

  f77 -c -o $tree/config/control/calib/PSTEMP/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/PSTEMP/$calscen/${calscen}_sensitivities.f

  cd $tree/code/src/calibration_utils/change_param/calib_iter/PSTEMP/
  compile
  cd ../../../../../

  f77 -o bin/calib_iter_PSTEMP_params_${calscen}.exe src/calibration_utils/change_param/calib_iter/PSTEMP/main.o ../config/control/calib/PSTEMP/$calscen/${calscen}_sensitivities.o src/calibration_utils/change_param/calib_iter/PSTEMP/caltemp.a src/lib/get_lib.a src/lib/util_lib.a


