#!/bin/csh
#   GET SCENARIO, BASIN, and TREE

  if (${#argv} != 1) then
    if (${#argv} != 2) then
      echo ' '
      echo 'usage:  compile_PQUAL_opt.csh calib_scenario '
      echo ' or     compile_PQUAL_opt.csh calib_scenario tree'
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

  f77 -c -o $tree/config/control/calib/PQUAL/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_sensitivities.f
  f77 -c -o $tree/config/control/calib/PQUAL/$calscen/${calscen}_calibrated.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_calibrated.f
  f77 -c -o $tree/config/control/calib/PQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_initial.f

  f77 -o $tree/code/bin/calib_iter_PQUAL_params_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/main.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_sensitivities.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/calib_iter_PQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  f77 -o $tree/code/bin/calib_iter_PQUAL_init_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/main_assign_initial_params.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/calib_iter_PQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  f77 -o $tree/code/bin/calib_iter_PQUAL_check_${calscen}.exe $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/main_check_calib.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_initial.o $tree/config/control/calib/PQUAL/$calscen/${calscen}_calibrated.o $tree/code/src/calibration_utils/change_param/calib_iter/PQUAL/calib_iter_PQUAL.a $tree/code/src/lib/get_lib.a $tree/code/src/lib/util_lib.a

  if (${#argv} == 1) then
    cd ../
    rm -r temp$$
  endif 

